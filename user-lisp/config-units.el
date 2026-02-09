;;; config-units.el --- Tab bookmarks -*- lexical-binding: t -*-

;; Copyright (C) 2026 Kristoffer Balintona

;; Author: Kristoffer Balintona
;; Created: 2026

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a WIP package for a bespoke configuration-declaration DSL.

;; TODO 2026-02-09: Warn/user-error for setting non-existing config
;; unit property.

;; TODO 2026-02-09: Consider have a user-option for handling packages
;; that are listed in config-package-specifications but are not
;; required by any config unit: either warn or install automatically,
;; with a message.

;; TODO 2026-02-09: Better error messages and handling.

;;; Code:

(require 'thunk)

;; Pacify byte-compiler
(defvar package-pinned-packages)

(defvar config-units nil
  "Alist of (name . list-of-thunk-specs).
Each thunk specification is a plist with :deps, :packages, :thunk.")

;; TODO 2026-02-09: Turn into defcustom.
(defvar config-package-specifications nil
  "List of package installation specifications.
Each package specification is a list whose car is a symbol for the name
of the package.  This symbol should be referenced by the :packages
property in `defconfig'.

The cdr of this list should be a plist.  Required properties in this
plist are:
- :manager - The package manager in use.  Available options: `package'
  and `package-vc'.
- :spec - A Lisp form passed to the package installation function(s)
  associated with the package manager specified by :manager.
Optional properties are:
- :pin - A form added to `package-pinned-packages'.  Only has an effect
  when :manager is `package'.

The value of this variable is used in `config--install-packages' to
install packages specified in `defconfig' forms.")

(defvar config-units-forced nil
  "List of config unit names that have been fully forced.")

(defvar config-thunks-forced nil
  "List of (name . index) pairs for thunks that have been forced.")

(defmacro defconfig (name-and-opts &rest body)
  "Define a configuration unit.
NAME-AND-OPTS specifies a configuration unit name and a set of \"thunk\"
properties.  A \"thunk\" is a set of configuration unit properties
alongside BODY that will be evaluated at some point, depending on the
thunk's properties.  NAME-AND-OPTS has the form:

    (name [:deps (...) [:packages (...) [...]]])

NAME is the name of the associated configuration unit of the specified
thunk.  There are several available thunk properties:
- :deps - A list of configuration unit names this thunk depends on.
- :packages - A list of packages to install, if they aren\\='t already.

BODY will be evaluated after all the thunks in the configuration units
specified in :deps are forced.


Each configuration unit may have multiple thunks.  That is, one may
specify (defconfig FOO ...)  multiple times, each with their own thunk
properties.  In these cases, each thunk is evaluated in the order of
their definition and after all configuration unit dependencies are
resolved."
  (declare (indent defun) (debug (sexp def-body)))
  (let* ((name (car name-and-opts))
         (opts (cdr name-and-opts))
         (deps (plist-get opts :deps))
         (packages (plist-get opts :packages)))
    `(let ((existing-unit (assq ',name config-units))
           (new-thunk (list :deps ',deps
                            :packages ',packages
                            :thunk (thunk-delay ,@body))))
       ;; Create new config unit or append thunk to existing config
       ;; unit's list of thunks
       (if existing-unit
           (setcdr existing-unit
                   (append (cdr existing-unit)
                           (list new-thunk)))
         (setq config-units (append config-units
                                    (list (cons ',name (list new-thunk)))))))))

(defun config--thunk-deps-satisfied-p (deps)
  "Check if all DEPS are satisfied.
DEPS is a list of configuration unit names."
  (seq-every-p (lambda (dep) (memq dep config-units-forced)) deps))

(defun config--install-packages (packages)
  "Ensure all PACKAGES are installed."
  (when packages
    (dolist (pkg packages)
      (let* ((props (cdr (assoc pkg config-package-specifications)))
             (manager (plist-get props :manager))
             (spec (plist-get props :spec))
             (pin (plist-get props :pin)))
        (pcase manager
          ('package
           (add-to-list 'package-pinned-packages pin)
           (unless (package-installed-p spec)
             (package-install spec)))
          ('package-vc
           (unless (package-installed-p spec)
             (package-vc-install spec)))
          ((pred null)
           (error "Keyword :manager is required for package %s in `config-package-specifications'"
                  pkg))
          (_
           (error "Value of \"%s\" for :manager not supported" manager)))))))

(defun config--try-force-thunk (name index thunk-spec)
  "Try to force a specific thunk if its deps are satisfied.
THUNK-SPEC is a plist of configuration unit properties.  See `defconfig'
for a description of the available configuration unit properties.

NAME is the name of the configuration unit associated with THUNK-SPEC,
and thunk\\='s associated INDEX number.

Return t if successfully forced thunk, and nil otherwise."
  (let ((thunk-id (cons name index)))
    (or (member thunk-id config-thunks-forced) ; Already forced
        (let ((deps (plist-get thunk-spec :deps))
              (packages (plist-get thunk-spec :packages))
              (thunk (plist-get thunk-spec :thunk)))
          (when (config--thunk-deps-satisfied-p deps)
            (config--install-packages packages)
            (thunk-force thunk)
            (push thunk-id config-thunks-forced)
            t)))))

(defun config--unit-fully-forced-p (name)
  "Check if all thunks for config unit NAME have been forced."
  (or (memq name config-units-forced)
      (let* ((unit (assq name config-units))
             (thunks (cdr unit)))
        (seq-every-p (lambda (index)
                       (member (cons name index) config-thunks-forced))
                     (number-sequence 0 (1- (length thunks)))))))

(defun load-all-configs ()
  "Load all config thunks in dependency order."
  (let ((remaining-thunks nil))
    ;; Index all thunks with the name of their associated config unit
    (dolist (unit config-units)
      (let ((name (car unit))
            (thunk-specs (cdr unit)))
        (cl-loop for spec in thunk-specs
                 for index from 0
                 do (push (list name index spec) remaining-thunks))))
    (setq remaining-thunks (reverse remaining-thunks))
    
    ;; Process thunks in definition order, forcing when deps are met
    (while remaining-thunks
      (let ((made-progress nil))
        (dolist (thunk-specs remaining-thunks)
          (let ((name (nth 0 thunk-specs))
                (index (nth 1 thunk-specs))
                (spec (nth 2 thunk-specs)))
            (when (config--try-force-thunk name index spec)
              (setq remaining-thunks (delq thunk-specs remaining-thunks)
                    made-progress t)
              ;; Mark config unit as fully forced if all its thunks
              ;; are done
              (when (config--unit-fully-forced-p name)
                (push name config-units-forced)))))
        (unless made-progress
          (error "Circular dependency or missing config unit.  Remaining thunks:%s"
                 (mapconcat (lambda (thunk) (format "\n\n%s" thunk))
                            remaining-thunks)))))))



;;; NAIVE TESTING

;; (setq config-units nil
;;       config-units-forced nil
;;       config-thunks-forced nil)
;; 
;; (defconfig (foo :deps (bar))
;;   (message "foo 2"))
;; 
;; (defconfig (bar)
;;   (message "bar 1"))
;; 
;; (defconfig (foo)
;;   (message "foo 1"))
;; 
;; (defconfig (foo :deps (baz))
;;   (message "foo 3"))
;; 
;; (load-all-configs)

;;; Provide
(provide 'config-units)
;;; config-units.el ends here
