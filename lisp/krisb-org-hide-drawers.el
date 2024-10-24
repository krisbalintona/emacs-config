;;; krisb-org-hide-drawers.el --- Hide drawers in Org using overlays  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: tools, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Hide drawers in Org using overlays.  My improved version of org-tidy.

;;; Code:
(require 'org)
(require 'org-element)

;;; Options
(defgroup krisb-org-hide-drawers ()
  "Hide Org drawers using overlays."
  :group 'org-mode
  :prefix "krisb-org-hide-drawers-")

(defcustom krisb-org-hide-drawers-string (propertize " #" 'face 'shadow)
  "Display string used for overlays."
  :type 'string)

(defcustom krisb-org-hide-drawers-blacklist (list)
  "A list of properties that prevent hiding drawer.
If any property in this option is present in a drawer, it will not be
hidden."
  :type '(repeat string))

;;; Variables
(defvar-local krisb-org-hide-drawers-overlays nil
  "A list of overlays used to hide Org drawers in the current buffer.")

;;; Functions
(defun krisb-org-drawer-properties (drawer)
  "Extract all properties from the given Org DRAWER element."
  (let ((contents (org-element-contents drawer))
        properties)
    (dolist (element contents)
      (when (and (eq (org-element-type element) 'node-property)
                 (org-element-property :key element))
        (push (cons (org-element-property :key element)
                    (org-element-property :value element))
              properties)))
    (nreverse properties)))             ; Return the list in original order

(defun krisb-org-hide-drawers--should-hide (drawer)
  "Predicate for whether DRAWER should be hidden.
DRAWER is an org-element.

Considers `krisb-org-hide-drawers-blacklist'."
  (let* ((properties (krisb-org-drawer-properties drawer))
         (property-keys (mapcar #'car properties)))
    (not (cl-some (lambda (blacklist-prop)
                    (member blacklist-prop property-keys)) ; Check against properties in the blacklist
                  krisb-org-hide-drawers-blacklist))))

;; TODO 2024-10-23: Consider special behavior for top-level drawers.  See
;; `org-tidy-should-tidy'.
(defun krisb-org-hide-drawers-create-overlays ()
  "Create overlays to hide Org drawers in the current buffer using the Org AST."
  (interactive)
  (let ((ast (org-element-parse-buffer 'element nil)))
    (org-element-map ast '(drawer property-drawer)
      (lambda (drawer)
        (when (krisb-org-hide-drawers--should-hide drawer)
          (let* ((begin (org-element-property :begin drawer))
                 (end (save-excursion
                        (goto-char (org-element-property :end drawer))
                        (skip-chars-backward "\n\t ") ; Skip trailing whitespace
                        (point)))
                 (ov (make-overlay (1- begin) ; Include preceding newline in overlay
                                   end))) ; Don't include proceeding whitespace in overlay

            ;; TODO 2024-10-23: Consider using the `insert-in-front-hooks'
            ;; special text property to notify the user of danger when adding
            ;; characters in front of hidden property drawer, since un-hiding it
            ;; then means that drawer is no longer recognized as such.

            (overlay-put ov 'display krisb-org-hide-drawers-string)
            (overlay-put ov 'modification-hooks
                         '((lambda (overlay after beg end)
                             (setq krisb-org-hide-drawers-overlays
                                   (remove overlay krisb-org-hide-drawers-overlays))
                             (delete-overlay overlay))))
            (overlay-put ov 'read-only t)
            (push ov krisb-org-hide-drawers-overlays)))))))

(defun krisb-org-hide-drawers-delete-overlays ()
  "Delete all drawer-hiding overlays in the current buffer."
  (interactive)
  (mapc #'delete-overlay krisb-org-hide-drawers-overlays)
  (setq krisb-org-hide-drawers-overlays nil))

(defun krisb-org-hide-drawers-toggle ()
  "Toggle visibility of Org drawers in the current buffer."
  (interactive)
  (if krisb-org-hide-drawers-overlays
      (krisb-org-hide-drawers-delete-overlays)
    (krisb-org-hide-drawers-create-overlays)))

;;; Minor mode
;;;###autoload
(define-minor-mode krisb-org-hide-drawers-mode
  "Minor mode to hide Org drawers with a # symbol."
  :lighter " HideDrawers"
  (if krisb-org-hide-drawers-mode
      (progn
        (krisb-org-hide-drawers-create-overlays)
        (add-hook 'after-save-hook #'krisb-org-hide-drawers-create-overlays nil t))
    (krisb-org-hide-drawers-delete-overlays)
    (remove-hook 'after-save-hook #'krisb-org-hide-drawers-create-overlays t)))

;;; Provide
(provide 'krisb-org-hide-drawers)
;;; krisb-org-hide-drawers.el ends here
