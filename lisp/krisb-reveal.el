;;; krisb-reveal.el --- Convenience for opening outline and org headings  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: convenience

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

;; Open the org or outline/outshine heading that contains point when needed.

;;; Code:
(require 'cl-macs)

;;; Options
(defgroup krisb-reveal nil
  "Reveal outline/outshine and org heading that contains point after command.
Inspired by `pulsar--post-command-pulse'."
  :group 'convenience
  :prefix "krisb-reveal-")

(defcustom krisb-reveal-fold-commands (list)
  "A plist that determines arguments to pass to `krisb-reveal-fold'.
This is a plist of elements.

The following properties are required:
- :command -- A symbol representing a command after which `krisb-reveal-fold'
  should be invoked
- :location -- A function (name or lambda expression) that returns a cons whose
  car is a point at which `krisb-reveal-fold' will be invoked and cdr is the
  buffer to invoke `krisb-reveal-fold' in.

The following properties are optional:
- :predicate -- A function (name or lambda expression) that, if returning nil,
  prevents `kris-reveal-fold' from being invoked after the command denoted by
  :command."
  :type '(plist :key-type (choice
                           (const :command)      ; Required
                           (const :location)     ; Required
                           (const :predicate))   ; Optional
                :value-type (choice
                             (function :tag "Command")           ; For :command
                             (function :tag "Location function") ; For :location
                             (function :tag "Predicate function")))) ; For :predicate)

;;; Functions
(declare-function org-show-entry "org")
(declare-function outline-show-entry "outline")

;;;###autoload
(defun krisb-reveal-fold (&optional point buffer predicate)
  "Open the outline/outshine or org heading at POINT in BUFFER.
This function is like `pulsar-reveal-entry' but not just when point is at heading.

This function assumes `krisb-reveal-mode' is non-nil.

PREDICATE is a function name or lambda expression that is evaluated in
the current buffer (not BUFFER) and prevents this function from doing
anything if it returns nil.

If POINT is nil, the current point in BUFFER will be assumed.  If BUFFER
is nil, the current buffer will be assumed.  If PREDICATE is nil, then
this function will operate normally."
  (when (or (not krisb-reveal-mode)
            (and predicate (not (funcall predicate))))
    (cl-return))
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (or point (point)))
    (cond
     ((eq major-mode 'org-mode)
      (org-show-entry))
     ((or (eq major-mode 'outline-mode)
          (bound-and-true-p outline-minor-mode))
      (outline-show-entry)))))

;; TODO 2024-10-24: Consider resolving aliases like
;; `pulsar--resolve-function-aliases'.
(defun krisb-reveal-fold--post-command ()
  "Maybe reveal fold at point.
A \"fold\" is an outline, outshine, or org heading.  Folds are revealed
if the `this-command' is one of the ones listed in
`krisb-reveal-fold-commands'.  Revealing is achieved by
`krisb-reveal-fold'.

This function is meant to be added to `post-command-hook'.

This function was inspired by `pulsar--post-command-pulse'."
  (when (eq this-command (plist-get krisb-reveal-fold-commands 'command))
    (let* ((location (funcall (plist-get krisb-reveal-fold-commands 'location)))
           (point (car location))
           (buffer (cdr location))
           (predicate (plist-get krisb-reveal-fold-commands 'predicate)))
      (krisb-reveal-fold point buffer predicate))))

;;; Minor mode
(define-minor-mode krisb-reveal-mode
  "Set up krisb-reveal for each command in `krisb-reveal-fold-commands'.
This is a buffer-local mode.  Also check `krisb-reveal-global-mode'."
  :global nil
  (if krisb-reveal-mode
      (progn
        (add-hook 'post-command-hook #'krisb-reveal-fold--post-command nil 'local))
    (remove-hook 'post-command-hook #'krisb-reveal-fold--post-command 'local)))

(defun krisb-reveal--on ()
  "Enable `krisb-reveal-mode'."
  (unless (minibufferp)
    (let (inhibit-quit)
      (krisb-reveal-mode 1))))

;;;###autoload
(define-globalized-minor-mode krisb-reveal-global-mode krisb-reveal-mode krisb-reveal--on)

;;; Provide
(provide 'krisb-reveal)
;;; krisb-reveal.el ends here
