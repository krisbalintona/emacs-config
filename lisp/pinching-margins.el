;;; pinching-margins.el ---Center buffer contents    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: extensions

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

;; Like Centered-window and Perfect-margin. Expands left and right margins to
;; center buffer content visually.

;;; Code:
(require 'cl-macs)

;;; Options
(defgroup pinching-margins ()
  "Visually center buffer content."
  :group 'emacs
  :prefix "pinching-margins-")

(defcustom pinching-margins-visible-width 128
  "The number of columns each window should be pinched to."
  :type 'number)

(defcustom pinching-margins-ignore-predicates
  (list #'window-minibuffer-p
        (lambda (win)
          (with-selected-window win (bound-and-true-p olivetti-mode))))
  "Predicates to exclude certain windows."
  :type '(repeat function))

(defcustom pinching-margins-force-predicates nil
  "Predicates to force including certain window."
  :type '(repeat function))

;;; Functions
(defun pinching-margins--calculate-margins (win)
  "Return the margin widths for window WIN.
The margin widths are returned as a cons cell whose car is the left
margin's width and cdr is the right margin's width."
  (let ((width (round (max 0 (/ (- (window-total-width win) pinching-margins-visible-width) 2)))))
    (cons width width)))

(defun pinching-margins--apply-p (win)
  "Determine whether the margins for window WIN should be applied.
Returns t if so, and nil if not.  Returned value is based on
`pinching-margins-force-predicates' and
`pinching-margins-ignore-predicates'."
  (with-current-buffer (window-buffer win)
    (or (run-hook-with-args-until-success 'pinching-margins-force-predicates win)
        (not (run-hook-with-args-until-success 'pinching-margins-ignore-predicates win)))))

(defun pinching-margins--set-win-margin (win)
  "Pinches margins of WIN if applicable.
Application proceeds if `pinching-margins--apply-p' returns non-nil."
  (with-selected-window win
    (when (pinching-margins--apply-p win)
      (let ((margins (pinching-margins--calculate-margins win)))
        (set-window-margins win (car margins) (cdr margins))))))

(defun pinching-margins--set-margins (&optional win)
  "Sets the margins for window WIN.
Margin widths are determined by `pinching-margins--set-win-margin'."
  (cl-loop for win in (or win (window-list))
           do (pinching-margins--set-win-margin win)))

(defun pinching-margins--window-splittable-p-advice (orig-fun window &optional horizontal)
  "Advice for `window-splittable-p' to temporarily remove margins when called.
If WINDOW is not managed by pinched-margins or HORIZONTAL is nil, the
function will not modify the margins and directly call ORIG-FUN."
  (if (or (not horizontal)
          (not (pinching-margins--apply-p window)))
      (funcall orig-fun window horizontal)
    (let ((margins (window-margins window)))
      (prog2
          (set-window-margins window 0 0)
          (funcall orig-fun window horizontal)
        (set-window-margins window (car margins) (cdr margins))))))

;;; Minor mode
;;;###autoload
(define-minor-mode pinching-margins-mode
  "Auto center windows."
  :init-value nil
  :global t
  (if pinching-margins-mode
      ;; Add hook and activate
      (progn
        (advice-add 'window-splittable-p :around #'pinching-margins--window-splittable-p-advice)
        (add-hook 'window-configuration-change-hook #'pinching-margins--set-margins)
        (add-hook 'window-size-change-functions #'pinching-margins--set-margins)
        (pinching-margins--set-margins))
    ;; Remove hook and restore margin
    (advice-remove 'window-splittable-p #'pinching-margins--window-splittable-p-advice)
    (remove-hook 'window-configuration-change-hook #'pinching-margins--set-margins)
    (remove-hook 'window-size-change-functions #'pinching-margins--set-margins)
    ;; FIXME 2024-09-19: This only restores the currently visible windows. E.g.
    ;; `tab-bar' windows that are elsewhere aren't affected.
    (dolist (window (window-list))
      (when (pinching-margins--apply-p window)
        (set-window-margins window 0 0)))))

;;; Provide
(provide 'pinching-margins)
;;; pinching-margins.el ends here
