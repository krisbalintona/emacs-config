;;; programming-c-rcp.el --- C                       -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

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

;; Packages related to developing in C.

;;; Code:
(require 'keybinds-general-rcp)

;;;; C-mode
(use-package cc-mode
  :ensure nil
  :bind
  ( :map c-mode-map
    ("TAB" . indent-for-tab-command)))

;;;; Gdb-mi
;; Built-in GDB
(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  (gud-gdb-command-name "gdb -i=mi --quiet")
  (gdb-restore-window-configuration-after-quit 'if-gdb-many-windows)
  :config
  (defun kb/gdb-non-stop-handler ()
    "Version of the original that avoids the GDB startup error
regarding \"target-async\"."
    (goto-char (point-min))
    (if (re-search-forward "No symbol" nil t)
        (progn
          (message
           "This version of GDB doesn't support non-stop mode.  Turning it off.")
          (setq gdb-non-stop nil)
          (setq gdb-supports-non-stop nil))
      (setq gdb-supports-non-stop t)
      ;; (gdb-input "-gdb-set target-async 1" 'ignore)
      (gdb-input "-gdb-set mi-async 1" 'ignore) ; Change to this, as advised
      (gdb-input "-list-target-features" 'gdb-check-target-async)))
  (advice-add 'gdb-non-stop-handler :override #'kb/gdb-non-stop-handler))

;;;; Gdb-bp-session
(use-package gdb-bp-session
  ;; :ensure (:type git :host github :repo "emacsmirror/gdb-bp-session")
  :vc (:url "https://github.com/emacsmirror/gdb-bp-session.git")
  :requires no-littering gud
  :hook (gdb-inferior-io-mode . kb/gdb-bp-session-ask-restore-breakpoints)
  :config
  (defvar kb/gdb-bp-session--save-dir
    (no-littering-expand-var-file-name "gdb-bp-session/"))

  (defun kb/gdb-bp-session--save-path-file ()
    (let* ((gud-process-name (file-name-sans-extension (gud-get-process-name)))
           (save-file-name (concat gud-process-name "-breakpoints.gdb"))
           (save-file-dir kb/gdb-bp-session--save-dir)
           (save-file-path (expand-file-name save-file-name save-file-dir)))
      (unless (file-exists-p save-file-dir)
        (make-directory save-file-dir))
      (if gud-process-name
          save-file-path
        (user-error "Not in GDB!"))))

  (defun kb/gdb-save-breakpoints ()
    "Save current breakpoint definitions as a script."
    (interactive)
    (let* ((save-file-path (kb/gdb-bp-session--save-path-file)))
      (gud-basic-call (concat "save breakpoints " save-file-path))))
  (advice-add 'gdb-save-breakpoints :override #'kb/gdb-save-breakpoints)

  (defun kb/gdb-restore-breakpoints ()
    "Restore the saved breakpoint definitions as a script."
    (interactive)
    (let* ((breakpoints-file (kb/gdb-bp-session--save-path-file)))
      (if (file-exists-p breakpoints-file)
          (gud-basic-call (format "source %s" breakpoints-file))
        (user-error "No saved breakpoints file!"))))
  (advice-add 'gdb-restore-breakpoints :override #'kb/gdb-restore-breakpoints)

  (defun kb/gdb-bp-session-ask-restore-breakpoints ()
    (when (y-or-n-p "Restore breakpoints?")
      (kb/gdb-restore-breakpoints)))

  (defun kb/gdb-delchar-or-quit (arg)
    "Delete ARG characters or send a quit command to GDB.
Send a quit only if point is at the end of the buffer, there is
no input, and GDB is waiting for input."
    (interactive "p")
    (unless (and (eq (current-buffer) gud-comint-buffer)
                 (eq gud-minor-mode 'gdbmi))
      (error "Not in a GDB-MI buffer"))
    (let ((proc (get-buffer-process gud-comint-buffer)))
      (if (and (eobp)
               (process-live-p proc)
               (not gud-running)
               (= (point) (marker-position (process-mark proc))))
          ;; Sending an EOF does not work with GDB-MI; submit an
          ;; explicit quit command.
          (progn
            (if (> gdb-control-level 0)
                (process-send-eof proc)
              (kb/gdb-save-breakpoints)
              (insert "quit")
              (comint-send-input t t)))
        (delete-char arg))))
  (advice-add 'gdb-delchar-or-quit :override #'kb/gdb-delchar-or-quit))

(provide 'programming-c-rcp)
;;; programming-c-rcp.el ends here
