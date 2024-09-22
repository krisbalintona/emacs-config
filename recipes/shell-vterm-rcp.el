;;; shell-vterm-rcp.el --- Vterm                     -*- lexical-binding: t; -*-

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

;; Vterm packages and their configuration.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Vterm
;; Full-fledged terminal emulator
(use-package vterm
  :ensure-system-package (("/usr/share/licenses/libvterm/" . libvterm) ; Specifically for Fedora
                          (libtool)
                          (cmake))
  :gfhook
  '(lambda ()
     (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
     (buffer-face-mode t)
     (face-remap-add-relative 'default :height 1.1))
  :general
  (:keymaps 'vterm-mode-map
            :states 'insert
            "<tab>" 'vterm-send-tab)
  (kb/open-keys
    "v" '((lambda ()
            (interactive)
            (vterm (concat "*vterm* "
                           (file-name-nondirectory (directory-file-name (file-name-directory default-directory))))))
          :wk "Vterm"))
  :custom
  (vterm-kill-buffer-on-exit nil)
  (vterm-copy-exclude-prompt t)
  (vterm-timer-delay nil)      ; Make vterm appear less "slow" by removing delay
  :config
  (defun kb/kill-vterm-process-maybe (&optional frame)
    "If the current buffer has a vterm process running, kill both the
process and its buffer without confirmation."
    (let ((kill-buffer-query-functions  ; Suppress asking for confirmation
           (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
      (when (string= major-mode 'vterm-mode)
        (kill-buffer))))
  (add-to-list 'delete-frame-functions #'kb/kill-vterm-process-maybe))

;;;; EAT
(use-package eat
  :hook (eshell-first-time-mode . eat-eshell-mode)
  :config
  (eat-eshell-visual-command-mode 1))

(provide 'shell-vterm-rcp)
;;; shell-vterm-rcp.el ends here
