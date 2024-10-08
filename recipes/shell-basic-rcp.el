;;; shell-basic-rcp.el --- Shell-mode                -*- lexical-binding: t; -*-

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

;; Configuration and packages related to the basic `shell-mode'

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)


;;;; Comint
(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 9999)
  (comint-completion-autolist t)
  :config
  (setq-default comint-scroll-to-bottom-on-input 'all)
  (setq-default comint-scroll-to-bottom-on-output 'all)
  (setq-default comint-input-autoexpand 'input))

;;;; Shell
;; Built-in shell
(use-package shell
  :ensure nil
  :custom
  (async-shell-command-buffer 'confirm-kill-process) ; Don't ask, just do
  (ansi-color-for-comint-mode t)
  (shell-command-prompt-show-cwd t)     ; Emacs 27.1
  (shell-input-autoexpand 'input)
  (shell-highlight-undef-enable t)                   ; Emacs 29.1
  (shell-has-auto-cd nil)                            ; Emacs 29.1
  (shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
  (shell-kill-buffer-on-exit t))                     ; Emacs 29.1

;;;; Bash-completion
(use-package bash-completion)

(provide 'shell-basic-rcp)
;;; shell-basic-rcp.el ends here
