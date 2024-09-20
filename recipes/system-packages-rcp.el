;;; system-packages-rcp.el --- Configuration for system packages  -*- lexical-binding: t; -*-

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

;; Configuration related to installing system packages

;;; Code:

;;;; Exec-path-from-shell
;; Ensure Emacs' and system shell have same path
(use-package exec-path-from-shell
  :demand
  :custom
  ;; Set to nil, particularly removing the `-i' (interactive) flag to quicken
  ;; startup. However, my variables need to be accessible by non-interactive
  ;; shells, meaning the proper installation steps need to be followed. Thus,
  ;; variables should be set in `~/.profile', `~/.bash_profile', or `~/.zshenv'
  ;; instead of `~/.bashrc' or `~/.zshrc'. See here for more:
  ;; https://github.com/purcell/exec-path-from-shell
  (exec-path-from-shell-arguments nil)
  (exec-path-from-shell-variables
   '("PATH" "MANPATH" "BROWSER"
     ;; `ssh-agent' environment variables. See
     ;; https://wiki.archlinux.org/title/SSH_keys#Start_ssh-agent_with_systemd_user
     "SSH_AGENT_PID" "SSH_AUTH_SOCK"
     ;; For LSP-mode. See
     ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
     "LSP_USE_PLISTS")))
(when (fboundp 'elpaca-wait)
  (elpaca-wait))
(exec-path-from-shell-initialize)

;; Found here:
;; https://www.reddit.com/r/emacs/comments/s6zkb6/comment/ht794j7/?utm_source=share&utm_medium=web2x&context=3
(exec-path-from-shell-copy-env "GOPATH")
(when (eq (length (getenv "NODE_PATH")) 0) ; For npm
  (setenv "NODE_PATH" "/usr/local/lib/node_modules"))

;;;; System-packages
;; Install system packages within Emacs. Necessary for use-package's
;; `:ensure-system-package' flag
(use-package system-packages
  :custom
  (system-packages-noconfirm t))        ; Bypass its prompt
(when (fboundp 'elpaca-wait)
  (elpaca-wait))

(provide 'system-packages-rcp)
;;; system-packages-rcp.el ends here
