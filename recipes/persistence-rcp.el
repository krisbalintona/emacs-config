;;; persistence-rcp.el --- Persistence across Emacs sessions  -*- lexical-binding: t; -*-

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

;; Packages relevant to saving and loading information across Emacs sessions.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Savehist
;; Make history of certain things (e.g. minibuffer) persistent across sessions
(use-package savehist
  :ensure nil
  :demand
  :custom
  (history-length 10000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 30)
  :config
  (dolist (var '(kill-ring
                 Info-history-list
                 last-kbd-macro
                 kmacro-ring
                 shell-command-history))
    (add-to-list 'savehist-additional-variables var))
  (savehist-mode 1)

  ;; REVIEW 2024-01-16: Not sure what this does...
  (unless (daemonp)
    (load savehist-file nil (not (called-interactively-p 'interactive)))))

;;;; Recentf
;; Enable logging of recent files
(use-package recentf
  :ensure nil
  :hook
  (on-first-input . recentf-mode)
  :bind
  ( :map krisb-file-keymap
    ("r" . recentf-open-files))
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  :config
  (recentf-mode 1))

;;;; Saveplace
;; Save and restore the point's location in files
(use-package saveplace
  :ensure nil
  :hook (on-first-file . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t)
  (save-place-limit 4000))

;;;; Desktop
;; Save buffers across Emacs sessions
(use-package desktop
  :ensure nil
  :demand t
  :custom
  (desktop-load-locked-desktop 'check-pid)
  (desktop-save 'ask-if-exists)
  (desktop-auto-save-timeout 3)
  (desktop-files-not-to-save
   (rx (or (regexp "\\(\\`/[^/:]*:\\|(ftp)\\'\\)")
           ;; Don't save files from other Emacs repos because sometimes they
           ;; have local variables that mess with desktop's loading of files
           (literal (expand-file-name "emacs-repos/" "~"))
           ;; Don't want to open my large org-agenda files which I'll open
           ;; eventually anyway
           (literal krisb-org-agenda-directory))))
  (desktop-globals-to-save '(desktop-missing-file-warning
                             tags-file-name
                             tags-table-list
                             search-ring
                             regexp-search-ring
                             ;; REVIEW 2024-10-13: The previews in
                             ;; `jump-to-register' cause errors when trying to
                             ;; visit a buffer or window which no longer exists.
                             ;; Removing it from the saved globals list is the
                             ;; workaround I choose for now.
                             ;; register-alist
                             file-name-history))
  (desktop-locals-to-save '(desktop-locals-to-save
                            truncate-lines
                            case-fold-search
                            case-replace
                            fill-column

                            overwrite-mode
                            change-log-default-name
                            line-number-mode

                            column-number-mode
                            size-indication-mode

                            buffer-file-coding-system
                            buffer-display-time

                            indent-tabs-mode
                            tab-width
                            indicate-buffer-boundaries

                            indicate-empty-lines
                            show-trailing-whitespace))

  (desktop-restore-eager 5)
  (desktop-restore-forces-onscreen nil)
  (desktop-restore-frames t)
  (desktop-restore-in-current-display nil)
  :config
  (desktop-save-mode 1))

(provide 'persistence-rcp)
;;; persistence-rcp.el ends here
