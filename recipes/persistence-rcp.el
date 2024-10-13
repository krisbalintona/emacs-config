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
  ( :map kb/file-keys
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
  :hook (after-init . desktop-save-mode)
  :custom
  (desktop-load-locked-desktop 'check-pid)
  (desktop-save 'if-exists)
  (desktop-auto-save-timeout 3)
  (desktop-files-not-to-save
   (rx (or (regexp "\\(\\`/[^/:]*:\\|(ftp)\\'\\)")
           ;; Don't save files from other Emacs repos because sometimes they
           ;; have local variables that mess with desktop's loading of files
           (literal (expand-file-name "emacs-repos/" "~")))))
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

  (desktop-restore-forces-onscreen nil)
  (desktop-restore-frames t)
  (desktop-restore-in-current-display nil))

;;;; Super-save
;; Automatically save buffers when you do certain things
(use-package super-save
  :disabled                             ; Opting for built in auto-save
  :demand
  :custom
  (super-save-auto-save-when-idle t) ; Save buffer if Emacs is idle
  (super-save-idle-duration 10) ; Wait 10 seconds for idle trigger
  (super-save-remote-files t) ; Turn on saving of remote files (those pulled from git repo?)
  (super-save-exclude nil) ; Don't exclude anything from being saved
  (super-save-predicates
   '((lambda ()
       (stringp (buffer-file-name (buffer-base-buffer))))
     (lambda ()
       (buffer-modified-p (current-buffer)))
     (lambda ()
       (file-writable-p (buffer-file-name (buffer-base-buffer))))
     (lambda ()
       (if (file-remote-p (buffer-file-name (buffer-base-buffer)))
           super-save-remote-files t))
     (lambda ()
       (super-save-include-p (buffer-file-name (buffer-base-buffer))))
     (lambda ()                              ; Don't save Email drafts
       (not (or (derived-mode-p 'message-mode)
                (eq major-mode 'org-msg-edit-mode))))))
  :config
  (add-to-list 'super-save-hook-triggers 'eyebrowse-pre-window-switch-hook)
  (add-to-list 'super-save-triggers 'evil-window-mru)
  ;; Make sure this goes after adding hooks, since the hooks are manually added once `super-save-mode' is enable
  (super-save-mode))

;;;; Built-in auto save and backup
;; Make recovery files
(use-package files
  :ensure nil
  :hook (on-first-file . auto-save-visited-mode)
  :custom
  ;; NOTE 2023-01-09: `auto-save-list-file-prefix' and `backup-directory-alist'
  ;; are set by `no-littering'

  ;; Auto save
  (auto-save-default t) ; Only a local minor mode exists; this variable influences the global value
  (auto-save-interval 150)
  (auto-save-timeout 8)
  (auto-save-include-big-deletions t)
  (delete-auto-save-files nil)
  (kill-buffer-delete-auto-save-files nil)
  (auto-save-no-message t)

  ;; Save buffer after idle time
  (remote-file-name-inhibit-auto-save-visited nil)
  (auto-save-visited-interval 15)
  (auto-save-visited-predicate
   ;; Inspired by `super-save'
   (lambda ()
     (or (derived-mode-p 'pdf-view-mode)
         (< (save-restriction (widen) (count-lines (point-min) (point-max)))
            5000))))

  ;; Backups
  (make-backup-files t)
  (backup-by-copying t)                 ; Don't clobber symlinks
  (version-control t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (delete-old-versions t)
  :config
  ;; HACK 2024-03-03: Prevent auto-save from complaining about long file names
  ;; by hashing them. Copied from Doom Emacs' doom-editor.el. Last updated:
  ;; 3bcee249d3e814ff15ddd59add491625d308352e
  (defun kb/auto-save-hash-file-name (&rest args)
    "Turn `buffer-file-name' into a hash.
Then apply ARGS."
    (let ((buffer-file-name
           (if (or
                ;; Don't do anything for non-file-visiting buffers. Names
                ;; generated for those are short enough already.
                (null buffer-file-name)
                ;; If an alternate handler exists for this path, bow out. Most
                ;; of them end up calling `make-auto-save-file-name' again
                ;; anyway, so we still achieve this advice's ultimate goal.
                (find-file-name-handler buffer-file-name
                                        'make-auto-save-file-name))
               buffer-file-name
             (sha1 buffer-file-name))))
      (apply args)))
  (advice-add 'make-auto-save-file-name :around #'kb/auto-save-hash-file-name)

  ;; HACK 2024-03-03: Do the same as `kb/auto-save-hash-file-name' but for
  ;; backup files because some packages that use `make-backup-file-name-1'
  ;; directly (like undo-tree).
  (defun kb/backup-hash-file-name (fn file)
    "A few places use the backup file name so paths don't get too long."
    (let ((alist backup-directory-alist)
          backup-directory)
      (while alist
        (let ((elt (car alist)))
          (if (string-match (car elt) file)
              (setq backup-directory (cdr elt)
                    alist nil)
            (setq alist (cdr alist)))))
      (let ((file (funcall fn file)))
        (if (or (null backup-directory)
                (not (file-name-absolute-p backup-directory)))
            file
          (expand-file-name (sha1 (file-name-nondirectory file))
                            (file-name-directory file))))))
  (advice-add 'make-backup-file-name-1 :around #'kb/backup-hash-file-name))

(provide 'persistence-rcp)
;;; persistence-rcp.el ends here
