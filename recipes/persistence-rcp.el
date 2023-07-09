;;; persistence-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages relevant to saving and loading information across Emacs sessions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Savehist
;; Make history of certain things (e.g. minibuffer) persistent across sessions
(use-package savehist
  :demand
  :custom
  (history-length 10000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'Info-history-list)

  (when (daemonp)
    (setq savehist-autosave-interval 30)
    (add-hook 'kill-emacs-hook #'savehist-save))
  (savehist-mode))

;;; Recentf
;; Enable logging of recent files
(use-package recentf
  :hook (kill-emacs . recentf-save-list)
  :general (kb/file-keys
             "r" '(recentf-open-files :wk "Recentf open file"))
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  :init
  (recentf-mode))

;;; Saveplace
;; Save and restore the point's location in files
(use-package saveplace
  :demand t
  :custom
  (save-place-forget-unreadable-files t)
  :config (save-place-mode))

;;; Desktop
;; Save buffers across Emacs sessions
(use-package desktop
  :disabled t
  :straight nil
  :hook ((window-setup . desktop-save-mode)
         (desktop-save-mode . desktop-read))
  :custom
  (desktop-dirname (no-littering-expand-var-file-name "desktop/"))
  (desktop-base-file-name "emacs.desktop")
  (desktop-path (list desktop-dirname))
  (desktop-auto-save)
  (desktop-save 'ask-if-new)
  (desktop-files-not-to-save "^$")      ; Reload tramp paths
  (desktop-load-locked-desktop 'ask)
  (desktop-auto-save-timeout 20)
  (desktop-buffers-not-to-save
   (concat "\\("
           "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
           "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
           "\\)$"))

  ;; Lazy loading
  (desktop-lazy-idle-delay 5)
  (desktop-restore-eager nil)
  (desktop-lazy-verbose nil)
  ;; :config
  ;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  )

;;; Super-save
;; Automatically save buffers when you do certain things
(use-package super-save
  :disabled                             ; Opting for built in auto-save
  :demand t
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

;;; Built-in auto save and backup
;; Make recovery files
(use-package files
  :straight nil
  :custom
  ;; NOTE 2023-01-09: `auto-save-list-file-prefix' and `backup-directory-alist'
  ;; are set by `no-littering'

  ;; Auto save
  (auto-save-default t) ; Only a local minor mode exists; this variable influences the global value
  (auto-save-interval 150)
  (auto-save-timeout 8)
  (delete-auto-save-files nil)
  (kill-buffer-delete-auto-save-files nil)
  (auto-save-no-message t)

  ;; Save buffer after idle time
  (remote-file-name-inhibit-auto-save-visited nil)
  (auto-save-visited-interval 15)
  (auto-save-visited-predicate
   ;; Inspired by `super-save'
   (lambda ()
     (< (save-restriction (widen) (count-lines (point-min) (point-max)))
        5000)))

  ;; Backups
  (make-backup-files t)
  (backup-by-copying t)                 ; Don't clobber symlinks
  (version-control t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (delete-old-versions t)
  :init
  (auto-save-visited-mode))

;;; persistence-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'persistence-rcp)
