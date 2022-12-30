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

;;; persistence-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'persistence-rcp)
