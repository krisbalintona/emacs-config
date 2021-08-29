;;; buffer-and-window-management-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for traversing and managing windows and buffers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Automatically managed
;;;;; Savehist
;; Make history of certain things (e.g. minibuffer) persistent across sessions
(use-package savehist
  :straight nil
  :ghook 'after-init-hook
  :custom
  (savehist-autosave-interval 300)
  :config
  (add-to-list 'savehist-additional-variables 'recentf-list) ; Save recent files
  (add-to-list 'savehist-additional-variables 'kill-ring) ; Save kill ring
  )

;;;;; Recentf
;; Enable logging of recent files
(use-package recentf
  :straight nil
  :ghook 'after-init-hook
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  )

;;;;; Save-place-mode
;; Save and restore the point's location in files
(use-package saveplace
  :straight nil
  :ghook ('after-init-hook 'save-place-mode)
  )

;;;;; Super-save
;; Automatically save buffers when you do certain things
(use-package super-save
  :ghook 'after-init-hook
  :custom
  (super-save-auto-save-when-idle t) ; Save buffer if Emacs is idle
  (super-save-idle-duration 10) ; Wait 10 seconds for idle trigger
  (super-save-remote-files t) ; Turn on saving of remote files (those pulled from git repo?)
  (super-save-exclude nil) ; Don't exclude anything from being saved
  :config
  (add-to-list 'super-save-triggers 'evil-window-next)
  (add-to-list 'super-save-hook-triggers 'eyebrowse-pre-window-switch-hook)
  )

;;;;; Autorevert
;; Automatically update buffers as files are externally modified
(use-package autorevert
  :ghook ('after-init-hook 'global-auto-revert-mode)
  :custom
  (auto-revert-interval 7)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose t)
  )

;;;; Window configuration
;;;;; Winner-mode
;; Reverting and traversing window configurations across time
(use-package winner
  :ghook 'after-init-hook
  :general ("C-<left>" 'winner-undo
            "C-<right>" 'winner-redo)
  :custom
  (winner-dont-bind-my-keys t) ; Don't bind keys because I bind them myself
  (winner-boring-buffers '("*Completions*" "*Help*" "*Apropos*" "*Buffer List*" "*info*" "*Compile-Log*"))
  (winner-boring-buffers-regexp "\\*helpful variable:\\|magit:") ; Skip `magit' and `helpful' buffers
  )

;;;;; Eyebrowse
;; Provide a simple way to have workspaces
(use-package eyebrowse
  :ghook 'after-init-hook
  :general
  ("M-`" 'eyebrowse-switch-to-window-config-0
   "M-1" 'eyebrowse-switch-to-window-config-1
   "M-2" 'eyebrowse-switch-to-window-config-2
   "M-3" 'eyebrowse-switch-to-window-config-3
   "M-4" 'eyebrowse-switch-to-window-config-4
   "M-5" 'eyebrowse-switch-to-window-config-5
   "M-6" 'eyebrowse-switch-to-window-config-6
   "M-7" 'eyebrowse-switch-to-window-config-7
   "M-8" 'eyebrowse-switch-to-window-config-8
   "M-9" 'eyebrowse-switch-to-window-config-9
   "M-0" 'eyebrowse-switch-to-window-config-0
   )
  (:states '(visual normal motion)
           "gt" 'eyebrowse-next-window-config
           "ga" 'eyebrowse-prev-window-config
           "gv" 'eyebrowse-last-window-config
           )
  (:keymaps 'eyebrowse-mode-map
            :prefix "C-c C-w"
            "r" 'eyebrowse-rename-window-config
            "c" 'eyebrowse-close-window-config
            )
  :custom
  (eyebrowse-default-workspace-slot 0) ; Start at 0
  (eyebrowse-keymap-prefix (kbd "C-c C-w"))
  (eyebrowse-mode-line-left-delimiter " ")
  (eyebrowse-mode-line-right-delimiter " ")
  (eyebrowse-mode-line-separator " ")
  (eyebrowse-tagged-slot-format "%t") ; Only show workspace name (tag) if avail
  (eyebrowse-wrap-around t) ; Cycle back to beginning when at the end and vice versa
  (eyebrowse-switch-back-and-forth t) ; Select current workspace to go to last used one
  :config
  (set-face-attribute 'eyebrowse-mode-line-active nil :weight 'semi-bold)
  (setq-default eyebrowse-new-workspace nil)    ; By default, just clone the current window configuration
  )

;;;;; Bookmark
(use-package bookmark
  :straight nil
  :hook (after-init . bookmark-maybe-load-default-file) ; Load bookmarks immediately for access
  :custom
  (bookmark-save-flag 1) ; Save bookmarks file every time there is a changed or added bookmark
  )

;;;;; Burly
(use-package burly
  :general (kb/leader-keys
             "Bw" '(burly-bookmark-windows :which-key "Burly windows")
             "Bm" '(burly-open-bookmark :which-key "Open burly bookmark")
             "BM" '(burly-open-last-bookmark :which-key "Open last bookmark")
             "Bo" '(burly-open-url :which-key "Open url at point")
             )
  )

;;;;; Shackle
;; Control the behavior of popup and side windows
(use-package shackle
  :commands shackle-mode
  :ghook 'after-init-hook
  :custom
  (shackle-rules '((flycheck-verify-mode :inhibit-window-quit t :same t)
                   (helpful-mode :inhibit-window-quit t :same t)
                   ;; (help-mode :inhibit-window-quit t :same t) ; Messes with org-roam-doctor buffer
                   (process-menu-mode :inhibit-window-quit t :same t)
                   ;; ("magit:" :regexp t :inhibit-window-quit t :align t :same t) ; Replaced by creating `kb/magit-mode-quit-window'
                   ("\\*org-roam\\*" :regexp t :align right :same nil :size 0.2)
                   ("*Flycheck errors*" :select t :align below :size 0.33)
                   ("\\*devdocs\\*" :select t :same nil)
                   ("\\*Dogears List\\*" :regexp t :align below :same t :inhibit-window-quit t :size 0.3)
                   ))
  (shackle-select-reused-windows t)
  )

;;;; Other
;;;;; Ace-window
(use-package ace-window
  :general ("M-w" '(ace-window :which-key "Ace window"))
  :custom
  (aw-scope 'visible)
  (aw-background t)
  (aw-dispatch-always nil)
  (aw-minibuffer-flag t)
  (aw-keys '(?h ?j ?k ?l ?H ?J ?K ?L))
  (aw-dispatch-alist
   '((?x aw-delete-window "Delete Window")
     (?m aw-swap-window "Swap Windows")
     (?M aw-move-window "Move Window")
     (?c aw-copy-window "Copy Window")
     (?j aw-switch-buffer-in-window "Select Buffer")
     (?n aw-flip-window)
     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?c aw-split-window-fair "Split Fair Window")
     (?v aw-split-window-vert "Split Vert Window")
     (?b aw-split-window-horz "Split Horz Window")
     (?o delete-other-windows "Delete Other Windows")
     (?? aw-show-dispatch-help))
   )
  )

;;;;; Dogears
;; Save and return to exact locations when you want, where you want
(use-package dogears
  :disabled t ; For now
  :straight (dogears :type git :host github :repo "alphapapa/dogears.el")
  :ghook 'after-init-hook
  :general
  (:keymaps 'dogears-list-mode-map
            :states '(motion normal visual)
            "dd" 'dogears-list-delete
            "RET" 'dogears-list-go
            )
  (kb/leader-keys
    "bd" '(dogears-go :which-key "Dogears go")
    "bD" '(dogears-list :which-key "Dogears list")
    )
  :custom
  (dogears-limit 200)
  (dogears-line-width 40)
  (dogears-ignore-places-functions
   '(dogears--ignored-mode-p
     minibufferp
     (lambda () (string-match "^/tmp/" (expand-file-name buffer-file-truename)))
     )
   )
  (dogears-ignore-modes
   '(fundamental-mode special-mode helm-major-mode
                      dogears-list-mode messages-buffer-mode custom-mode helpful-mode elfeed-search-mode elfeed-show-mode org-roam-mode embark-collect-mode man-mode flycheck-error-list-mode ledger-report-mode

                      magit-status-mode magit-log-mode magit-wip-mode magit-diff-mode magit-blob-mode magit-refs-mode magit-stash-mode magit-blame-mode magit-reflog-mode magit-cherry-mode magit-proces-mode magit-section-mode magit-stashes-mode magit-repolist-mode magit-revision-mode magit-log-select-mode magit-merge-preview-mode magit-wip-after-save-mode magit-submodule-list-mode magit-blame-read-only-mode magit-wip-after-apply-mode magit-wip-before-apply-mode magit-wip-initial-backup-mode magit-wip-after-save-local-mode unpackaged/magit-log-date-headers-mode

                      )
   )
  (dogears-functions
   '(
     ))
  (dogears-hooks
   '(imenu-after-jump-hook
     change-major-mode-hook
     eyebrowse-pre-window-switch-hook
     window-configuration-change-hook
     ))
  )

;;; buffer-and-window-management-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffer-and-window-management-rcp)
