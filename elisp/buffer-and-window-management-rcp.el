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

;;;; Savehist
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

;;;; Winner-mode
;; Reverting and traversing window configurations across time
(use-package winner
  :ghook 'after-init-hook
  :general ("C-<left>" 'winner-undo
            "C-<right>" 'winner-redo)
  :custom
  (winner-dont-bind-my-keys t) ; Don't bind keys because I bind them myself
  )

;;;; Eyebrowse
;; Provide a simple way to have workspaces
(use-package eyebrowse
  :ghook 'after-init-hook
  :general
  (:states '(visual normal motion)
           "gt" 'eyebrowse-next-window-config
           "ga" 'eyebrowse-prev-window-config
           "gz" 'eyebrowse-last-window-config
           )
  (:keymaps 'eyebrowse-mode-map
            "C-c C-w r" 'eyebrowse-rename-window-config
            "C-c C-w c" 'eyebrowse-close-window-config
            )
  ("M-1" 'eyebrowse-switch-to-window-config-1
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
  )

;;;; Ace-window
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

;;;; Shackle
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
                   ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)
                   ("*Flycheck errors*" :select t :align below :size 0.33)
                   ("\\*Dogears List\\*" :regexp t :align below :same t :inhibit-window-quit t :size 0.3)
                   ))
  (shackle-select-reused-windows t)
  )

;;;; Burly
(use-package burly
  :gfhook ('after-init-hook 'bookmark-maybe-load-default-file nil nil t) ; Load bookmarks immediately for access
  :general (kb/leader-keys
             "Bw" '(burly-bookmark-windows :which-key "Burly windows")
             "Bm" '(burly-open-bookmark :which-key "Open burly bookmark")
             "BM" '(burly-open-last-bookmark :which-key "Open last bookmark")
             )
  :custom
  (bookmark-save-flag 1) ; Save bookmarks file every time there is a changed or added bookmark
  :preface (require 'bookmark)
  )

;;; buffer-and-window-management-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffer-and-window-management-rcp)
