;;; buffers-and-windows-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for traversing and managing windows and buffers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Window configurations
;;;; Winner-mode
;; Reverting and traversing window configurations across time
(use-package winner
  :general ("C-<left>" 'winner-undo
            "C-<right>" 'winner-redo)
  :custom
  (winner-dont-bind-my-keys t) ; Don't bind keys because I bind them myself
  (winner-boring-buffers '("*Completions*" "*Help*" "*Apropos*" "*Buffer List*" "*info*" "*Compile-Log*"))
  (winner-boring-buffers-regexp "\\*helpful variable:\\|\\*helpful command:\\|magit:") ; Skip `magit' and `helpful' buffers
  :init (winner-mode)
  )

;;;; Shackle
;; Control the behavior of popup and side windows
(use-package shackle
  :disabled t
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
  (shackle-select-reused-windows t)     ; Reuse windows by default
  )

;;;; Window
(use-package window
  :straight nil
  :general
  (:keymaps 'ctl-x-map
            "q" '(kill-buffer-and-window :which-key "Kill buffer and window")
            )
  (kb/general-keys
    "wt" '(window-toggle-side-windows :which-key "Toggle side windows")
    )
  :custom
  (split-height-threshold nil)       ; Threshold for vertical window splitting
  (split-width-threshold 160)        ; Threshold for horizontal window splitting
  (window-combination-resize t)
  (even-window-sizes 'height-only)      ; Or `t' or ``width-only''
  (window-sides-vertical t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-alist
   `(;; No window
     ("\\`\\*Async Shell Command\\*\\'"
      (display-buffer-no-window))
     ;; Same window
     ("\\*helpful *"
      (display-buffer-reuse-mode-window display-buffer-same-window))
     ("*Flycheck errors*"
      (display-buffer-same-window))
     ("\\*devdocs\\*"
      (display-buffer-same-window))
     ;; To the left
     ("\\*Faces\\*"
      (display-buffer-in-side-window)
      (window-width . 0.25)
      (side . left)
      (slot . -2)
      (window-parameters . ((no-other-window . t))))
     ((lambda (buf act) (or (equal (kb/buffer-major-mode buf) 'Custom-mode)
                       (string-match-p "^\\*Customize" (buffer-name))))
      (kb/select-buffer-in-side-window)
      (window-width . 74)
      (side . left)
      (slot . 5))
     ((lambda (buf act) (equal (kb/buffer-major-mode buf) 'help-mode))
      (display-buffer-reuse-window
       kb/select-buffer-in-side-window
       display-buffer-in-direction)
      (window-width . 74)
      (side . left)
      (direction . left)
      (slot . 2)
      (window-parameters . ((split-window . #'ignore))))
     ;; To the right
     ("\\*org-roam\\*"
      (display-buffer-in-side-window)
      (dedicated . t)
      (side . right)
      (window-width . 0.2))
     ("\\*.*\\(e?shell\\|v?term\\).*"
      (display-buffer-reuse-mode-window
       kb/select-buffer-in-side-window)
      (side . right)
      (window-width . 0.4))
     ;; To the top
     ("\\*Messages\\*"
      (display-buffer-in-side-window)
      (window-height . 0.16)
      (side . top)
      (slot . 1))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.3)
      (side . top)
      (slot . 2))
     ("\\*\\(?:Org Select\\|Agenda Commands\\)\\*"
      (kb/select-buffer-in-side-window)
      (window-height . (lambda (win) (fit-window-to-buffer win)))
      (side . top)
      (slot . -2)
      (preserve-size . (nil . t))
      (window-parameters . ((mode-line-format . nil))))
     ;; To the bottom
     ("\\*Flycheck errors\\*"
      (display-buffer-reuse-mode-window
       display-buffer-in-side-window)
      (window-height . 0.33))
     ("\\(?:[Oo]utput\\)\\*"
      (display-buffer-in-side-window)
      (window-height . (lambda (win)
                         (fit-window-to-buffer win (floor (frame-height) 3))))
      (side . bottom)
      (slot . -4))
     ("\\*Async Shell Command\\*"
      (kb/select-buffer-in-side-window)
      (window-height . 0.20)
      (side . bottom)
      (slot . -4)
      (window-parameters . ((no-other-window . t))))
     ;; Below current window
     ("\\*\\(Calendar\\|Org Select\\).*"
      (display-buffer-reuse-mode-window
       display-buffer-below-selected)
      (window-height . fit-window-to-buffer))
     ("\\*Embark Actions.*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . fit-window-to-buffer)
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none))))
     ("\\*\\(Embark\\)?.*Completions.*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none))))
     ("\\*\\(I?Python3\\|Python3\\)\\*"
      (display-buffer-reuse-mode-window
       kb/select-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.27))
     ))
  :init
  ;; Helper functions for `display-buffer-alist'
  (defun kb/buffer-major-mode (&optional buffer-or-name)
    "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
    (buffer-local-value 'major-mode
                        (if buffer-or-name
                            (get-buffer buffer-or-name)
                          (current-buffer))))
  (defun kb/select-buffer-in-side-window (buffer alist)
    "Display buffer in a side window and select it"
    (let ((window (display-buffer-in-side-window buffer alist)))
      (select-window window)
      ))
  (defun kb/select-buffer-in-side-window (buffer alist)
    "Display buffer in a side window and select it"
    (let ((window (display-buffer-in-side-window buffer alist)))
      (select-window window)
      ))
  )

;;;; Eyebrowse
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
  )

;;;; Ace-window
(use-package ace-window
  :general ("M-w" '(ace-window :which-key "Ace window"))
  :custom
  (aw-scope 'frame)
  (aw-background t)
  (aw-dispatch-always t)   ; Open dispatch when less than three windows are open
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

;;;; Burly
(use-package burly
  :disabled t ; NOTE 2021-08-30: For some reason, burly bookmarks can't be deleted by `bookmark-delete'
  :general (kb/general-keys
             "Bw" '(burly-bookmark-windows :which-key "Burly windows")
             "Bm" '(burly-open-bookmark :which-key "Open burly bookmark")
             "BM" '(burly-open-last-bookmark :which-key "Open last bookmark")
             "Bo" '(burly-open-url :which-key "Open url at point")
             )
  )

;;;; Popper
;; "Tame ephemeral windows"
(use-package popper
  :general ("C-'" 'popper-toggle-latest
            "M-'" 'popper-cycle
            "C-\"" 'popper-kill-latest-popup
            "C-M-'" 'popper-toggle-type
            )
  :custom
  (popper-reference-buffers
   '(;; General
     "\\*Messages\\*"
     "^\\*Warnings\\*$"
     "^\\*Backtrace\\*"

     ;; Coding
     "[Oo]utput\\*"
     "\\*Shell Command Output\\*"
     "\\*Async Shell Command\\*"
     "^\\*Compile-Log\\*$"
     compilation-mode

     help-mode
     "^Calc:"

     ;; Shells
     ;; To consistently match shells, supply both the buffer name and major mode
     "^\\*eshell.*\\*$"
     eshell-mode
     "^\\*shell.*\\*$"
     shell-mode
     "^\\*term.*\\*$"
     term-mode
     "^\\*vterm.*\\*$"
     vterm-mode
     "^\\*IPython3.*\\*$"
     py-shell-mode
     ))
  (popper-display-control 'user)
  (popper-display-function 'popper-select-popup-at-bottom)
  (popper-group-function
   #'(lambda ()
       (let ((dd (abbreviate-file-name default-directory)))
         (cond
          ((string-match-p "\\(?:~/\\.config/\\|~/dotfiles/\\)" dd)
           'Config)
          ((locate-dominating-file dd "init.el") 'Emacs)
          (t (popper-group-by-project)) ; Default to project.el
          ))
       ))
  (popper-mode-line nil)                ; Remove modeline

  ;; Popper-echo
  (popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (popper-echo-dispatch-actions t)
  (popper-echo-transform-function
   #'(lambda (name)
       (cond
        ((string-match "^\\*vterm:? ?\\(.*\\)\\*$" name)
         (concat (match-string 1 name)
                 (if (string-empty-p (match-string 1 name)) "shell(V)" "(V)")))
        ((string-match "^\\*eshell:? ?\\(.*\\)\\*$" name)
         (concat (match-string 1 name)
                 (if (string-empty-p (match-string 1 name)) "shell(E)" "(E)")))
        ((string-match "^\\*\\(.*?\\)\\(?:Output\\|Command\\)\\*$" name)
         (concat (match-string 1 name)
                 "(O)"))
        ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
         (concat (match-string 1 name)
                 "(L)"))
        ((string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
         (concat (match-string 1 name)
                 "(C)"))
        (t name))
       ))
  :config
  (popper-mode)
  (popper-echo-mode)              ; Hinting in the echo area when `popper-cycle'
  )

;;; Buffers
;;;; Bookmark
(use-package bookmark
  :custom
  (bookmark-save-flag 1) ; Save bookmarks file every time there is a changed or added bookmark
  :config
  (bookmark-maybe-load-default-file) ; Load bookmarks immediately for access

  ;; From https://www.reddit.com/r/emacs/comments/e1uyvk/weekly_tipstricketc_thread/f8v4re2?utm_source=share&utm_medium=web2x&context=3
  (defun kb/bookmark-cleanup ()
    "Check for bookmarks which point to deleted/moved files at
startup and popup bookmark menu to fix it"
    (require 'recentf)
    (bookmark-maybe-load-default-file)
    (let ((lost ()))
      (dolist (bm (mapcar #'car bookmark-alist))
        (let ((file (bookmark-get-filename bm)))
          ;; see `recentf-keep'
          (when (and file (not (recentf-keep-p file)))
            (push bm lost))))
      (when lost
        (call-interactively 'bookmark-bmenu-list)
        (while lost
          (bookmark-bmenu-goto-bookmark (car lost))
          ;; just marks for deletion does not delete
          (bookmark-bmenu-delete)
          (pop lost))
        (message "Fix fileless file bookmarks, press C-M-c when done.")
        (recursive-edit)
        (when (derived-mode-p 'bookmark-bmenu-mode)
          (quit-window)))))
  (add-hook 'emacs-startup-hook #'kb/bookmark-cleanup)
  )

;;;; Dogears
;; Save and return to exact locations when you want, where you want
(use-package dogears
  :disabled t ; For now
  :straight (dogears :type git :host github :repo "alphapapa/dogears.el")
  :after bookmark
  :ghook 'after-init-hook
  :general
  (:keymaps 'dogears-list-mode-map
            :states '(motion normal visual)
            "dd" 'dogears-list-delete
            "RET" 'dogears-list-go
            )
  (kb/buffer-keys
    "d" '(dogears-go :which-key "Dogears go")
    "D" '(dogears-list :which-key "Dogears list")
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

;;;; Ibuffer
;; Manage buffer list
(use-package ibuffer
  :general (kb/buffer-keys
             "l" '(ibuffer :which-key "Ibuffer")
             "L" '(ibuffer-other-window :which-key "Ibuffer other window"))
  :custom
  (ibuffer-expert nil)
  (ibuffer-truncate-lines nil)
  (ibuffer-default-directory (cdr (project-current)))
  )

;;; buffers-and-windows-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffers-and-windows-rcp)
