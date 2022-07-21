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
  (winner-boring-buffers '("*Completions*" "*Help*" "*Apropos*" "*Buffer List*" "*info*" "*Compile-Log*" ))
  (winner-boring-buffers-regexp "\\*helpful variable:\\|\\*helpful command:\\|magit:") ; Skip `magit' and `helpful' buffers
  :init (winner-mode))

;;;; Windmove
(use-package windmove
  :hook (after-init . windmove-mode)
  :init
  (windmove-default-keybindings '(hyper))
  (windmove-swap-states-default-keybindings '(shift hyper)))

;;;; Window
(use-package window
  :straight nil
  :general (:keymaps 'global-map ; For some reason, this needs to be explicitly set?
            (general-chord "wt") 'window-toggle-side-windows
            (general-chord "ww") 'other-window)
  :custom
  (split-height-threshold nil)       ; Threshold for vertical window splitting
  (split-width-threshold 160)        ; Threshold for horizontal window splitting
  (window-combination-resize t)
  (even-window-sizes 'height-only)      ; Or `t' or ``width-only''
  (window-sides-vertical t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-alist
   `(;; Automatically hide

     ;; Same window
     ("\\*helpful *"
      (display-buffer-reuse-mode-window
       display-buffer-same-window))
     ("*Flycheck errors*"
      (display-buffer-same-window))
     ("\\*devdocs\\*"
      (display-buffer-same-window))
     
     ;; To the left
     ("\\*Faces\\*"
      (kb/select-buffer-in-side-window)
      (window-width . 0.33)
      (side . left)
      (slot . 2)
      (window-parameters . ((no-other-window . t))))
     ((lambda (buf act) (or (equal (kb/buffer-major-mode buf) 'Custom-mode)
                       (string-match-p "^\\*Customize" (buffer-name))))
      (display-buffer-reuse-window
       kb/select-buffer-in-side-window
       display-buffer-in-direction)
      (window-width . 0.4)
      (side . left)
      (direction . left)
      (slot . 1))
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
     ("\\*.*\\(eshell\\).*"
      (display-buffer-reuse-mode-window
       kb/select-buffer-in-side-window)
      (side . right)
      (window-width . 0.4))
     ("\\*Async Shell Command\\*"
      (kb/select-buffer-in-side-window
       display-buffer-in-direction)
      (window-width . 0.20)
      (side . right)
      (direction . right)
      (slot . 4)
      (window-parameters . ((no-other-window . t))))

     ;; To the top
     ("\\*Messages\\*"
      (display-buffer-reuse-window
       kb/select-buffer-in-side-window)
      (window-height . 0.36)
      (side . top)
      (slot . 1))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
      (display-buffer-reuse-window
       kb/select-buffer-in-side-window)
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
     ("\\*Embark Actions\\*"
      (display-buffer-reuse-mode-window
       display-buffer-at-bottom)
      (window-height . fit-window-to-buffer)
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none))))
     ((lambda (buf act) (equal (kb/buffer-major-mode buf) 'dap-ui-repl-mode))
      (display-buffer-reuse-mode-window
       display-buffer-at-bottom)
      (window-height . 12)
      (window-parameters . ((mode-line-format . none))))
     ((lambda (buf act) (equal (kb/buffer-major-mode buf) 'special-mode))
      (kb/select-buffer-at-bottom)
      (window-height . 0.35))

     ;; Below current window
     ("\\*\\(Calendar\\|Org Select\\).*"
      (display-buffer-reuse-mode-window
       display-buffer-below-selected)
      (window-height . fit-window-to-buffer))
     ("\\*\\(Embark\\)?.*Completions.*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none))))
     ("\\*\\(I?Python3\\|Python3\\)\\*"
      (display-buffer-reuse-mode-window
       display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.27))))
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
  (defun kb/select-buffer-at-bottom (buffer alist)
    "Display buffer in a side window and select it"
    (let ((window (display-buffer-at-bottom buffer alist)))
      (select-window window)
      )))

;;;; Eyebrowse
;; Provide a simple way to have workspaces
(use-package eyebrowse
  :general
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
   "M-\\" 'eyebrowse-last-window-config
   "M-[" 'eyebrowse-prev-window-config
   "M-]" 'eyebrowse-next-window-config)
  (:keymaps 'eyebrowse-mode-map
   :prefix eyebrowse-keymap-prefix
   "d" 'eyebrowse-close-window-config)
  :custom
  (eyebrowse-default-workspace-slot 0)  ; Start at 0
  (eyebrowse-keymap-prefix (kbd "C-c M-w"))
  (eyebrowse-mode-line-style t)         ; Always show
  (eyebrowse-mode-line-left-delimiter "[")
  (eyebrowse-mode-line-right-delimiter "]")
  (eyebrowse-mode-line-separator "|")
  (eyebrowse-tagged-slot-format "%t")  ; Only show workspace name (tag) if avail
  (eyebrowse-wrap-around t) ; Cycle back to beginning when at the end and vice versa
  (eyebrowse-switch-back-and-forth t) ; Select current workspace to go to last used one
  :init
  (eyebrowse-mode))

;;;; Ace-window
(use-package ace-window
  :general ("C-c w" '(ace-window :wk "Ace window")
            "C-c W" '(ace-swap-window :wk "Ace swap-window"))
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
   ))

;;;; Popper
;; "Tame ephemeral windows"
(use-package popper
  :general ("C-;" 'popper-toggle-latest
            "C-:" 'popper-cycle
            "C-'" 'popper-kill-latest-popup
            "C-\"" 'popper-toggle-type
            )
  :custom
  (popper-reference-buffers
   '(;; General
     "^\\*Messages\\*"
     "^\\*Warnings\\*"
     "^\\*Backtrace\\*"
     "^\\*Customize*"
     "^\\*compilation\\*"
     comint-mode

     ;; Coding
     "[Oo]utput\\*"
     "\\*Shell Command Output\\*"
     "\\*Async Shell Command\\*"
     "^\\*Compile-Log\\*$"
     compilation-mode

     special-mode
     help-mode
     "^Calc:"
     dap-ui-repl-mode
     quickrun--mode
     gud-mode

     ;; Shells
     ;; To consistently match shells, supply both the buffer name and major mode
     "^\\*eshell.*\\*$"
     eshell-mode
     "^\\*shell.*\\*$"
     shell-mode
     "^\\*IPython3.*\\*$"
     py-shell-mode
     ))
  (popper-display-control nil)
  (popper-group-function
   #'(lambda ()
        (cond
         ((string-match-p "\\(?:~/\\.config/\\|~/dotfiles/\\)" default-directory)
          'Config)
         ((locate-dominating-file default-directory "init.el") 'Emacs)
         ((project-current) (project-root (project-current)))
         (t nil)                         ; No group
         )))
  (popper-mode-line '(:eval (propertize " POP" 'face 'mode-line-buffer-id)))

  ;; Popper-echo
  (popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (popper-echo-dispatch-actions t)
  (popper-echo-transform-function
   #'(lambda (name)
        (cond
         ((string-match "^\\*\\(.*?\\)\\(?:Output\\|Command\\)\\*$" name)
          (concat (match-string 1 name)
                  "(O)"))
         ((string-match "^\\*\\(.*?\\)\\(?:Help\\|helpful\\)\\*$" name)
          (concat (match-string 1 name)
                  "(H)"))
         ((string-match "^\\*Warnings\\*" name)
          (concat (match-string 1 name)
                  "(W)"))
         ((string-match "^\\*Backtrace\\*" name)
          (concat (match-string 1 name)
                  "(B)"))
         ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
          (concat (match-string 1 name)
                  "(L)"))
         ((string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
          (concat (match-string 1 name)
                  "(C)"))
         ((string-match "^\\*Java Run" name)
          (concat (match-string 1 name)
                  (concat "(CG)" (substring name 18 (- (length name) 1)))
                  ))
         ((string-match "^\\*Customize" name)
          (concat (match-string 1 name)
                  (concat "(CG)" (substring name 18 (- (length name) 1)))
                  ))
         (t name))
        ))

  ;; Mode line
  (popper-mode-line-position 0)
  (popper-mode-line '(:eval
                      (propertize "   (P)" 'face 'mode-line-emphasis)))
  :config
  (popper-mode)
  (popper-echo-mode)              ; Hinting in the echo area when `popper-cycle'
  )

;;; Buffers
;;;; Bookmark
(use-package bookmark
  :hook (kill-emacs . kb/bookmark-cleanup)
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
          (quit-window))))
    (bookmark-save)))

;;; buffers-and-windows-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffers-and-windows-rcp)
