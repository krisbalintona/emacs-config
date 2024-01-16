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
  :elpaca nil
  :general ("C-<left>" 'winner-undo
            "C-<right>" 'winner-redo)
  :custom
  (winner-dont-bind-my-keys t) ; Don't bind keys because I bind them myself
  (winner-boring-buffers '("*Completions*" "*Help*" "*Apropos*" "*Buffer List*" "*info*" "*Compile-Log*" ))
  :init (winner-mode))

;;;; Windmove
(use-package windmove
  :elpaca nil
  :init
  (windmove-default-keybindings '(hyper))
  (windmove-swap-states-default-keybindings '(shift hyper))
  (windmove-mode))

;;;; Transpose-frame
;; Rotate window configuration
(use-package transpose-frame
  :general ("H-r" 'rotate-frame-anticlockwise
            "H-l" 'rotate-frame-clockwise))

;;;; Window
(use-package window
  :elpaca nil
  :custom
  (window-resize-pixelwise t)
  (window-sides-vertical t)

  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-alist
   `(;; Automatically hide

     ;; Full frame
     ((lambda (buf act) (equal (kb/buffer-major-mode buf) 'org-msg-edit-mode))
      (display-buffer-pop-up-window
       display-buffer-same-window))

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

     ;; To the right
     ("\\*org-roam\\*"
      (display-buffer-in-side-window)
      (dedicated . t)
      (side . right)
      (window-width . 0.2))
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
      (window-height . 0.27))
     ("\\*Racket REPL"
      (display-buffer-reuse-mode-window
       display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.35))
     ((lambda (buf act) (equal (kb/buffer-major-mode buf) 'rustic-cargo-run-mode))
      (display-buffer-reuse-mode-window
       display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.35))))
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
  :disabled                             ; Testing out `tab-bar'
  :demand
  :general
  (:keymaps 'eyebrowse-mode-map
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
  (eyebrowse-mode-line-right-delimiter "] ")
  (eyebrowse-mode-line-separator "|")
  (eyebrowse-tagged-slot-format "%t")  ; Only show workspace name (tag) if avail
  (eyebrowse-wrap-around t) ; Cycle back to beginning when at the end and vice versa
  (eyebrowse-switch-back-and-forth t) ; Select current workspace to go to last used one
  :config
  (eyebrowse-mode)
  ;; Immediately remove from mode line so I can manually place it
  (delete '(eyebrowse-mode (:eval (eyebrowse-mode-line-indicator))) mode-line-misc-info))

;;;; Tab-bar
(use-package tab-bar
  :elpaca nil
  :general
  (:keymaps 'tab-prefix-map
            "w" 'tab-bar-move-window-to-tab)
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice 'clone)
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-tab-hints t)
  (tab-bar-show t)
  (tab-bar-format
   '(tab-bar-format-tabs-groups
     tab-bar-format-align-right
     tab-bar-format-global))
  :init
  (tab-bar-mode)
  (tab-bar-history-mode))

;;;; Project-tab-groups
(use-package project-tab-groups
  :disabled                             ; Intrusive for me
  :init
  (project-tab-groups-mode))

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
  :general ("M-`" 'popper-toggle-latest
            "C-`" 'popper-cycle
            "H-`" 'popper-kill-latest-popup
            "C-~" 'popper-toggle-type)
  :custom
  (popper-reference-buffers
   '(;; General
     "^\\*Messages\\*"
     "^\\*Warnings\\*"
     "^\\*Backtrace\\*"
     "^\\*compilation\\*"
     comint-mode

     ;; Coding
     "[Oo]utput\\*"
     "\\*Shell Command Output\\*"
     "\\*Async Shell Command\\*"
     "^\\*Compile-Log\\*$"
     compilation-mode

     ;; Shells
     ;; To consistently match shells, supply both the buffer name and major mode
     "^\\*shell.*\\*$"
     shell-mode
     "^\\*IPython3.*\\*$"
     py-shell-mode

     ;; Other
     special-mode
     "^Calc:"
     dap-ui-repl-mode
     quickrun--mode
     gud-mode
     racket-repl-mode
     rustic-cargo-run-mode
     rustic-cargo-test-mode
     rustic-compilation-mode
     Man-mode
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
  :elpaca nil
  :custom
  (bookmark-save-flag 1)                 ; Save bookmarks file every new entry
  (bookmark-watch-bookmark-file 'silent) ; Reload bookmarks file without query
  :config
  (bookmark-maybe-load-default-file))   ; Load bookmarks immediately for access

;;;; Bufler
(use-package bufler
  :general ([remap list-buffers] 'bufler))

;;;; Burly
(use-package burly
  :elpaca (:depth 1
                  :fetcher github
                  ;; NOTE 2023-07-15: See
                  ;; https://github.com/alphapapa/burly.el/issues/28 for details on
                  ;; this branch
                  :repo "alphapapa/burly.el"
                  :branch "wip/readablep"
                  :files ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el")))
  :init
  (burly-tabs-mode))

;;;; Perfect-margin
(use-package perfect-margin
  :diminish
  :hook (elpaca-after-init . perfect-margin-mode)
  :custom
  (perfect-margin-visible-width 128)
  (perfect-margin-only-set-left-margin nil)
  (perfect-margin-ignore-modes
   '(exwm-mode doc-view-mode nov-mode pdf-view-mode))
  (perfect-margin-ignore-filters
   '(window-minibuffer-p
     (lambda (window)
       (with-selected-window window (bound-and-true-p olivetti-mode)))))
  :config
  ;; Additional mouse bindings for now wider margins. Taken from
  ;; https://github.com/mpwang/perfect-margin#additional-binding-on-margin-area
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll)))

  ;; In order to split windows sensibly (horizontally when desirable). Inspired
  ;; by `visual-fill-column-split-window-sensibly'. A relevant issue might be
  ;; found at https://github.com/mpwang/perfect-margin/issues/9
  (defun kb/split-window-sensibly (&optional window)
    "tk"
    (let ((margins (window-margins window))
          (fringes (window-fringes window))
          new)
      ;; Unset the margins and try to split the window. I choose to set fringes
      ;; and margins both to 0, though adapting this to depend on the value of
      ;; `olivetti-style' would remove undesirable behavior in edge cases
      (set-window-margins window 0 0)
      (set-window-fringes window 0 0)
      (unwind-protect
          (setq new (split-window-sensibly window))
        (when (not new)
          (set-window-margins window (car margins) (cdr margins))
          (set-window-fringes window (car fringes) (cdr fringes))))))
  (setq split-window-preferred-function 'kb/split-window-sensibly))

;;; buffers-and-windows-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffers-and-windows-rcp)
