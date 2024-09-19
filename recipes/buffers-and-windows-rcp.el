;;; buffers-and-windows-rcp.el --- Buffer and window control  -*- lexical-binding: t; -*-

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

;; Packages for traversing and managing windows and buffers.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Server
;; Prefer Emacs server over Emacs daemon
(use-package server
  :demand
  :ensure nil
  :hook (after-init . (lambda () (unless (server-running-p)
                                   (server-mode))))
  :custom
  (server-client-instructions nil))

;;;; Window configurations
;;;;; Winner-mode
;; Reverting and traversing window configurations across time
(use-package winner
  :ensure nil
  :general ("C-<left>" 'winner-undo
            "C-<right>" 'winner-redo)
  :custom
  (winner-dont-bind-my-keys t) ; Don't bind keys because I bind them myself
  (winner-boring-buffers '("*Completions*"))
  :init
  (winner-mode))

;;;;; Windmove
(use-package windmove
  :ensure nil
  :general (:keymaps 'windmove-mode-map
                     "C-M-s-h" 'windmove-left
                     "C-M-s-j" 'windmove-down
                     "C-M-s-k" 'windmove-up
                     "C-M-s-l" 'windmove-right
                     "C-M-s-<left>" 'windmove-left
                     "C-M-s-<down>" 'windmove-down
                     "C-M-s-<up>" 'windmove-up
                     "C-M-s-<right>" 'windmove-right
                     "C-M-s-H" 'windmove-swap-states-left
                     "C-M-s-J" 'windmove-swap-states-down
                     "C-M-s-K" 'windmove-swap-states-up
                     "C-M-s-L" 'windmove-swap-states-right)
  :init
  (windmove-mode))

;;;;; Transpose-frame
;; Rotate window configuration
(use-package transpose-frame
  :general ("C-M-s-<right>" 'rotate-frame-clockwise
            "C-M-s-l" 'rotate-frame-clockwise))

;;;;; Window
(use-package window
  :ensure nil
  :general
  ("M-o" 'other-window)
  (:keymaps 'diff-mode-map
            "M-o" nil)
  :custom
  ;;;; Prefer vertical splits over horizontal ones
  (split-width-threshold 170)
  (split-height-threshold nil)

  (window-resize-pixelwise t)
  (window-sides-vertical t)

  (switch-to-buffer-obey-display-actions t) ; As per suggestion of Mastering Emacs
  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-alist
   `(;; Don't show
     ("\\*BibTeX validation errors\\*"
      ;; (display-buffer-reuse-mode-window display-buffer-no-window))
      (display-buffer-no-window)
      (allow-no-window . t))

     ;; Full frame

     ;; Same window
     ("\\*helpful *"
      (display-buffer-reuse-mode-window display-buffer-same-window))
     ("*Flycheck errors*"
      (display-buffer-reuse-mode-window display-buffer-same-window))
     ("\\*devdocs\\*"
      (display-buffer-reuse-mode-window display-buffer-same-window))
     ((major-mode . diff-mode)
      (display-buffer-same-window))
     ((or . ((major-mode . vc-git-log-view-mode)
             (major-mode . vc-annotate-mode)
             (major-mode . vc-git-region-history-mode)))
      (display-buffer-same-window))
     ("OrgMimeMailBody"
      (display-buffer-same-window))
     ((major-mode . denote-interface-mode)
      (display-buffer-same-window))

     ;; To the left
     ("\\*Faces\\*"
      (kb/select-buffer-in-side-window)
      (window-width . 0.33)
      (side . left)
      (slot . 2)
      (window-parameters . ((no-other-window . t))))

     ;; To the right
     ("\\*Help\\*"
      (display-buffer-in-previous-window))
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
      (kb/select-buffer-in-side-window)
      (window-height . 0.36)
      (side . top)
      (slot . 1))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
      (kb/select-buffer-in-side-window)
      (window-height . 0.3)
      (side . top)
      (slot . 2))
     ("\\*\\(?:Org Select\\|Agenda Commands\\)\\*"
      (kb/select-buffer-in-side-window)
      (window-height . fit-window-to-buffer)
      (side . top)
      (slot . -2)
      (preserve-size . (nil . t))
      (window-parameters . ((mode-line-format . none))))

     ;; To the bottom
     ("\\*Flycheck errors\\*"
      (display-buffer-in-side-window)
      (window-height . 0.33))
     ("\\(?:[Oo]utput\\)\\*"
      (display-buffer-in-side-window)
      (window-height . fit-window-to-buffer)
      (side . bottom)
      (slot . -4))
     ("\\*Embark Actions\\*"
      (display-buffer-at-bottom)
      (window-height . fit-window-to-buffer)
      (window-parameters . ((no-other-window . t))))
     ((major-mode . dap-ui-repl-mode)
      (display-buffer-at-bottom)
      (window-height . 12)
      (window-parameters . ((mode-line-format . none))))
     ("^\\*eldoc"
      ((lambda (buffer alist)
         (let ((window (display-buffer-at-bottom buffer alist)))
           (select-window window))))
      (window-height . shrink-window-if-larger-than-buffer)
      (window-parameters . ((mode-line-format . none))))

     ;; Below current window
     ("\\*\\(Calendar\\|Org Select\\).*"
      (display-buffer-below-selected)
      (window-height . fit-window-to-buffer))
     ("\\*\\(Embark\\)?.*Completions.*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none))))
     ("\\*compilation\\*"
      (display-buffer-in-side-window)
      (side . bottom))
     ("\\*\\(I?Python3\\|Python3\\)\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.27))
     ("\\*Racket REPL"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.35))
     ((major-mode . rustic-cargo-run-mode)
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.35))
     ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
      (display-buffer-reuse-mode-window display-buffer-below-selected)
      (window-height . 20)
      (dedicated . t)
      (preserve-size . (t . t)))
     ("\\*vc-log\\*"
      (display-buffer-reuse-mode-window display-buffer-below-selected)
      (dedicated . t))))
  :init
  ;; Helper functions for `display-buffer-alist'
  (defun kb/select-buffer-in-side-window (buffer alist)
    "Display buffer in a side window then select it."
    (let ((window (display-buffer-in-side-window buffer alist)))
      (select-window window))))

;;;;; Eyebrowse
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

;;;;; Tab-bar
(use-package tab-bar
  :ensure nil
  :general (:keymaps 'tab-prefix-map
                     "w" 'tab-bar-move-window-to-tab)
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice 'clone)
  (tab-bar-close-last-tab-choice 'delete-frame)
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

;;;;; Project-tab-groups
(use-package project-tab-groups
  :disabled                             ; Intrusive for my use
  :init
  (project-tab-groups-mode))

;;;;; Ace-window
(use-package ace-window
  :general (:prefix "C-c"
                    "w" 'ace-window
                    "W" 'ace-swap-window)
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

;;;;; Popper
;; "Tame ephemeral windows"
(use-package popper
  :general ("M-`" 'popper-toggle-latest
            "C-`" 'popper-cycle
            "C-M-s-`" 'popper-kill-latest-popup
            "C-~" 'popper-toggle-type)
  :custom
  (popper-reference-buffers
   '(;; General
     "^\\*Messages\\*"
     "^\\*Warnings\\*"
     "^\\*Backtrace\\*"
     comint-mode

     ;; Coding
     "[Oo]utput\\*"
     "\\*Shell Command Output\\*"
     "\\*Async Shell Command\\*"
     "^\\*Compile-Log\\*$"

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

;;;;; Switchy-window
;; `other-window' by most recently used
(use-package switchy-window
  :general (:keymaps 'switchy-window-minor-mode-map
                     [remap other-window] #'switchy-window)
  :init
  (switchy-window-minor-mode)
  (defvar-keymap switchy-window-repeat-map
    :doc "Keymap to repeat `switchy-window'.  Used in `repeat-mode'."
    :repeat t
    "o" #'switchy-window)

  (defun kb/switchy-window (&optional arg)
    "Switch to other windows in most-recently-used order.
If prefix ARG is given, use least-recently-used order.

If the time between consecutive invocations is smaller than
`switchy-window-delay' seconds, selects one after the other window in
LRU order and cycles when all windows have been visited.  If
`switchy-window-delay' has passed, the current switching cycle ends and
the now selected window gets its tick updated (a kind of
timestamp)."
    (interactive)

    (unless switchy-window-minor-mode
      (user-error "switchy-window requires `switchy-window-minor-mode' being active"))

    ;; Remove dead windows.
    (setq switchy-window--tick-alist (seq-filter
                                      (lambda (e)
                                        (and (or (not (window-parameter (car e) 'no-other-window))
                                                 ignore-window-parameters)
                                             (window-live-p (car e))))
                                      switchy-window--tick-alist))
    ;; Add windows never selected.
    (dolist (win (seq-filter (lambda (e) (or (not (window-parameter e 'no-other-window))
                                             ignore-window-parameters))
                             (window-list (selected-frame))))
      (unless (assq win switchy-window--tick-alist)
        (setf (alist-get win switchy-window--tick-alist) 0)))

    ;; Ensure the current window is marked as visited.
    (setq switchy-window--visited-windows (cons (selected-window)
                                                switchy-window--visited-windows))

    (let ((win-entries (seq-filter
                        (lambda (e)
                          (let ((win (car e)))
                            (and (eq (window-frame win) (selected-frame))
                                 (or (minibuffer-window-active-p win)
                                     (not (eq win (minibuffer-window
                                                   (selected-frame)))))
                                 (not (memq win switchy-window--visited-windows)))))
                        switchy-window--tick-alist)))
      (if win-entries
          (when-let ((win (car (seq-reduce (lambda (x e)
                                             (if (and x (funcall (if arg #'< #'>)
                                                                 (cdr x) (cdr e)))
                                                 x
                                               e))
                                           win-entries nil))))
            (setq switchy-window--visited-windows
                  (cons win switchy-window--visited-windows))
            (select-window win))
        ;; Start a new cycle if we're not at the start already, i.e., we visited
        ;; just one (the current) window.
        (when (length> switchy-window--visited-windows 1)
          (setq switchy-window--visited-windows nil)
          (switchy-window)))))
  (advice-add 'switchy-window :override 'kb/switchy-window))

;;;; Buffers
;;;;; Bookmark
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1)                 ; Save bookmarks file every new entry
  (bookmark-watch-bookmark-file 'silent) ; Reload bookmarks file without query
  (bookmark-fringe-mark nil)             ; No value and intrusive oftentimes
  :config
  (bookmark-maybe-load-default-file))   ; Load bookmarks immediately for access

;;;;; Ibuffer
(use-package ibuffer
  :ensure nil
  :general
  ([remap list-buffers] 'ibuffer)
  (:keymaps 'ibuffer-mode-map
            "SPC" 'scroll-up-command)
  :custom
  (ibuffer-save-with-custom nil)
  (ibuffer-default-sorting-mode 'recency)
  (ibuffer-directory-abbrev-alist
   `((,(file-name-as-directory (expand-file-name kb/notes-dir)) . ,(propertize "Notes/" 'face 'bold))
     (,(expand-file-name user-emacs-directory) . ,(propertize "Emacs/" 'face 'bold))))
  (ibuffer-eliding-string "…")
  (ibuffer-jump-offer-only-visible-buffers t)
  (ibuffer-old-time 48)
  (ibuffer-expert nil)
  (ibuffer-show-empty-filter-groups t)
  (ibuffer-formats
   `((mark modified read-only locked
           " " (icon 2 2 :left :elide)
           " " (name 18 18 :left :elide)
           " " (size 9 -1 :right)
           " " (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " " (name 16 -1) " " filename)))
  (ibuffer-saved-filters
   '(("PDFs"
      (mode . pdf-view-mode))
     ("Emacs built-ins"
      (filename . "/usr/local/share/emacs"))))
  (ibuffer-filter-group-name-face '(:inherit (success bold)))
  (ibuffer-saved-filter-groups      ; NOTE 2024-02-11: Order of entries matters!
   `(("Basic"
      ("Help" ,(-flatten `(or ,(mapcar (lambda (mode) `(mode . ,mode)) ibuffer-help-buffer-modes))))
      ("Org" (directory . ,org-directory))
      ("Libraries" ,(-flatten `(or ,(mapcar (lambda (dir) `(directory . ,dir))
                                            (remove "/home/krisbalintona/.emacs.d/recipes"
                                                    load-path)))))
      ("Emacs" (directory . ,(expand-file-name user-emacs-directory))))))
  :config
  ;; The following columns are taken from Doom Emacs.
  ;; Display buffer icons on GUI
  (define-ibuffer-column icon (:name "   ")
    (let ((icon (if (and (buffer-file-name)
                         (nerd-icons-auto-mode-match?))
                    (nerd-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                  (nerd-icons-icon-for-mode major-mode :v-adjust -0.05))))
      (if (symbolp icon)
          (setq icon (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-dsilver :height 0.8 :v-adjust 0.0))
        icon)))

  ;; Redefine size column to display human readable size
  (define-ibuffer-column size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

;;;;; Burly
(use-package burly
  :demand
  ;; :ensure (:depth 1
  ;;                 :fetcher github
  ;;                 ;; NOTE 2023-07-15: See
  ;;                 ;; https://github.com/alphapapa/burly.el/issues/28 for details on
  ;;                 ;; this branch
  ;;                 :repo "alphapapa/burly.el"
  ;;                 :branch "wip/readablep"
  ;;                 :files ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
  ;;                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el")))
  :vc (:url "https://github.com/alphapapa/burly.el.git"
            :rev :newest
            :branch "wip/readablep")
  :config
  (burly-tabs-mode 1))

;;;;; Perfect-margin
;; Center the window contents via setting the margins
(use-package perfect-margin
  :demand
  :diminish
  :custom
  (fringes-outside-margins nil)
  (perfect-margin-visible-width 128)
  (perfect-margin-only-set-left-margin nil)
  (perfect-margin-disable-in-splittable-check t)
  (perfect-margin-ignore-modes
   '(exwm-mode doc-view-mode nov-mode pdf-view-mode))
  (perfect-margin-ignore-regexps
   '("^minibuf"))
  (perfect-margin-ignore-filters
   '(window-minibuffer-p
     (lambda (window)
       "Ignore if `olivetti-mode' is active."
       (with-selected-window window (bound-and-true-p olivetti-mode)))
     (lambda (window)
       "Ignore if a special buffer whose major mode isn't...
- `vc-dir-mode'"
       (with-selected-window window
         (and (string-match-p "^[[:space:]]*\\*" (buffer-name))
              (not (eq major-mode 'vc-dir-mode)))))))
  :config
  (perfect-margin-mode 1)

  ;; Additional mouse bindings for now wider margins. Taken from
  ;; https://github.com/mpwang/perfect-margin#additional-binding-on-margin-area
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll))))

;;;;; Centered-window
;; Center the window contents via setting the fringes
(use-package centered-window
  :disabled ; Prefer margins over fringes, since fringes tend to be used more by other packages
  :custom
  (fringes-outside-margins t) ; REVIEW 2024-02-25: Haven't tested if this is desirable yet
  (cwm-lighter nil)
  (cwm-centered-window-width 128)
  (cwm-ignore-buffer-predicates
   '(cwm-special-buffer-p
     (lambda (buf)
       "Ignore if `olivetti-mode' is active."
       (with-current-buffer buf (bound-and-true-p olivetti-mode)))
     (lambda (buf)
       "Ignore special buffers unless I like it."
       (with-current-buffer buf
         (let ((buf-name (buffer-name)))
           (not (derived-mode-p major-mode
                                '(exwm-mode doc-view-mode pdf-view-mode))))))))
  :config
  (centered-window-mode 1)

  ;; My own version
  (defun kb/window-splittable-p (window &optional horizontal)
    "Override for `window-splittable-p'.
Determine if WINDOW is splittable."
    (when (and (window-live-p window)
               (not (window-parameter window 'window-side)))
      (with-current-buffer (window-buffer window)
        (if horizontal
            (and (memq window-size-fixed '(nil height))
                 (numberp split-width-threshold)
                 (>= (if (bound-and-true-p centered-window-mode)
                         ;; Added this. Not sure if this is foolproof, since all
                         ;; it does is take into consideration the margins and
                         ;; fringes, but for now it's a sufficient approximation
                         (window-total-width window)
                       (window-width window))
                     (max split-width-threshold
                          (* 2 (max window-min-width 2)))))
          (and (memq window-size-fixed '(nil width))
               (numberp split-height-threshold)
               (>= (window-height window)
                   (max split-height-threshold
                        (* 2 (max window-min-height
                                  (if mode-line-format 2 1))))))))))
  (advice-add 'window-splittable-p :override #'kb/window-splittable-p))

;;;; Activities
(use-package activities
  ;; :ensure (:type git :host github :repo "alphapapa/activities.el")
  :vc (:rev :newest)
  :general (:prefix "C-c a"
                    "d" 'activities-define
                    "n" 'activities-new
                    "g" 'activities-revert
                    "s" 'activities-suspend
                    "k" 'activities-kill
                    "a" 'activities-resume
                    "b" 'activities-switch-buffer
                    "B" 'activities-switch
                    "l" 'activities-list)
  :custom
  (activities-kill-buffers t)
  :init
  (activities-mode 1)
  (activities-tabs-mode 1))

(provide 'buffers-and-windows-rcp)
;;; buffers-and-windows-rcp.el ends here
