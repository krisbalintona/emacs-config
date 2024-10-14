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
  :hook (on-first-buffer . winner-mode)
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo))
  :custom
  (winner-dont-bind-my-keys t) ; Don't bind keys because I bind them myself
  (winner-boring-buffers '("*Completions*")))

;;;;; Windmove
(use-package windmove
  :ensure nil
  :hook (on-first-input . windmove-mode)
  :bind
  ( :map windmove-mode-map
    ("C-M-s-h" . windmove-left)
    ("C-M-s-j" . windmove-down)
    ("C-M-s-k" . windmove-up)
    ("C-M-s-l" . windmove-right)
    ("C-M-s-<left>" . windmove-left)
    ("C-M-s-<down>" . windmove-down)
    ("C-M-s-<up>" . windmove-up)
    ("C-M-s-<right>" . windmove-right)
    ("C-M-s-H" . windmove-swap-states-left)
    ("C-M-s-J" . windmove-swap-states-down)
    ("C-M-s-K" . windmove-swap-states-up)
    ("C-M-s-L" . windmove-swap-states-right)))

;;;;; Transpose-frame
;; Rotate window configuration
(use-package transpose-frame
  :bind
  (("C-M-s-r" . rotate-frame-clockwise)
   ("C-M-s-R" . rotate-frame-anticlockwise)))

;;;;; Window
(use-package window
  :ensure nil
  :bind* ("M-o" . other-window)
  :bind (([remap other-window] . kb/other-window-alternating)
         :repeat-map other-window-repeat-map
         ("o" . kb/other-window-alternating))
  :custom
  (split-width-threshold (ceiling (/ (frame-width) 2.0)))
  (split-height-threshold 80)
  (window-sides-vertical t)
  (window-resize-pixelwise t)
  (window-combination-resize t) ; Allow to resize existing windows when splitting?
  (fit-window-to-buffer-horizontally t)

  (switch-to-buffer-obey-display-actions t) ; As per suggestion of Mastering Emacs
  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-alist
   `(;; Don't show
     ("\\*BibTeX validation errors\\*"
      (display-buffer-no-window)
      (allow-no-window . t))

     ;; Same window
     ("\\*devdocs\\*"
      (display-buffer-reuse-mode-window display-buffer-same-window))
     ((major-mode . diff-mode)
      (display-buffer-same-window))
     ((or . ((major-mode . vc-dir-mode)
             (major-mode . vc-git-log-view-mode)
             (major-mode . vc-annotate-mode)
             (major-mode . vc-git-region-history-mode)))
      (display-buffer-same-window))
     ("OrgMimeMailBody"
      (display-buffer-same-window))
     ((major-mode . denote-interface-mode)
      (display-buffer-same-window))

     ;; To the left
     ("\\*Faces\\*"
      (display-buffer-in-side-window)
      (side . left)
      (post-command-select-window . t))

     ;; To the top
     (,(rx (literal messages-buffer-name))
      (display-buffer-in-side-window)
      (window-height . 0.36)
      (side . top)
      (slot . 1)
      (post-command-select-window . t))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.3)
      (side . top)
      (slot . 2)
      (post-command-select-window . t))
     ("\\*\\(?:Org Select\\|Agenda Commands\\)\\*"
      (display-buffer-in-side-window)
      (window-height . fit-window-to-buffer)
      (side . top)
      (slot . -2)
      (preserve-size . (nil . t))
      (window-parameters . ((mode-line-format . none)))
      (post-command-select-window . t))

     ;; To the bottom
     ("\\(?:[Oo]utput\\)\\*"
      (display-buffer-in-side-window)
      (window-height . shrink-window-if-larger-than-buffer)
      (side . bottom)
      (slot . -4))
     ("\\*Embark Actions\\*"
      (display-buffer-at-bottom)
      (window-height . fit-window-to-buffer)
      (window-parameters . ((no-other-window . t))))
     ("^\\*eldoc"
      (display-buffer-at-bottom)
      (post-command-select-window . t)
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
      (dedicated . t))
     ((major-mode . help-mode)
      (display-buffer-reuse-window display-buffer-pop-up-window display-buffer-below-selected)
      (window-height . shrink-window-if-larger-than-buffer))

     ;; Pop up
     ((or ,(rx (literal "*Man ") num (literal " "))
          (major-mode . Man-mode))
      (display-buffer-reuse-window display-buffer-pop-up-window)
      (post-command-select-window . t))))
  :config
  ;; Fixed version from
  ;; https://karthinks.com/software/emacs-window-management-almanac/#other-window-alternating
  (defalias 'kb/other-window-alternating
    (let ((direction 1))
      (lambda (&optional arg)
        "Call `other-window', switching directions each time."
        (interactive "p")
        (if (equal last-command 'kb/other-window-alternating)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1))))))))

;; Below selected
(with-eval-after-load 'xref
  (add-to-list 'display-buffer-alist
               `((or (major-mode . xref--xref-buffer-mode)
                     (,(rx (literal xref-buffer-name))))
                 (display-buffer-below-selected display-buffer-at-bottom)
                 (window-height . 0.25))))

;;;;; Eyebrowse
;; Provide a simple way to have workspaces
(use-package eyebrowse
  :disabled                             ; Testing out `tab-bar'
  :demand
  :bind
  ( :map eyebrowse-mode-map
    ("M-1" . eyebrowse-switch-to-window-config-1)
    ("M-2" . eyebrowse-switch-to-window-config-2)
    ("M-3" . eyebrowse-switch-to-window-config-3)
    ("M-4" . eyebrowse-switch-to-window-config-4)
    ("M-5" . eyebrowse-switch-to-window-config-5)
    ("M-6" . eyebrowse-switch-to-window-config-6)
    ("M-7" . eyebrowse-switch-to-window-config-7)
    ("M-8" . eyebrowse-switch-to-window-config-8)
    ("M-9" . eyebrowse-switch-to-window-config-9)
    ("M-0" . eyebrowse-switch-to-window-config-0)
    ("M-\\" . eyebrowse-last-window-config)
    ("M-[" . eyebrowse-prev-window-config)
    ("M-]" . eyebrowse-next-window-config)
    ((concat eyebrowse-keymap-prefix "d") . eyebrowse-close-window-config))
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
  :demand
  :bind (([remap winner-undo] . tab-bar-history-back)
         ([remap winner-redo] . tab-bar-history-forward)
         :map tab-prefix-map
         ("w" . tab-bar-move-window-to-tab))
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
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;;;;; Project-tab-groups
(use-package project-tab-groups
  :disabled                             ; Intrusive for my use
  :init
  (project-tab-groups-mode))

;;;;; Ace-window
(use-package ace-window
  :bind (("C-c w" . ace-window)
         ("C-c W" . kb/ace-window-prefix))
  :custom
  (aw-scope 'global)
  (aw-swap-invert t)
  (aw-background t)
  (aw-display-mode-overlay nil)
  (aw-dispatch-always t) ; Dispatch available even when less than three windows are open
  (aw-minibuffer-flag t)
  (aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?p))
  (aw-fair-aspect-ratio 3)
  :custom-face
  (aw-leading-char-face ((t (:height 3.0 :weight bold))))
  :config
  ;; Is not a defcustom, so use setq
  (setq aw-dispatch-alist
        '((?k aw-delete-window "Delete window")
          (?K delete-other-windows "Delete other windows")
          (?s aw-swap-window "Swap windows")
          (?m kb/aw-take-over-window "Go to window and delete current window")
          (?c aw-copy-window "Copy window")
          (?o aw-flip-window "Other window")
          (?v kb/ace-set-other-window "Set to other-scroll-window's window")
          (?b aw-switch-buffer-in-window "Switch to buffer in window")
          (?B aw-switch-buffer-other-window "Change buffer in window")
          (?2 aw-split-window-vert "Split vertically")
          (?3 aw-split-window-horz "Split horizontally")
          (?+ aw-split-window-fair "Split heuristically") ; See `aw-fair-aspect-ratio'
          (?? aw-show-dispatch-help)))

  ;; Taken from Karthink's config
  (defun kb/aw-take-over-window (window)
    "Move from current window to WINDOW.

Delete current window in the process."
    (let ((buf (current-buffer)))
      (if (one-window-p)
          (delete-frame)
        (delete-window))
      (aw-switch-to-window window)
      (switch-to-buffer buf)))

  ;; Taken from Karthink's config
  (defun kb/ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer. When
`switch-to-buffer-obey-display-actions' is non-nil, `switch-to-buffer'
commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))

  ;; Based off of similar code taken from
  ;; https://karthinks.com/software/emacs-window-management-almanac/#scroll-other-window--built-in
  (defun kb/ace-set-other-window (window)
    "Set WINDOW as the \"other window\" for the current one.
\"Other window\" is the window scrolled by `scroll-other-window' and
`scroll-other-window-down'."
    (setq-local other-window-scroll-buffer (window-buffer window)))

  ;; A useful macro for executing stuff in other windows. Taken from
  ;; https://karthinks.com/software/emacs-window-management-almanac/#with-other-window-an-elisp-helper
  (defmacro with-other-window (&rest body)
    "Execute forms in BODY in the other-window."
    `(unless (one-window-p)
       (with-selected-window (other-window-for-scrolling)
         ,@body))))

;;;;; Popper
;; "Tame ephemeral windows"
(use-package popper
  :bind
  (("M-`" . popper-toggle-latest)
   ("C-`" . popper-cycle)
   ("C-M-s-`" . popper-kill-latest-popup)
   ("M-~" . popper-toggle-type))
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
     "\\*vc-git"

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
     rustic-compilation-mode))
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
  :disabled t
  :hook (on-first-buffer . switchy-window-minor-mode)
  :bind
  ( :map switchy-window-minor-mode-map
    ([remap other-window] . switchy-window)
    :repeat-map kb/switchy-window-repeat-map
    ("o" . switchy-window))
  :config
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
  :hook (on-first-input . bookmark-maybe-load-default-file)
  :custom
  (bookmark-save-flag 1)                 ; Save bookmarks file every new entry
  (bookmark-watch-bookmark-file 'silent) ; Reload bookmarks file without query
  (bookmark-fringe-mark nil))            ; No value and intrusive oftentimes

;;;;; Ibuffer
(use-package ibuffer
  :ensure nil
  :bind
  (([remap list-buffers] . ibuffer)
   :map ibuffer-mode-map
   ("SPC" . scroll-up-command))
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
  ;; NOTE 2024-09-22: Gets overwritten by `all-the-icons-ibuffer-formats' when
  ;; `nerd-icons-ibuffer-mode'
  (ibuffer-formats
   '((mark modified read-only locked
           " " (name 18 18 :left :elide)
           " " (size 9 -1 :right)
           " " (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " " (name 16 -1) " " filename)))
  (ibuffer-saved-filters
   '(("Files, directories, and Info"
      (or (mode . dired-mode)
          (mode . Info-mode)
          (visiting-file)))
     ("Emacs built-ins"
      (filename . "/usr/local/share/emacs/"))
     ("Processes"
      (process))))
  (ibuffer-filter-group-name-face '(:inherit (success bold)))
  (ibuffer-saved-filter-groups ; NOTE 2024-02-11: Order of entries matters!
   `(("Basic"
      ("Help" ,(append '(or) (mapcar (lambda (mode) `(mode . ,mode)) ibuffer-help-buffer-modes)))
      ("Org" (directory . ,kb/org-dir))
      ("Libraries" ,(append '(or) (mapcar (lambda (dir) `(directory . ,dir))
                                          (remove "/home/krisbalintona/.emacs.d/recipes"
                                                  load-path))))
      ("Emacs" (directory . ,(expand-file-name user-emacs-directory))))))
  :config
  ;; Redefine size column to display human readable size
  (define-ibuffer-column size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

;;;;; Nerd-icons-ibuffer
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (nerd-icons-ibuffer-color-icon t)
  (nerd-icons-ibuffer-icon-size 0.97)
  (nerd-icons-ibuffer-formats
   `((mark modified read-only ,(if (>= emacs-major-version 26) 'locked "")
           ;; Here you may adjust by replacing :right with :center or :left
           ;; According to taste, if you want the icon further from the name
           " " (icon 2 2 :right)
           " " (name 18 18 :left :elide)
           " " (size 9 -1 :right)
           " " (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " " (name 16 -1) " " filename))))

;;;;; Burly
(use-package burly
  :hook (on-switch-buffer . burly-tabs-mode))

;;;;; Perfect-margin
;; Center the window contents via setting the margins
(use-package perfect-margin
  :disabled ; NOTE 2024-09-19: Went on to start `pinching-margins' because the code wasn't working and is bloated
  :diminish
  :hook (on-first-input . perfect-margin-mode)
  :custom
  (fringes-outside-margins nil)
  (perfect-margin-visible-width 128)
  (perfect-margin-only-set-left-margin nil)
  (perfect-margin-disable-in-splittable-check t)
  (perfect-margin-ignore-modes '(exwm-mode doc-view-mode pdf-view-mode))
  (perfect-margin-ignore-regexps '("^minibuf" "^[[:space:]]*\\*"))
  (perfect-margin-ignore-filters
   '(window-minibuffer-p
     (lambda (window)
       "Ignore if `olivetti-mode' is active."
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

  ;; REVIEW 2024-09-19: For some reason I find myself manually needing to make
  ;; this fix? Perhaps a change in Emacs broke perfect-margin?
  (defun kb/perfect-margin--init-window-margins ()
    "Calculate target window margins as if there is only one window on frame."
    ;; (let ((init-margin-width (round (max 0 (/ (- (frame-width) perfect-margin-visible-width) 2)))))
    (let ((init-margin-width (round (max 0 (/ (- (window-total-width) perfect-margin-visible-width) 2)))))
      (cons
       init-margin-width
       (if perfect-margin-only-set-left-margin 0 init-margin-width))))
  (advice-add 'perfect-margin--init-window-margins :override #'kb/perfect-margin--init-window-margins))

;;;;; Pinching-margins
(defcustom pinching-margins-visible-width 128
  "The visible width each window should be kept to"
  :type 'number)

(defcustom pinching-margins-ignore-predicates
  '(window-minibuffer-p
    (lambda (win)
      (with-selected-window win
        (member major-mode '(exwm-mode doc-view-mode))))
    (lambda (win)
      (cl-some (lambda (regexp) (string-match-p regexp (buffer-name (window-buffer win))))
               '("^[[:space:]]*\\*")))
    (lambda (win)
      (with-selected-window win (bound-and-true-p olivetti-mode))))
  "Predicates to exclude certain windows."
  :type '(repeat function))

(defcustom pinching-margins-force-predicates
  '((lambda (win)
      (with-selected-window win
        (member major-mode '())))
    (lambda (win)
      (cl-some (lambda (regexp) (string-match-p regexp (buffer-name (window-buffer win))))
               '("^\\*vc-"))))
  "Predicates to force including certain window."
  :type '(repeat function))

(defun pinching-margins--calculate-margins (win)
  (let ((width (round (max 0 (/ (- (window-total-width win) pinching-margins-visible-width) 2)))))
    (cons width width)))

(defun pinching-margins--apply-p (win)
  (with-current-buffer (window-buffer win)
    (or (run-hook-with-args-until-success 'pinching-margins-force-predicates win)
        (not (run-hook-with-args-until-success 'pinching-margins-ignore-predicates win)))))

(defun pinching-margins--set-win-margin (win)
  (with-selected-window win
    (when (pinching-margins--apply-p win)
      (let ((margins (pinching-margins--calculate-margins win)))
        (set-window-margins win (car margins) (cdr margins))))))

(defun pinching-margins--set-margins (&optional win)
  (cl-loop for win in (or win (window-list))
           do (pinching-margins--set-win-margin win)))

(defun pinching-margins--window-splittable-p-advice (orig-fun window &optional horizontal)
  "Advice for `window-splittable-p' to temporarily remove margins when called.
If WINDOW is not managed by pinched-margins or HORIZONTAL is nil, the
function will not modify the margins and directly call ORIG-FUN."
  (if (or (not horizontal)
          (not (pinching-margins--apply-p window)))
      (funcall orig-fun window horizontal)
    (let ((margins (window-margins window)))
      (prog2
          (set-window-margins window 0 0)
          (funcall orig-fun window horizontal)
        (set-window-margins window (car margins) (cdr margins))))))

(define-minor-mode pinching-margins-mode
  "Auto center windows."
  :init-value nil
  :global t
  (if pinching-margins-mode
      ;; Add hook and activate
      (progn
        (advice-add 'window-splittable-p :around #'pinching-margins--window-splittable-p-advice)
        (add-hook 'window-configuration-change-hook #'pinching-margins--set-margins)
        (add-hook 'window-size-change-functions #'pinching-margins--set-margins)
        (pinching-margins--set-margins))
    ;; Remove hook and restore margin
    (advice-remove 'window-splittable-p #'pinching-margins--window-splittable-p-advice)
    (remove-hook 'window-configuration-change-hook #'pinching-margins--set-margins)
    (remove-hook 'window-size-change-functions #'pinching-margins--set-margins)
    ;; FIXME 2024-09-19: This only restores the currently visible windows. E.g.
    ;; `tab-bar' windows that are elsewhere aren't affected.
    (dolist (window (window-list))
      (when (pinching-margins--apply-p window)
        (set-window-margins window 0 0)))))
(pinching-margins-mode 1)

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
  :bind
  (("C-c a d" . activities-define)
   ("C-c a a" . activities-resume)
   ("C-c a g" . activities-revert)
   ("C-c a b" . activities-switch-buffer)
   ("C-c a B" . activities-switch)
   ("C-c a s" . activities-suspend)
   ("C-c a k" . activities-kill)
   ("C-c a l" . activities-list))
  :preface
  (add-to-list 'package-pinned-packages '(activities . "gnu-elpa-devel"))
  :custom
  (activities-kill-buffers t)
  :config
  (activities-mode 1)
  (activities-tabs-mode 1))

;;;; Beframe
(use-package beframe
  :demand t
  :bind-keymap ("C-c B" . beframe-prefix-map)
  :custom
  (beframe-functions-in-frames nil)
  (beframe-rename-function #'beframe-rename-frame)
  :config
  (beframe-mode 1)

  ;; `consult-buffer' integration. Taken from (info "(beframe) Integration with Consult")
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defun my-beframe-buffer-names-sorted (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
     With optional argument FRAME, return the list of buffers of FRAME."
      (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'my-beframe-buffer-names-sorted
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source)))

(provide 'buffers-and-windows-rcp)
;;; buffers-and-windows-rcp.el ends here
