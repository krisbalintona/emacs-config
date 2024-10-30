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
  :custom
  (display-buffer-alist
   `(;; Don't show
     ("\\*BibTeX validation errors\\*"
      (display-buffer-no-window)
      (allow-no-window . t))

     ;; Same window
     ("\\*devdocs\\*"
      (display-buffer-reuse-mode-window display-buffer-same-window))
     ((major-mode . denote-interface-mode)
      (display-buffer-same-window))

     ;; To the left
     ("\\*Faces\\*"
      (display-buffer-in-side-window)
      (side . left)
      (post-command-select-window . t))

     ;; To the top
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.3)
      (side . top)
      (slot . 2)
      (post-command-select-window . t))

     ;; To the bottom
     ("\\(?:[Oo]utput\\)\\*"
      (display-buffer-in-side-window)
      (window-height . shrink-window-if-larger-than-buffer)
      (side . bottom)
      (slot . -4))

     ;; Below current window
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

     ;; Pop up
     ((or ,(rx (literal "*Man ") num (literal " "))
          (major-mode . Man-mode))
      (display-buffer-reuse-window display-buffer-pop-up-window)
      (post-command-select-window . t)))))

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
    ("C-c M-w d" . eyebrowse-close-window-config))
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

;;;;; Project-tab-groups
(use-package project-tab-groups
  :disabled                             ; Intrusive for my use
  :init
  (project-tab-groups-mode))

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
;;;;; Ibuffer
(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-directory-abbrev-alist
   `((,(file-name-as-directory (expand-file-name krisb-notes-directory)) . ,(propertize "Notes/" 'face 'bold))
     (,(expand-file-name user-emacs-directory) . ,(propertize "Emacs/" 'face 'bold))))
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
  (ibuffer-saved-filter-groups ; NOTE 2024-02-11: Order of entries matters!
   `(("Basic"
      ("Help" ,(append '(or) (mapcar (lambda (mode) `(mode . ,mode)) ibuffer-help-buffer-modes)))
      ("Org" (directory . ,krisb-org-directory))
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
;; FIXME 2024-10-16: Still isn't working. I think it's a mysterious issue with
;; my config somewhere? See https://github.com/alphapapa/burly.el/issues/28
(use-package burly
  :commands kb/burly-bookmark-frame
  :hook (on-switch-buffer . burly-tabs-mode)
  :config
  (defun kb/burly-bookmark-frame (name)
    "Bookmark the current frame as NAME."
    (interactive
     (list (completing-read "Save Burly bookmark: " (burly-bookmark-names)
                            nil nil burly-bookmark-prefix)))
    (let ((record (list (cons 'url (burly-frames-url (list (selected-frame))))
                        (cons 'handler #'burly-bookmark-handler))))
      (bookmark-store name record nil))))

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

;;;; Beframe
(use-package beframe
  :demand t
  :bind-keymap ("C-c B" . beframe-prefix-map)
  :custom
  (beframe-functions-in-frames nil)
  (beframe-rename-function #'kb/beframe-rename-frame-by-count)
  :config
  (beframe-mode 1)

  (defun kb/beframe-rename-frame-by-count (frame &optional name)
    "Rename FRAME.
Meant to be the value of `beframe-rename-function'."
    (interactive
     (let ((select-frame (beframe--frame-prompt :force-even-if-one)))
       (list
        (beframe--frame-object select-frame)
        (when current-prefix-arg
          (read-string
           (format "Rename the frame now called `%s' to: "
                   select-frame)
           nil 'beframe--rename-frame-history select-frame)))))
    (modify-frame-parameters
     frame
     (list (cons 'name
                 (or name (number-to-string
                           (1+ (cl-position (selected-frame)
                                            (make-frame-names-alist) :key #'cdr :test #'eq))))))))
  (funcall 'kb/beframe-rename-frame-by-count (selected-frame))

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

;;;; Bufferlo
(use-package bufferlo
  :disabled t
  :demand t
  :config
  (bufferlo-mode 1)
  (bufferlo-anywhere-mode 1)

  ;; `consult-buffer' integration. From
  ;; https://github.com/florommel/bufferlo?tab=readme-ov-file#consult-integration
  (with-eval-after-load 'consult
    (defvar kb/bufferlo-consult--source-buffer
      `(:name "Other Buffers"
              :narrow   ?b
              ;; :hidden t          ; Can keep hidden unless filtered for with this
              :category buffer
              :face     consult-buffer
              :history  buffer-name-history
              :state    ,#'consult--buffer-state
              :items ,(lambda () (consult--buffer-query
                                  :predicate #'bufferlo-non-local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Non-local buffer candidate source for `consult-buffer'.")

    (defvar kb/bufferlo-consult--source-local-buffer
      `(:name "Local Buffers"
              :narrow   ?l
              :category buffer
              :face     consult-buffer
              :history  buffer-name-history
              :state    ,#'consult--buffer-state
              :default  t
              :items ,(lambda () (consult--buffer-query
                                  :predicate #'bufferlo-local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Local buffer candidate source for `consult-buffer'.")

    (add-to-list 'consult-buffer-sources 'kb/bufferlo-consult--source-local-buffer)
    (when (member 'consult--source-buffer consult-buffer-sources)
      (setf (car (member 'consult--source-buffer consult-buffer-sources)) 'kb/bufferlo-consult--source-buffer))))

(provide 'buffers-and-windows-rcp)
;;; buffers-and-windows-rcp.el ends here
