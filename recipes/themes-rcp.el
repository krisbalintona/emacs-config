;;; themes-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Here are all the themes that interest me. One is enabled and all the others
;; are disabled. I've also added my doom-modeline configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'fonts-rcp)
(require 'kb-themes)

;;; Fonts
;;;; Fontaine
;; Test faces without restart Emacs!
(use-package fontaine
  :init
  (setq fontaine-presets
        '((regular
           :default-family "Hack"
           :default-weight normal
           :default-height 100
           :fixed-pitch-family "Fira Code"
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :variable-pitch-family "Noto Sans"
           :variable-pitch-weight normal
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family "Source Code Pro"
           :italic-slant italic
           :line-spacing 1)
          (large
           :default-family "Iosevka"
           :default-weight normal
           :default-height 150
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :variable-pitch-family "FiraGO"
           :variable-pitch-weight normal
           :variable-pitch-height 1.05
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil ; use whatever the underlying face has
           :italic-slant italic
           :line-spacing 1)
          ;; Mine
          (test
           :default-family "Iosevka Term SS04"
           :default-weight normal
           :default-height 140
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-weight nil
           :fixed-pitch-height 1.0
           :variable-pitch-family "Open Sans"
           :variable-pitch-weight normal
           :variable-pitch-height 1.10
           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing 1)))
  )

;;;; Lighter minibuffer and echo area
;; Make minibuffer and echo fonts a little lighter. Taken from
;; https://www.reddit.com/r/emacs/comments/14q399t/comment/jqm6zr3/?utm_source=share&utm_medium=web2x&context=3
(dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                      " *Minibuf-1*" " *Echo Area 1*"))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (face-remap-add-relative 'bold :weight 'normal)
      (face-remap-add-relative 'default :weight 'light))))

(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (face-remap-add-relative 'bold :weight 'normal)
             (face-remap-add-relative 'default :weight 'light)))

;;; UI
;;;; Fringes
;; Default left fringe, 0 pixels for right fringe
(fringe-mode '(nil . 0))
;; Places the fringes outside the margins, closest to the frame edge. Useful for
;; `git-gutter-fringes'
;; (setq-default fringes-outside-margins t)

;;;; Hide-mode-line
;; Hide the modeline when you don't want to see it
(use-package hide-mode-line
  :commands hide-mode-line-mode)

;;;; Transparency toggle
;; Set the alpha-background parameter. Initially arose from a patch of Emacs
;; 29.0.50: https://github.com/TheVaffel/emacs, but was merged on January 30,
;; 2022:
;; https://github.com/emacs-mirror/emacs/commit/5c87d826201a5cae242ce5887a0aa7e24ad6f5ee
(unless kb/linux-ubuntu
  (set-frame-parameter nil 'alpha-background 100)
  (add-to-list 'default-frame-alist '(alpha-background . 100)))

(defun kb/toggle-window-transparency (&optional arg)
  "Toggle the value of `alpha-background'.

Toggles between 100 and 72 by default. Can choose which value to
change to if called with ARG."
  (interactive "P")
  (set-frame-parameter nil 'alpha-background
                       (if arg
                           (read-number "Change the transparency to which value (0-100)? ")
                         (cl-case (frame-parameter nil 'alpha-background)
                           (72 100)
                           (100 72)
                           (t 100)))))
(general-define-key "<f12>" 'kb/toggle-window-transparency)

;;;; Solaire-mode
;; Have "non-real" (by my own predicate) buffers and other faces swapped.
;; NOTE 2023-07-13: Make sure you set solaire's faces if my theme does not
;; already set them
(use-package solaire-mode
  :disabled
  ;; :hook ((after-change-major-mode dictionary-mode) . turn-on-solaire-mode)
  :custom
  (solaire-mode-real-buffer-fn
   '(lambda ()                  ; Real buffers have at least one of these properties:
      (or (buffer-file-name (buffer-base-buffer)) ; Connected to a file
          ;; (string-match "*[Ss]cratch" (buffer-name)) ; Is a scratch buffer
          (string-match "*Minimap*" (buffer-name)) ; Demap minimap
          )))
  :init
  ;; NOTE 2022-01-21: Enable `solaire-global-mode' if I'm okay swapping the
  ;; background faces which solaire remaps, i.e., non-real buffers dark and real
  ;; light to non-real light and real dark.
  (solaire-global-mode))

;;;; Lin
;; `hl-line-mode' but contextual based on mode (e.g. more visible)
(use-package lin
  :custom
  (lin-face 'kb/lin-face)
  (lin-mode-hooks
   '(prog-mode-hook
     dired-mode-hook
     elfeed-search-mode-hook
     git-rebase-mode-hook
     grep-mode-hook
     log-view-mode-hook
     magit-log-mode-hook
     mu4e-headers-mode
     notmuch-search-mode-hook
     notmuch-tree-mode-hook
     occur-mode-hook
     org-agenda-mode-hook
     proced-mode-hook
     tabulated-list-mode-hook
     ;; My hooks
     LaTeX-mode-hook
     org-mode-hook
     eww-mode-hook))
  :init
  (defface kb/lin-face
    '((default :foreground unspecified :underline nil :extend t)
      (((class color) (min-colors 88) (background light))
       :background "#c0e4ff")
      (((class color) (min-colors 88) (background dark))
       :background "#212428")
      (t :background "blue"))
    "My face for `lin-face'.")
  (lin-global-mode))

;;; Modeline
;;;; Nerd-icons
(use-package nerd-icons
  :demand)

;;;; Doom-modeline
;; Sleek modeline from Doom Emacs
(use-package doom-modeline
  :disabled
  :custom
  ;; Modeline settings
  (doom-modeline-window-width-limit fill-column) ; The limit of the window width.
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon (display-graphic-p)) ; Show icons if in Emacs GUI or server
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(LaTeX-mode markdown-mode gfm-mode org-mode))
  (doom-modeline-mu4e nil) ; Requires `mu4e-alert' - flip this value
  (doom-modeline-percent-position nil)
  (doom-modeline-number-limit 99)
  (doom-modeline-vcs-max-length 28)
  (doom-modeline-lsp t)
  (doom-modeline-height 33)
  (doom-modeline-bar-width 2) ; Width (in number of columns) of window until information (on the right) starts to disappear
  (doom-modeline-window-width-limit 100) ; Width of the bar segment
  :config (require 'kb-doom-modeline-segments))

;;;; Kb-mood-line
(use-package kb-mood-line
  :disabled
  :elpaca nil
  ;; :hook ((window-setup server-after-make-frame) . kb/mood-line-setup)
  :init
  (require 'doom-modeline)
  (use-package mood-line))

;;;; Minions
(use-package minions
  :ghook 'elpaca-after-init-hook
  :custom
  (minions-mode-line-lighter "…")
  (minions-mode-line-delimiters '("[" . "]"))
  (minions-prominent-modes
   '(kb/lisp-keyword-indent-mode tree-sitter-mode)))

;;;; Diminish
(use-package diminish
  :hook (elpaca-after-init . kb/diminish-setup)
  :init
  (defun kb/diminish-setup ()
    "Set up `diminish’ lighters for pre-loaded packages (packages that
are troublesome)."
    (with-eval-after-load 'subword
      (diminish 'subword-mode))
    (with-eval-after-load 'simple
      (diminish 'visual-line-mode))
    ;; (with-eval-after-load (diminish 'hs-minor-mode))
    (with-eval-after-load 'face-remap
      (diminish 'buffer-face-mode))
    (with-eval-after-load 'abbrev
      (diminish 'abbrev-mode))))

;;;; Mlscroll
;; Adds an interactive indicator for the view's position in the current buffer
;; to the modeline
(use-package mlscroll
  :disabled                         ; Messes with `mode-line-format-right-align'
  :hook (server-after-make-frame . mlscroll-mode)
  :init
  (mlscroll-mode))

;;;; Default mode line
(unless (bound-and-true-p mood-line-mode)
  (setq mode-line-defining-kbd-macro (propertize " Macro" 'face 'mode-line-emphasis)
        mode-line-compact 'long        ; Emacs 28
        mode-line-right-align-edge 'window
        mode-line-percent-position nil ; Don't show percentage of position in buffer
        mode-line-position-line-format '(" %l ")
        mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28

  (defvar kb/mode-line-modes
    (let ((recursive-edit-help-echo
           "Recursive edit, type M-C-c to get out"))
      (list (propertize "%[" 'help-echo recursive-edit-help-echo)
            "["                         ; Changed delimiter
            `(:propertize ("" mode-name)
                          help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                          mouse-face mode-line-highlight
                          local-map ,mode-line-major-mode-keymap)
            '("" mode-line-process)
            `(:propertize ("" minor-mode-alist)
                          mouse-face mode-line-highlight
                          help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                          local-map ,mode-line-minor-mode-keymap)
            (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                        'mouse-face 'mode-line-highlight
                        'local-map (make-mode-line-mouse-map
                                    'mouse-2 #'mode-line-widen))
            "]"                         ; Changed delimiter
            (propertize "%]" 'help-echo recursive-edit-help-echo)
            " "))
    "Mode line construct for displaying major and minor modes.")

  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  vc-mode " "
                  mode-line-buffer-identification
                  mode-line-position
                  (:eval
                   (when (bound-and-true-p anzu-mode) anzu--mode-line-format))
                  mode-line-format-right-align
                  mode-line-process
                  (:eval
                   (when (and (bound-and-true-p flymake-mode)
                              (mode-line-window-selected-p))
                     flymake-mode-line-format))
                  " "
                  (:eval
                   (when (mode-line-window-selected-p)
                     mode-line-misc-info))
                  (:eval kb/mode-line-modes)
                  mode-line-end-spaces)))

;;;; Time
;; Enable time in the mode-line
(use-package time
  :elpaca nil
  :custom
  (display-time-24hr-format t)
  (display-time-format "(%a %d, %R)")
  (display-time-interval 60)           ; Update every since if I'm using seconds
  (display-time-default-load-average nil) ; Don't show load average
  (world-clock-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Nicosia" "Nicosia (capital of Cyprus)")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Tokyo" "Tokyo")
     ("Asia/Shanghai" "Beijing")))
  :init
  (display-time-mode))

;;;; Battery
;; Display batter percentage
(use-package battery
  :elpaca nil
  :custom
  (battery-load-critical 15)
  (battery-load-low 25)
  (battery-mode-line-limit 95)
  ;; (battery-mode-line-format "%cmAh")
  ;; (battery-mode-line-format "  %p%%")
  (battery-mode-line-format "%b%p%% ")
  :init
  (display-battery-mode))

;;;; Display-line-numbers-mode
;; Show line numbers on the left fringe
(use-package display-line-numbers
  :elpaca nil
  :general (kb/toggle-keys
             "l" '(display-line-numbers-mode :wk "Line numbers"))
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width-start t)) ; Keep width consistent in buffer

;;; themes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'themes-rcp)
