;; -*- lexical-binding: t; -*-

(require 'color)

;;; Krisb-themes-ext
(use-package krisb-themes-ext
  :ensure nil
  :demand t
  :bind (("<f8>" . krisb-themes-ext-theme-switcher)
         ("<f9>" . krisb-themes-ext-toggle-window-transparency))
  :custom
  (krisb-themes-ext-light 'modus-operandi)
  (krisb-themes-ext-dark 'modus-vivendi)
  :config
  (krisb-theme-ext-enable-theme-time-of-day 8 19))

;;; Modus-themes
(use-package modus-themes
  :custom
  (modus-themes-custom-auto-reload t) ; Only applies to `customize-set-variable' and related
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-prompts '(semibold))
  (modus-themes-completions '((matches . (semibold))
                              (selection . (regular))))
  (modus-themes-headings '((0 . (semibold 1.1))
                           (1 . (regular 1.1))
                           (2 . (regular 1.1))
                           (agenda-date . (1.1))
                           (agenda-structure . (variable-pitch 1.2))
                           (t . (regular 1.0))))
  :config
  ;; Overrides
  (setopt modus-themes-common-palette-overrides
          `(;; Completion
            (fg-completion-match-0 fg-main) ; See (info "(modus-themes) Make headings more or less colorful")
            (fg-completion-match-1 fg-main)
            (fg-completion-match-2 fg-main)
            (fg-completion-match-3 fg-main)
            (bg-completion-match-0 bg-blue-intense)
            (bg-completion-match-1 bg-yellow-intense)
            (bg-completion-match-2 bg-cyan-intense)
            (bg-completion-match-3 bg-red-intense)

            ;; "Invisible" border in mode line. See (info "(modus-themes) Make the
            ;; mode line borderless")
            (border-mode-line-active bg-mode-line-active)
            (border-mode-line-inactive bg-mode-line-inactive)

            ;; Headings
            (fg-heading-1 red-faint)
            (fg-heading-6 rainbow-0)

            ;; Make the fringe invisible
            (fringe unspecified)

            ;; More noticeable block (e.g. org) backgrounds
            (bg-prose-block-contents bg-active-value)

            ;; Region
            (fg-region unspecified)
            (bg-region bg-ochre))
          modus-operandi-palette-overrides
          `(
            ;; I like `modus-*-tinted's mode line colors. I like to keep
            ;; `border-mode-line-active' and `border-mode-line-inactive'
            ;; "invisible" though
            (bg-mode-line-active        "#cab9b2")
            (fg-mode-line-active        "#000000")
            (bg-mode-line-inactive      "#dfd9cf")
            (fg-mode-line-inactive      "#585858")
            )
          modus-vivendi-palette-overrides
          `(
            ;; I like `modus-*-tinted's mode line colors. I like to keep
            ;; `border-mode-line-active' and `border-mode-line-inactive'
            ;; "invisible" though
            (bg-mode-line-active        "#484d67")
            (fg-mode-line-active        "#ffffff")
            (bg-mode-line-inactive      "#292d48")
            (fg-mode-line-inactive      "#969696")))

  ;; Set up essential faces
  (krisb-modus-themes-setup-faces
   "mode-line"
   (set-face-attribute 'mode-line-active nil
                       :background bg-mode-line-active
                       :box `( :line-width 3
                               :color ,bg-mode-line-active))

   (let ((bg-color (cond
                    ((eq theme krisb-themes-ext-dark)
                     (color-darken-name bg-mode-line-inactive 13))
                    ((eq theme krisb-themes-ext-light)
                     (color-lighten-name bg-mode-line-inactive 13)))))
     (set-face-attribute 'mode-line-inactive nil
                         :background bg-color
                         :box `( :line-width 3
                                 :color ,bg-color))))

  (krisb-modus-themes-setup-faces
   "cursor"
   (set-face-attribute 'cursor nil :background magenta-cooler))

  (krisb-modus-themes-setup-faces
   "fringe"
   (set-face-background 'fringe (face-attribute 'default :background)))

  (krisb-modus-themes-setup-faces
   "font-lock"
   ;; As described in (info "(modus-themes) DIY Measure color contrast"), I can
   ;; check for contrast by making sure the color contrast (relative luminance)
   ;; between the foreground and background color is at least 7:1.
   ;;
   ;; Like:
   ;;    (modus-themes-contrast (modus-themes-with-colors bg-main) (face-foreground 'font-lock-function-call-face))
   (cond
    ((string-match "^modus-operandi" (symbol-name theme))
     (set-face-attribute 'font-lock-function-call-face nil :foreground "#161BA1"))
    ((string-match "^modus-vivendi" (symbol-name theme))
     (set-face-attribute 'font-lock-function-call-face nil :foreground "#66B1F2")))))

;;; Provide
(provide 'krisb-themes)
