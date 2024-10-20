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
  (modus-themes-prompts '(semibold))
  (modus-themes-completions '(((matches . (heavy))
                               (selection . (semibold)))))
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-headings '((t . (semibold))))
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
            (bg-prose-block-contents bg-active-value))
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

  ;; TODO 2024-10-19: Use `krisb-modus-themes-setup-faces'?
  (defun krisb-modus-themes-setup-base-faces (theme)
    "Set up common faces in FRAME."
    (set-face-attribute 'mode-line-active nil
                        :background (modus-themes-with-colors bg-mode-line-active)
                        :box `( :line-width 3
                                :color ,(modus-themes-with-colors bg-mode-line-active)))
    (let ((bg-color
           (if (eq (car custom-enabled-themes) krisb-themes-ext-dark)
               (color-darken-name (modus-themes-with-colors bg-mode-line-inactive) 13)
             (color-lighten-name (modus-themes-with-colors bg-mode-line-inactive) 13))))
      (set-face-attribute 'mode-line-inactive nil
                          :background bg-color
                          :box `( :line-width 3
                                  :color ,bg-color)))

    (modus-themes-with-colors
      (set-face-attribute 'cursor nil :background magenta-cooler))

    (set-face-background 'fringe (face-attribute 'default :background))
    ;; Note that the vertical border is distinct from the window divider when
    ;; `window-divider-mode' is enabled.
    (set-face-attribute 'vertical-border nil
                        :foreground (face-attribute 'default :background))

    ;; Set up font-lock faces.
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
      (set-face-attribute 'font-lock-function-call-face nil :foreground "#66B1F2"))))
  (add-hook 'enable-theme-functions #'krisb-modus-themes-setup-base-faces))

;;; Cursory
;; Global and local cursor presets
(use-package cursory
  :hook ((prog-mode . (lambda () (cursory-set-preset 'code :local)))
         ((org-mode markdown-mode) . (lambda () (cursory-set-preset 'prose :local))))
  :custom
  (cursory-latest-state-file (no-littering-expand-var-file-name "cursory/cursory-latest-state"))
  (cursory-presets
   '((code
      :cursor-type box
      :cursor-in-non-selected-windows hollow
      :blink-cursor-mode 1)
     (prose
      :cursor-type (bar . 2)
      :blink-cursor-mode -1
      :cursor-in-non-selected-windows (hbar . 3))
     (default)
     (t                                 ; The fallback values
      :cursor-type box
      :cursor-in-non-selected-windows hollow
      :blink-cursor-mode 1
      :blink-cursor-blinks 10
      :blink-cursor-delay 5
      :blink-cursor-interval 0.5)))
  :config
  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (when (file-exists-p cursory-latest-state-file)
    (cursory-set-preset (or (cursory-restore-latest-preset) 'default)))

  ;; Persist latest preset used across Emacs sessions
  (cursory-mode 1))


;;; Lin
;; Lin is a stylistic enhancement for Emacs' built-in `hl-line-mode'. It remaps
;; the `hl-line' face (or equivalent) buffer-locally to a style that is optimal
;; for major modes where line selection is the primary mode of interaction.
(use-package lin
  :custom
  (lin-face 'lin-cyan)
  :config
  (lin-global-mode 1)

  (add-to-list 'lin-mode-hooks 'LaTeX-mode-hook))

;;; Pulsar
;; Alternative to `pulse.el'
(use-package pulsar
  ;; TODO 2024-10-18: Redistribute to their respective use-package declarations
  :hook ((consult-after-jump . pulsar-recenter-top)
         (consult-after-jump . pulsar-reveal-entry)
         (imenu-after-jump . pulsar-recenter-top)
         (imenu-after-jump . pulsar-reveal-entry))
  :custom
  (pulsar-pulse t)
  (pulsar-face 'pulsar-red)
  (pulsar-delay 0.05)
  (pulsar-iterations 5)
  :config
  (pulsar-global-mode 1))

;;; Provide
(provide 'krisb-themes)
