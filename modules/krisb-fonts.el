;;; Fontaine
;; Define then apply face presets
(use-package fontaine
  :demand
  :custom
  (fontaine-latest-state-file (no-littering-expand-var-file-name "fontaine/fontaine-latest-state.eld"))
  (fontaine-presets
   '((iosevka-variant
      :default-family "Iosevka SS11"
      :fixed-pitch-family "Iosevka")
     (default-wsl2
      :default-height 135
      :inherit iosevka-variant)
     (default                           ; Use fallback values
      :inherit iosevka-variant)
     ;; Below are the shared fallback properties. I leave them there also as
     ;; reference for all possible properties
     (t
      ;; Alternatives:
      :default-family "Iosevka Term SS04"
      :default-weight regular
      :default-slant normal
      :default-width normal
      :default-height 165

      ;; Alternatives
      ;; "Hack Nerd Font Mono"
      :fixed-pitch-family "Iosevka"
      :fixed-pitch-weight nil
      :fixed-pitch-slant nil
      :fixed-pitch-width nil
      :fixed-pitch-height 1.0

      :fixed-pitch-serif-family nil
      :fixed-pitch-serif-weight nil
      :fixed-pitch-serif-slant nil
      :fixed-pitch-serif-width nil
      :fixed-pitch-serif-height 1.0

      ;; Alternatives:
      ;; "LiterationSerif Nerd Font"       ; Variable
      ;; "Latin Modern Mono Prop"          ; Monospace
      ;; "Sans Serif"
      ;; "Open Sans" (1.1 height)
      :variable-pitch-family "Overpass Nerd Font Propo"
      :variable-pitch-weight nil
      :variable-pitch-slant nil
      :variable-pitch-width nil
      :variable-pitch-height 1.2

      ;; Alternatives:
      ;; "JetBrainsMono Nerd Font"
      :mode-line-active-family "Iosevka Aile"
      :mode-line-active-weight nil
      :mode-line-active-slant nil
      :mode-line-active-width nil
      :mode-line-active-height 0.93

      :mode-line-inactive-family "Iosevka Aile"
      :mode-line-inactive-weight nil
      :mode-line-inactive-slant nil
      :mode-line-inactive-width nil
      :mode-line-inactive-height 0.93

      :header-line-family nil
      :header-line-weight nil
      :header-line-slant nil
      :header-line-width nil
      :header-line-height 1.0

      :line-number-family nil
      :line-number-weight nil
      :line-number-slant nil
      :line-number-width nil
      :line-number-height 1.0

      :tab-bar-family "Overpass Nerd Font"
      :tab-bar-weight nil
      :tab-bar-slant nil
      :tab-bar-width nil
      :tab-bar-height 0.93

      :tab-line-family nil
      :tab-line-weight nil
      :tab-line-slant nil
      :tab-line-width nil
      :tab-line-height 1.0


      :bold-slant nil
      :bold-weight bold
      :bold-width nil
      :bold-height 1.0

      :italic-family nil
      :italic-weight nil
      :italic-slant italic
      :italic-width nil
      :italic-height 1.0

      :line-spacing nil)))
  :config
  ;; Set the last preset or fall back to desired style from `fontaine-presets'
  (when (file-exists-p fontaine-latest-state-file)
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default)))

  ;; Persist the latest font preset when closing/starting Emacs and while
  ;; switching between themes.
  (fontaine-mode 1)

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

;;; Mixed-pitch
;; Locally remap default face to variable-pitch.
(use-package mixed-pitch
  :diminish
  :custom
  ;; We don't want to set the height of variable-pitch faces because
  ;; non-variable-pitch faces will be "out of sync" with the height.  Therefore,
  ;; to have larger font sizes in these buffers, we have to remap those faces
  ;; manually and locally.
  (mixed-pitch-set-height nil)
  (mixed-pitch-variable-pitch-cursor nil))

;;; Provide
(provide 'krisb-fonts)
