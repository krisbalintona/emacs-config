;;; Fontaine
;; Define then apply face presets
(use-package fontaine
  :demand t
  :custom
  (fontaine-latest-state-file (no-littering-expand-var-file-name "fontaine/fontaine-latest-state.eld"))
  (fontaine-presets
   '((default-wsl2
      :default-height 180
      :inherit iosevka-variants)
     (iosevka-variants
      ;; NOTE 2025-04-14: On Arch Linux, Iosevka fonts have associated packages
      ;; for each variant in the AUR (though not necessarily the Nerd Fonts
      ;; versions).
      :default-family "Iosevka SS 11 Nerd Font" ; 2025-04-14: Must be a bug that there is a space between "SS" and "11" in the font name
      :fixed-pitch-family "Iosevka Nerd Font"
      :mode-line-active-family "Iosevka Aile Nerd Font"
      :mode-line-inactive-family "Iosevka Aile Nerd Font")
     ;; Below are the shared fallback properties. I leave them there also as
     ;; reference for all possible properties
     (t
      ;; Alternatives:
      :default-family "IosevkaTermSS04 Nerd Font"
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
      :fixed-pitch-height nil

      :fixed-pitch-serif-family nil
      :fixed-pitch-serif-weight nil
      :fixed-pitch-serif-slant nil
      :fixed-pitch-serif-width nil
      :fixed-pitch-serif-height nil

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

      :mode-line-active-family "JetBrainsMono Nerd Font"
      :mode-line-active-weight nil
      :mode-line-active-slant nil
      :mode-line-active-width nil
      :mode-line-active-height 0.93

      :mode-line-inactive-family "JetBrainsMono Nerd Font"
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
      :line-number-height nil

      :tab-bar-family "Overpass Nerd Font"
      :tab-bar-weight nil
      :tab-bar-slant nil
      :tab-bar-width nil
      :tab-bar-height 0.93

      :tab-line-family nil
      :tab-line-weight nil
      :tab-line-slant nil
      :tab-line-width nil
      :tab-line-height nil


      :bold-slant nil
      :bold-weight bold
      :bold-width nil
      :bold-height nil

      :italic-family nil
      :italic-weight nil
      :italic-slant italic
      :italic-width nil
      :italic-height nil

      :line-spacing nil)))
  :config
  ;; 2025-04-14: I manually create the parent directory if it doesn't already
  ;; exist; this is not yet implemented upstream, so I do it manually here for
  ;; fresh installs of Emacs.
  (make-directory (file-name-directory fontaine-latest-state-file) t)

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

;;; Ligature
;; Ligatures!  Be aware that this differs from `prettify-symbols-mode' because
;; ligatures are provided by and must be supported by the particular font.  See
;; for configuration examples: https://github.com/j/wiki
(use-package ligature
  :hook (window-setup . global-ligature-mode)
  :config
  ;; Enables simple HTML ligations for web-related major modes using the string
  ;; notation to create ligations
  (ligature-set-ligatures
   '(html-mode nxml-mode web-mode)
   '("<!--" "-->" "</>" "</" "/>" "://"))

  ;; Enable all Iosevka ligatures in programming modes.  Taken from
  ;; https://github.com/mickeynp/ligature.el/wiki#iosevka
  (ligature-set-ligatures
   '(prog-mode conf-mode)
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
     "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
     "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
     ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

;;; Astute.el
;; Redisplay typographical punctuation (e.g., em-dashes as "—" and en-dashes as
;; "–")
(use-package astute
  :hook (text-mode . astute-mode)
  :custom
  (astute-lighter "")
  (astute-prefix-single-quote-exceptions
   '("bout"
     "em"
     "n'"
     "cause"
     "round"
     "twas"
     "tis")))

;;; Prettify-symbols-mode
(setopt prettify-symbols-unprettify-at-point 'right-edge)

;; Org-mode
(defun krisb-prettify-symbols--org-mode-setup ()
  "Set up pretty symbols in `org-mode'."
  (add-to-list 'prettify-symbols-alist '("->" . ?→))
  (add-to-list 'prettify-symbols-alist '("<-" . ?←)))
(add-hook 'org-mode-hook #'krisb-prettify-symbols--org-mode-setup)

;; Prog-mode
(defun krisb-prettify-symbols--prog-mode-setup ()
  "Set up pretty symbols in `prog-mode'."
  (add-to-list 'prettify-symbols-alist '("->" . ?→))
  (add-to-list 'prettify-symbols-alist '("<-" . ?←))
  (add-to-list 'prettify-symbols-alist '("<->" . ?↔))
  (add-to-list 'prettify-symbols-alist '("lambda" . ?λ)))
(add-hook 'prog-mode-hook #'krisb-prettify-symbols--prog-mode-setup)

;;; Show-font
;; Best font previewer
(use-package show-font)

;;; Default-text-scale
;; Text-scale-mode but Emacs-wide
(use-package default-text-scale)

;;; Provide
(provide 'krisb-fonts)
