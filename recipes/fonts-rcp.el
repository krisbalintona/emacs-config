;;; fonts-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to fonts and faces.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Builtin
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;;; All-the-icons
;; Provides a bunch of unicode icons which many other packages leverage
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.1)
  )

;;; Mixed-pitch
;; Allow the same buffer to have both fixed- and variable-pitch
;; NOTE Changes the family and height of the default face to the family and
;; height of the variable-pitch face
(use-package mixed-pitch
  :diminish
  :ghook 'text-mode-hook
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces
               '(highlight-indent-guides-character-face ; Highlight-indent-guides
                 ;; Ace-jump
                 ace-jump-face-foreground
                 ace-jump-face-background
                 ))
  )

;;; Default-text-scale
;; Text-scale-mode but Emacs-wide
(use-package default-text-scale)

;;; Emojify
(use-package emojify
  :hook (after-init . global-emojify-mode)
  :custom
  ;; See
  ;; https://github.com/iqbalansari/emacs-emojify#displaying-composed-text-as-emojis
  ;; for a description of this setting
  (emojify-composed-text-p t)
  ;; Check out https://ianyepan.github.io/posts/emacs-emojis/
  (emojify-emoji-styles '(unicode))
  ;; (emojify-emoji-styles 'unicode)
  (emojify-display-style 'unicode))

;;; Unicode-fonts
;; NOTE 2022-01-24: See https://github.com/rolandwalker/unicode-fonts#testing
;; for how to test for its success. Also see the very recommended font
;; installations in the same README. Notably, the following are the listed
;; bare-minimum fonts:
;; DejaVu Sans
;; DejaVu Sans Mono
;; Quivira
;; Symbola
;; Noto Sans
;; Noto Sans Symbols
(use-package unicode-fonts
  :demand
  ;; OPTIMIZE 2023-01-08: Breaks emojis???!!! Perhaps a clue will be found in
  ;; `unicode-fonts-block-font-mapping'
  :init
  ;; (add-hook 'after-init-hook #'unicode-fonts-setup -100)
  :config
  ;; See https://ianyepan.github.io/posts/emacs-emojis/
  (set-fontset-font
   t 'symbol (font-spec
              :family (cond
                       ;; FIXME 2023-01-10: Right now I don't install the Noto
                       ;; Color Emoji system package because, though it works in
                       ;; Emacs, it ruins Firefox's fonts (i.e. much too wide).
                       ;; Instead, I use the Apple Emoji font (though I don't
                       ;; prefer its appearance over Noto Emojis)
                       ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                       ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
                       ((member "NotoEmoji Nerd Font" (font-family-list)) "NotoEmoji Nerd Font")
                       ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
                       ((member "Symbola" (font-family-list)) "Symbola")))
   nil 'prepend))

;;; Ligature
;; Ligatures! See for configuration examples: https://github.com/j/wiki
(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :hook (window-setup . global-ligature-mode)
  :config
  ;; Enables simple HTML ligations for web-related major modes using the string
  ;; notation to create ligations
  (ligature-set-ligatures '(html-mode nxml-mode web-mode) '("<!--" "-->" "</>" "</" "/>" "://"))

  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

;;; fonts-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'fonts-rcp)
