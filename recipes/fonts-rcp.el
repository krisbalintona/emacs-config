;;; fonts-rcp.el --- Emacs-wide font config          -*- lexical-binding: t; -*-

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

;; Packages and settings related to fonts and faces functionality generally
;; across Emacs.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;;;; Slimmer minibuffer and echo area faces
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

;;;; Mixed-pitch
;; Allow the same buffer to have both fixed- and variable-pitch
;; NOTE Changes the family and height of the default face to the family and
;; height of the variable-pitch face
(use-package mixed-pitch
  :diminish
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces
               '(highlight-indent-guides-character-face ; Highlight-indent-guides
                 ;; Ace-jump
                 ace-jump-face-foreground
                 ace-jump-face-background)))

;;;; Default-text-scale
;; Text-scale-mode but Emacs-wide
(use-package default-text-scale)

;;;; Emojify
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

;;;; Unicode-fonts
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
  ;; (add-hook 'elpaca-after-init-hook #'unicode-fonts-setup -100)
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

;;;; Ligature
;; Ligatures! See for configuration examples: https://github.com/j/wiki
(use-package ligature
  ;; :ensure (ligature :type git :host github :repo "mickeynp/ligature.el")
  :hook (window-setup . global-ligature-mode)
  :config
  ;; Enables simple HTML ligations for web-related major modes using the string
  ;; notation to create ligations
  (ligature-set-ligatures '(html-mode nxml-mode web-mode) '("<!--" "-->" "</>" "</" "/>" "://"))

  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures '(prog-mode conf-mode) '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                                   "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                                   "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                                   ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

;;;; Fontaine
;; Define then apply face presets
(use-package fontaine
  :demand
  :custom
  (fontaine-presets
   '((default)                          ; Use fallback values
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

      :bold-family nil
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
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default))

  ;; Persist the latest font preset when closing/starting Emacs and while
  ;; switching between themes.
  (fontaine-mode 1)

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

;;;; Show-font
;; Best font previewer
(use-package show-font)

;;;; All-the-icons
;; Provides a bunch of unicode icons which many other packages leverage
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.1))

;;;; All-the-icons-completion
;; Add `all-the-icons' icons to minibuffer completion candidates
(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode 1))

(provide 'fonts-rcp)
;;; fonts-rcp.el ends here
