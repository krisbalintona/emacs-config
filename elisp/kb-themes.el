;;; kb-themes.el --- Summary
;;
;;; Commentary:
;;
;; Code related to how I make commenting easier for myself. Heavily taken from
;; the built-in `comment-dwim' and Prot's `prot-comment-timestamp-keyword'
;; infrastructure. The idea of including a timestamp alongside keyword is
;; inspired from him.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Font famlies
(defvar kb/themes-default-font
  "Iosevka Term SS04"
  "Font for default face.")

(defvar kb/themes-fixed-pitch-font
  ;; "Hack Nerd Font Mono"
  "Iosevka Comfy"
  "Font for fixed-pitch face.")

(defvar kb/themes-variable-pitch-font
  ;; "LiterationSerif Nerd Font"           ; Variable
  ;; "Latin Modern Mono Prop"              ; Monospace
  ;; "Sans Serif"
  "Open Sans"
  "Font for the variable-pitch face.")

;;; Install themes
(setq custom-theme-load-path load-path)

;;;; Dark
(use-package atom-one-dark-theme)
(use-package apropospriate-theme)
(use-package nano-theme)
(use-package mood-one-theme
  ;; :after uninspiring-dark-theme
  :init
  (mood-one-theme-arrow-fringe-bmp-enable)
  (eval-after-load 'flycheck #'mood-one-theme-flycheck-fringe-bmp-enable)
  )
(require 'uninspiring-dark-theme)

;;;; Light
(use-package modus-themes
  :init
  ;; Also make sure these are set before `modus-themes-load-themes' is called
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-subtle-line-numbers nil
        modus-themes-deuteranopia nil
        modus-themes-fringes nil
        modus-themes-tabs-accented nil
        modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense'
        modus-themes-lang-checkers '(faint)

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', `padded'.
        modus-themes-mode-line '(borderless)

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax '(faint green-strings)

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line '(accented)

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match '(bold intense)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links '(neutral-underline italic)

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts '(intense bold)

        modus-themes-completions
        (quote ((matches . (extrabold background intense))
                (selection . (semibold accented intense))
                (popup . (accented))))

        modus-themes-mail-citations nil ; {nil,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(accented)

        ;; Options for `modus-themes-diffs': nil, 'desaturated,
        ;; 'bg-only, 'deuteranopia, 'fg-only-deuteranopia
        modus-themes-diffs nil

        modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        (quote ((header-block . (variable-pitch 1.2 semibold))
                (header-date . (grayscale workaholic bold-today 1.2))
                (event . (accented italic varied))
                (scheduled . uniform)
                (habit . traffic-light)))

        modus-themes-variable-pitch-ui nil
        ;; (modus-themes-headings
        ;;  (quote ((1 . (1.1))
        ;;          (2 . (1.15))
        ;;          (3 . (1))
        ;;          (t . (monochrome)))))
        )

  ;; Overrides
  (setq modus-themes-vivendi-color-overrides
        '((bg-main . "#1d2021")
          (fg-main . "#c2c2c2")))
  :config
  (modus-themes-load-themes)

  ;; Set foundational faces
  ;; (set-face-attribute 'default nil :font uninspiring-dark-default :height 136)
  ;; (set-face-attribute 'modus-themes-variable-pitch nil :font uninspiring-dark-variable-pitch :height 140)
  ;; (set-face-attribute 'modus-themes-fixed-pitch nil :font uninspiring-dark-fixed-pitch :height 158)
  )
(use-package solo-jazz-theme)
(use-package kaolin-themes)

;;; Variable declarations
(defvar kb/themes-dark 'modus-vivendi
  "My chosen dark theme.")

(defvar kb/themes-light 'modus-operandi
  "My chosen light theme.")

(defvar kb/themes-hooks '(kb/themes-setup-base-faces)
  "Hook that runs after the `kb/proper-load-theme-light' and
`kb/proper-load-theme-dark'.")

;;; Function definitions
(defun kb/themes-setup-base-faces ()
  "Set up the default, fixed-pitch, and variable-pitch
faces."
  (set-face-attribute 'default nil
                      :font kb/themes-default-font
                      :height 140)
  (set-face-attribute 'fixed-pitch nil
                      :font kb/themes-fixed-pitch-font
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :font kb/themes-variable-pitch-font
                      :height 1.1))

(defun kb/ensure-themes-loaded ()
  "Ensure that the themes in `kb/themes-list' are loaded."
  (unless (or (custom-theme-p kb/themes-dark)
              (custom-theme-p kb/themes-light))
    (load-theme kb/themes-dark t t)
    (load-theme kb/themes-light t t)))

(defun kb/proper-load-theme-light ()
  "Properly load `kb/theme-light' theme by disabling its dark counterpart as well.
Additionally, run `kb/themes-hooks'."
  (interactive)
  (disable-theme kb/themes-dark)
  (load-theme kb/themes-light t)
  (run-hooks 'kb/themes-hooks))

(defun kb/proper-load-theme-dark ()
  "Properly load `kb/theme-dark' theme by disabling its light counterpart as well.
Additionally, run `kb/themes-hooks'."
  (interactive)
  (disable-theme kb/themes-light)
  (load-theme kb/themes-dark t)
  (run-hooks 'kb/themes-hooks))

;;; Theme switcher
(defun kb/theme-switcher ()
  "Switch between the light and dark themes specified in `kb/themes-list'."
  (interactive)
  (kb/ensure-themes-loaded)
  ;; For this let clause to function, dark and light themes need to be in
  ;; `solaire-mode-themes-to-face-swap', assuming `solaire-mode' is active
  (let* ((current (car custom-enabled-themes)))
    (cond ((equal kb/themes-light current)
           (kb/proper-load-theme-dark))
          ((equal kb/themes-dark current)
           (kb/proper-load-theme-light))
          )))
(general-define-key "<f6>" 'kb/theme-switcher)

;;; Load default theme
(kb/proper-load-theme-dark)

;;; kb-themes.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-themes)
