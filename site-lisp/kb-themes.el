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

(defvar kb/themes-mode-line-font
  "JetBrainsMono Nerd Font"
  "Font for the mode line.")

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
  (setq modus-themes-custom-auto-reload t ; only applies to `customize-set-variable' and related
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts t

        modus-themes-prompts '(semibold)
        modus-themes-completions '(((matches . (heavy))
                                    (selection . (semibold))))
        modus-themes-region '(bg-only)
        modus-themes-org-blocks 'gray-background
        modus-themes-variable-pitch-ui nil
        )

  ;; Overrides
  (setq modus-themes-common-palette-overrides 
        '((fg-completion-match-0 fg-main) ; See 4.11.2.5 Make completion matches more or less colorful 
          (fg-completion-match-1 fg-main)
          (fg-completion-match-2 fg-main)
          (fg-completion-match-3 fg-main)
          (bg-completion-match-0 bg-blue-intense)
          (bg-completion-match-1 bg-yellow-intense)
          (bg-completion-match-2 bg-cyan-intense)
          (bg-completion-match-3 bg-red-intense))))
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
                      :height 1.1)

  (set-face-background 'fringe (face-attribute 'default :background))

  ;; TODO 2022-06-05: Kind of different heights between the active and inactive
  ;; mode line. Take a look at `modus-themes--mode-line-attrs', the modus-theme
  ;; background changes depending on `modus-themes-vivendi-colors' and
  ;; `modus-themes-operandi-colors'
  (set-face-attribute 'mode-line nil
                      :font kb/themes-mode-line-font
                      :height 113)
  (set-face-attribute 'mode-line-inactive nil
                      :inherit 'mode-line))
;; Set faces properly for the first frame
(general-add-hook 'server-after-make-frame-hook #'kb/themes-setup-base-faces t nil t)

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
(general-define-key "<f11>" 'kb/theme-switcher)

;;; Load appropriate theme based on time of day
(let ((hour (string-to-number (format-time-string "%H"))))
  ;; Dark theme between 6 PM or 6 AM
  (if (or (<= 18 hour) (>= 6))
      (kb/proper-load-theme-dark)
    (kb/proper-load-theme-light)))

;;; kb-themes.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-themes)
