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

;;; Font famlies
(defvar kb/themes-default-font
  "Iosevka Term SS04"
  "Font for default face.")

(defvar kb/themes-fixed-pitch-font
  ;; "Hack Nerd Font Mono"
  "Iosevka"
  "Font for fixed-pitch face.")

(defvar kb/themes-variable-pitch-font
  ;; "LiterationSerif Nerd Font"           ; Variable
  ;; "Latin Modern Mono Prop"              ; Monospace
  ;; "Sans Serif"
  "Open Sans"
  "Font for the variable-pitch face.")

(defvar kb/themes-mode-line-font
  ;; "JetBrainsMono Nerd Font"
  "Iosevka Aile"
  "Font for the mode line.")

;;; Install themes
(setq custom-theme-load-path load-path)

;;;; Dark
(use-package atom-one-dark-theme :disabled)
(use-package apropospriate-theme :disabled)
(use-package nano-theme)
(use-package mood-one-theme
  :disabled
  :init
  (mood-one-theme-arrow-fringe-bmp-enable)
  (eval-after-load 'flycheck #'mood-one-theme-flycheck-fringe-bmp-enable))
(require 'uninspiring-dark-theme)

;;;; Light
(use-package modus-themes
  :hook ((modus-themes-after-load-theme . kb/themes-setup-base-faces)
         ((modus-themes-after-load-theme kb/themes) . kb/modus-themes-solaire-faces))
  :config
  ;; Also make sure these are set before `modus-themes-load-themes' is called
  (setq modus-themes-custom-auto-reload t ; only applies to `customize-set-variable' and related
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts t

        modus-themes-prompts '(semibold)
        modus-themes-completions '(((matches . (heavy))
                                    (selection . (semibold))))
        modus-themes-org-blocks 'tinted-background
        modus-themes-variable-pitch-ui nil

        modus-themes-headings
        '((t . (semibold))))

  ;; Overrides
  (setq modus-themes-common-palette-overrides
        `(;; Completion
          (fg-completion-match-0 fg-main) ; See (modus-themes) Make headings more or less colorful
          (fg-completion-match-1 fg-main)
          (fg-completion-match-2 fg-main)
          (fg-completion-match-3 fg-main)
          (bg-completion-match-0 bg-blue-intense)
          (bg-completion-match-1 bg-yellow-intense)
          (bg-completion-match-2 bg-cyan-intense)
          (bg-completion-match-3 bg-red-intense)

          ;; No border in mode line. See (modus-themes) Make the mode line borderless
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)

          ;; Headings
          (fg-heading-1 red-faint)
          (fg-heading-6 rainbow-0)))

  ;; Taken from (info "(modus-themes) Add support for solaire-mode")
  (defun kb/modus-themes-solaire-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       `(solaire-default-face ((,c :inherit default :background ,bg-dim :foreground ,fg-dim)))
       `(solaire-line-number-face ((,c :inherit solaire-default-face :foreground ,fg-line-number-inactive)))
       `(solaire-hl-line-face ((,c :background ,bg-active)))
       `(solaire-org-hide-face ((,c :background ,bg-dim :foreground ,bg-dim)))))))
(use-package solo-jazz-theme :disabled)
(use-package kaolin-themes  :disabled)
(elpaca-wait)

;;; Variable declarations
(defvar kb/themes-dark 'modus-vivendi
  "My chosen dark theme.")

(defvar kb/themes-light 'modus-operandi
  "My chosen light theme.")

(defvar kb/themes-hook nil
  "Hook that runs after the `kb/proper-load-theme-*'.")

;;; Function definitions
(defun kb/themes-setup-base-faces (&optional frame)
  "Set up the default, fixed-pitch, and variable-pitch faces in FRAME."
  (setq frame (or frame (selected-frame)))
  (select-frame frame)
  (set-face-attribute 'default nil
                      :family kb/themes-default-font
                      ;; 1920x1080 display
                      ;; :height 140)
                      ;; 2560x1600 display
                      ;; :height 186)
                      ;; 2560x1600 display, with DPI set to 118
                      :height 161)
  (set-face-attribute 'fixed-pitch nil
                      :family kb/themes-fixed-pitch-font
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family kb/themes-variable-pitch-font
                      :height 1.1)

  (set-face-attribute 'mode-line nil
                      :family kb/themes-mode-line-font
                      ;; :height 113)      ; JetBrainsMono Nerd Font
                      :height 135       ; Iosevka Aile
                      :box `(:line-width 4
                                         :color
                                         ,(face-background 'mode-line)
                                         :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :box `(:line-width 4
                                         :color ,(face-background 'mode-line-inactive)
                                         :style nil)
                      :inherit 'mode-line)

  (dolist (face (list 'tab-bar-tab
                      'tab-bar-tab-inactive
                      'tab-bar-tab-group-current
                      'tab-bar-tab-group-inactive))
    (set-face-attribute face nil
                        :family (face-attribute 'variable-pitch :family)))

  (set-face-background 'fringe (face-attribute 'default :background))
  ;; Note that the vertical border is distinct from the window divider when
  ;; `window-divider-mode' is enabled.
  (set-face-attribute 'vertical-border nil
                      :foreground (face-attribute 'default :background))

  (remove-hook 'after-make-frame-functions #'kb/themes-setup-base-faces))
(add-hook 'after-make-frame-functions #'kb/themes-setup-base-faces)
(add-hook 'kb/themes-hook #'kb/themes-setup-base-faces)

(defun kb/ensure-themes-loaded ()
  "Ensure that the themes in `kb/themes-list' are loaded."
  (unless (or (custom-theme-p kb/themes-dark)
              (custom-theme-p kb/themes-light))
    (load-theme kb/themes-dark t t)
    (load-theme kb/themes-light t t)))

(defun kb/proper-load-theme-light ()
  "Properly load `kb/theme-light' theme by disabling its dark counterpart as well.
Additionally, run `kb/themes-hook'."
  (interactive)
  (disable-theme kb/themes-dark)
  (load-theme kb/themes-light t)
  (run-hooks 'kb/themes-hook))

(defun kb/proper-load-theme-dark ()
  "Properly load `kb/theme-dark' theme by disabling its light counterpart as well.
Additionally, run `kb/themes-hook'."
  (interactive)
  (disable-theme kb/themes-light)
  (load-theme kb/themes-dark t)
  (run-hooks 'kb/themes-hook))

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
           (kb/proper-load-theme-light)))))
(define-key global-map (kbd "<f11>") 'kb/theme-switcher)

;;; Load appropriate theme based on time of day
(let ((hour (string-to-number (format-time-string "%H"))))
  ;; Dark theme between 7 PM or 8 AM
  (if (or (<= 19 hour) (<= hour 8))
      (kb/proper-load-theme-dark)
    (kb/proper-load-theme-light)))
(add-hook 'elpaca-after-init-hook #'kb/themes-setup-base-faces)
(add-hook 'server-after-make-frame-hook #'kb/themes-setup-base-faces)
(elpaca-wait)

;;; kb-themes.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-themes)
