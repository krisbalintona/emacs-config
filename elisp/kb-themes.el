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
  :custom
  (modus-themes-mixed-fonts t)
  :config
  ;; Set foundational faces
  (set-face-attribute 'default nil :font uninspiring-dark-default :height 136)
  (set-face-attribute 'modus-themes-variable-pitch nil :font uninspiring-dark-variable-pitch :height 140)
  (set-face-attribute 'modus-themes-fixed-pitch nil :font uninspiring-dark-fixed-pitch :height 158))
(use-package solo-jazz-theme)
(use-package kaolin-themes)

;;; Variable declarations
;; (defvar kb/themes-dark 'nano-dark
;;   "My chosen dark theme.")
(defvar kb/themes-dark 'uninspiring-dark
  "My chosen dark theme.")
(defvar kb/themes-light 'kaolin-light
  "My chosen light theme.")

(defvar kb/themes-hooks nil
  "Hook that runs after the `kb/proper-load-theme-light' and
`kb/proper-load-theme-dark'.")

;;; Function definitions
(defun kb/ensure-themes-loaded ()
  "Ensure that the themes in `kb/themes-list' are loaded."
  (unless (or (custom-theme-p kb/themes-dark)
              (custom-theme-p kb/themes-light))
    (load-theme kb/themes-dark t t)
    (load-theme kb/themes-light t t))
  )
(defun kb/proper-load-theme-light ()
  "Properly load `kb/theme-light' theme by disabling its dark counterpart as well.
Additionally, run `kb/themes-hooks'."
  (interactive)
  (disable-theme kb/themes-dark)
  (load-theme kb/themes-light t)
  (run-hooks 'kb/themes-hooks)
  )
(defun kb/proper-load-theme-dark ()
  "Properly load `kb/theme-dark' theme by disabling its light counterpart as well.
Additionally, run `kb/themes-hooks'."
  (interactive)
  (disable-theme kb/themes-light)
  (load-theme kb/themes-dark t)
  (run-hooks 'kb/themes-hooks)
  )

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
