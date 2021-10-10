;;; themes-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Here are all the themes that interest me. One is enabled and all the others
;; are disabled. I've also added my doom-modeline configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'fonts-rcp)

;;;; UI
;;;;; Hide-mode-line
;; Hide the modeline when you don't want to see it
(use-package hide-mode-line
  :commands hide-mode-line-mode
  )

;;;;; Transparency
(unless kb/linux-ubuntu
  (set-frame-parameter (selected-frame) 'alpha '(98 . 98))
  (add-to-list 'default-frame-alist '(alpha . (98 . 98)))
  )

;;;; Themes and toggling
;;;;; Install themes
;; (use-package atom-one-dark-theme :demand t)
;; (use-package apropospriate-theme :demand t)

(setq custom-theme-load-path load-path)
(require 'uninspiring-dark-theme)
(use-package modus-themes)

;;;;;; Make sure Modus theme font families are correct
(defun kb/proper-set-font-families ()
  "A makeshift solution to the Modus themes not having an in-built
variable to set font families."
  ;; (defvar uninspiring-dark-default "Iosevka Term SS04")
  ;; (defvar uninspiring-dark-fixed-pitch "Hack Nerd Font Mono")
  ;; (defvar uninspiring-dark-variable-pitch "LiterationSerif Nerd Font")
  (set-face-attribute 'default nil :inherit 'uninspiring-dark-default :font ,uninspiring-dark-default :height 136)
  (set-face-attribute 'variable-pitch nil :inherit 'uninspiring-dark-variable-pitch :font ,uninspiring-dark-fixed-pitch :height 140)
  (set-face-attribute 'fixed-pitch nil :inherit 'uninspiring-dark-fixed-pitch :font ,uninspiring-dark-variable-pitch :height 158)
  )
(advice-add 'kb/proper-load-theme-light :after #'kb/proper-set-font-families)

;;;;; Variable declarations
(defvar kb/themes-light 'modus-operandi
  "My chosen light theme.")
(defvar kb/themes-dark 'uninspiring-dark
  "My chosen dark theme.")

(defvar kb/themes-hooks nil
  "Hook that runs after the `kb/proper-load-theme-light' and
`kb/proper-load-theme-dark'.")

;;;;; Function definitions
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

;;;;; Theme switcher
(defun kb/theme-switcher ()
  "Switch between the light and dark themes specified in `kb/themes-list'."
  (interactive)
  (kb/ensure-themes-loaded)
  (let* ((current (car custom-enabled-themes)))
    (cond ((equal kb/themes-light current) (kb/proper-load-theme-dark))
          ((equal kb/themes-dark current) (kb/proper-load-theme-light))
          ))
  )
(general-define-key "<f6>" 'kb/theme-switcher)

;;;;; Load default theme
(kb/proper-load-theme-dark)

;;;; Modeline
;;;;; Doom-modeline
;; Sleek modeline from Doom Emacs
(use-package doom-modeline
  :hook (window-configuration-change . doom-modeline-refresh-font-width-cache) ; Prevent modeline from being cut off
  :ghook 'emacs-startup-hook
  :custom
  ;; Modeline settings
  (doom-modeline-window-width-limit fill-column) ; The limit of the window width.
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon (or(display-graphic-p) (server-running-p))) ; Show icons if in Emacs GUI or server
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(LaTeX-mode markdown-mode gfm-mode org-mode))
  (doom-modeline-mu4e nil) ; Requires `mu4e-alert' - flip this value
  (doom-modeline-percent-position nil)
  (doom-modeline-number-limit 99)
  (doom-modeline-vcs-max-length 28)
  (doom-modeline-lsp t)
  (doom-modeline-height 33)
  (doom-modeline-bar-width 2) ; Width (in number of columns) of window until information (on the right) starts to disappear
  (doom-modeline-window-width-limit 100) ; Width of the bar segment
  :config (require 'kb-doom-modeline-segments)
  )

;;;;; Time
;; Enable time in the mode-line
(use-package time
  :after doom-modeline
  :hook (window-setup . display-time-mode)
  :custom
  (display-time-format "%H:%M") ; Use 24hr format
  (display-time-default-load-average 1) ; Don't show load average along with time
  )

;;;;; Battery
;; Display batter percentage
(use-package battery
  :straight nil
  :ghook ('doom-modeline-mode-hook 'display-battery-mode)
  :custom
  (battery-load-critical 15)
  (battery-load-low 25)
  )

;;;; Display-line-numbers-mode
;; Show line numbers on the left fringe
(use-package display-line-numbers
  :ghook 'prog-mode-hook 'LaTeX-mode-hook
  ;; :ghook 'prog-mode-hook
  :gfhook 'column-number-mode ; Column number in modeline
  :general (kb/leader-keys
             "tl" '(display-line-numbers-mode :which-key "Line numbers"))
  :custom
  (display-line-numbers-type 'relative)
  )

;;; themes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'themes-rcp)
