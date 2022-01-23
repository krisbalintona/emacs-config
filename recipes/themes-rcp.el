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
(require 'kb-themes)

;;; UI
;;;; Hide-mode-line
;; Hide the modeline when you don't want to see it
(use-package hide-mode-line
  :commands hide-mode-line-mode
  :general ("<f8>" 'hide-mode-line-mode)
  )

;;;; Transparency
(defun kb/set-default-transparency ()
  "Sets default transparency of current and default frame alist."
  (unless kb/linux-ubuntu
    (set-frame-parameter (selected-frame) 'alpha '(98 .98))
    (add-to-list 'default-frame-alist   '(alpha . (98 .98)))
    ))

(defvar alpha-background-transparency t
  "Enable background transparency? Meant for the `alpha-background' parameter.")

(defun kb/toggle-transparency ()
  "Toggle transparency. Requires a patched version of Emacs found
here: https://github.com/TheVaffel/emacs"
  (interactive)
  (if alpha-background-transparency
      (progn (set-frame-parameter (selected-frame) 'alpha-background 75)
             (setq alpha-background-transparency nil)
             (kb/set-default-transparency)
             )
    (progn (set-frame-parameter (selected-frame) 'alpha-background 100)
           (setq alpha-background-transparency t)
           (kb/set-default-transparency))
    ))
(general-define-key "<f7>" 'kb/toggle-transparency)

;;;; Solaire-mode
;; Have "non-real" (by my own predicate) buffers and other faces swapped.
(use-package solaire-mode
  :hook ((window-state-change window-configuration-change) . turn-on-solaire-mode)
  :custom
  (solaire-mode-remap-alist
   '(;; Defaults
     (default . solaire-default-face)
     (hl-line . solaire-hl-line-face)
     (region . solaire-region-face)
     (org-hide . solaire-org-hide-face)
     (org-indent . solaire-org-hide-face)
     (linum . solaire-line-number-face)
     (line-number . solaire-line-number-face)
     (header-line . solaire-header-line-face)
     (mode-line . solaire-mode-line-face)
     (mode-line-active . solaire-mode-line-active-face)
     (mode-line-inactive . solaire-mode-line-inactive-face)
     (highlight-indentation-face . solaire-hl-line-face)
     (fringe . solaire-fringe-face)
     ))
  (solaire-mode-themes-to-face-swap `(,kb/themes-dark ,kb/themes-light))
  (solaire-mode-swap-alist
   '(;; Defaults
     (default . solaire-default-face)
     (hl-line . solaire-hl-line-face)
     (region . solaire-region-face)
     (org-hide . solaire-org-hide-face)
     (org-indent . solaire-org-hide-face)
     (linum . solaire-line-number-face)
     (line-number . solaire-line-number-face)
     (header-line . solaire-header-line-face)
     (mode-line . solaire-mode-line-face)
     (mode-line-active . solaire-mode-line-active-face)
     (mode-line-inactive . solaire-mode-line-inactive-face)
     (highlight-indentation-face . solaire-hl-line-face)
     (fringe . solaire-fringe-face)
     ))
  :init
  ;; NOTE 2022-01-21: Enable `solaire-global-mode' if I want to swap the
  ;; background faces which solaire remaps, e.g., non-real buffers dark and real
  ;; light to non-real light and real dark.
  ;; (solaire-global-mode)
  )

;;; Modeline
;;;; Doom-modeline
;; Sleek modeline from Doom Emacs
(use-package doom-modeline
  :hook (window-configuration-change . doom-modeline-refresh-font-width-cache) ; Prevent modeline from being cut off
  ;; :ghook 'emacs-startup-hook
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

;;;; Mood-line
;; Mode line which accompanies the mood-one theme
;; TODO 2022-01-22: Would like to remove all Doom Modeline dependency from this
;; modeline config one day, but it will take some time...
(use-package mood-line
  :ghook 'after-init-hook
  :config
  (require 'kb-mood-line)
  )

;;;; Time
;; Enable time in the mode-line
(use-package time
  ;; :hook (window-setup . display-time-mode)
  :custom
  (display-time-format "%H:%M:%S")     ; Use 24hr format with seconds
  (display-time-interval 1)            ; Update every since if I'm using seconds
  (display-time-default-load-average nil) ; Don't show load average
  (world-clock-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Nicosia" "Nicosia (capital of Cyprus)")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Tokyo" "Tokyo")
     ))
  :init
  (display-time-mode)
  )

;;;; Battery
;; Display batter percentage
(use-package battery
  :custom
  (battery-load-critical 15)
  (battery-load-low 25)
  (battery-mode-line-limit 100)
  ;; (battery-mode-line-format "%cmAh")
  (battery-mode-line-format "  %p%%")
  :init
  (display-battery-mode)
  )

;;;; Fancy-battery
(use-package fancy-battery
  :custom
  (fancy-battery-show-percentage t)
  :init
  (fancy-battery-mode)
  )

;;;; Display-line-numbers-mode
;; Show line numbers on the left fringe
(use-package display-line-numbers
  :ghook 'prog-mode-hook 'LaTeX-mode-hook
  :gfhook 'column-number-mode ; Column number in modeline
  :general (kb/toggle-keys
             "l" '(display-line-numbers-mode :which-key "Line numbers"))
  :custom
  (display-line-numbers-type 'relative)
  )

;;; themes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'themes-rcp)
