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
;;;; Fringes
(set-fringe-style 1)               ; Equivalent to the "minimal" `fringe-styles'

;;;; Line-spacing
(setq line-spacing 1)

;;;; Hide-mode-line
;; Hide the modeline when you don't want to see it
(use-package hide-mode-line
  :commands hide-mode-line-mode)

;;;; Transparency
;; Set the alpha-background parameter. Initially arose from a patch of Emacs
;; 29.0.50: https://github.com/TheVaffel/emacs, but was merged on January 30,
;; 2022:
;; https://github.com/emacs-mirror/emacs/commit/5c87d826201a5cae242ce5887a0aa7e24ad6f5ee
(unless kb/linux-ubuntu
  (set-frame-parameter nil 'alpha '(98 . 98))
  (add-to-list 'default-frame-alist '(alpha . (98 . 98)))
  (set-frame-parameter nil 'alpha-background 100)
  (add-to-list 'default-frame-alist '(alpha-background . 100)))

(defun kb/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 75))
    (pcase (frame-parameter nil 'alpha-background)
      ((pred (lambda (n) (= n alpha-transparency)))
       (set-frame-parameter nil 'alpha-background 100))
      (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))
(general-define-key "<f12>" 'kb/toggle-window-transparency)

;;;; Solaire-mode
;; Have "non-real" (by my own predicate) buffers and other faces swapped.
(use-package solaire-mode
  :hook ((after-change-major-mode dictionary-mode) . turn-on-solaire-mode)
  :custom
  (solaire-mode-real-buffer-fn
   '(lambda ()                  ; Real buffers have at least one of these properties:
       (or (buffer-file-name)                         ; Connected to a file
           ;; (string-match "*[Ss]cratch" (buffer-name)) ; Is a scratch buffer
           (string-match "*Minimap*" (buffer-name)) ; Demap minimap
           )))
  :init
  ;; NOTE 2022-01-21: Enable `solaire-global-mode' if I want to swap the
  ;; background faces which solaire remaps, e.g., non-real buffers dark and real
  ;; light to non-real light and real dark.
  ;; (solaire-global-mode)
  )

;;;; Lin
(use-package lin
  :custom
  (lin-face 'kb/lin-face)
  (lin-mode-hooks
   '(prog-mode-hook
     dired-mode-hook
     elfeed-search-mode-hook
     git-rebase-mode-hook
     grep-mode-hook
     log-view-mode-hook
     magit-log-mode-hook
     mu4e-headers-mode
     notmuch-search-mode-hook
     notmuch-tree-mode-hook
     occur-mode-hook
     org-agenda-mode-hook
     proced-mode-hook
     tabulated-list-mode-hook))
  :init
  (defface kb/lin-face
    '((default :foreground unspecified :underline nil :extend t)
      (((class color) (min-colors 88) (background light))
       :background "#c0e4ff")
      (((class color) (min-colors 88) (background dark))
       :background "#212428")
      (t :background "blue"))
    "My face for `lin-face'.")
  (lin-global-mode))

;;; Modeline
;;;; Doom-modeline
;; Sleek modeline from Doom Emacs
(use-package doom-modeline
  :custom
  ;; Modeline settings
  (doom-modeline-window-width-limit fill-column) ; The limit of the window width.
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon (display-graphic-p)) ; Show icons if in Emacs GUI or server
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
  :config (require 'kb-doom-modeline-segments))

;;;; Mood-line
(use-package mood-line)

;;;; Kb-mood-line
(use-package kb-mood-line
  :straight nil
  :hook ((window-setup server-after-make-frame) . kb/mood-line-setup)
  :init
  (require 'doom-modeline))

;;;; Time
;; Enable time in the mode-line
(use-package time
  ;; :hook (window-setup . display-time-mode)
  :custom
  ;; (display-time-format "%H:%M:%S")     ; Use 24hr format with seconds
  ;; (display-time-interval 1)            ; Update every since if I'm using seconds
  (display-time-format "%H:%M")        ; Use 24hr format with seconds
  (display-time-interval 60)           ; Update every since if I'm using seconds
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
  :ghook 'prog-mode-hook 'LaTeX-mode-hook 'conf-mode-hook
  :gfhook 'column-number-mode           ; Column number in modeline
  :general (kb/toggle-keys
             "l" '(display-line-numbers-mode :wk "Line numbers"))
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start t)) ; Keep width consistent in buffer

;;;; Which-function
;; Display the name of the function I am currently under
(use-package which-func
  :init
  (which-func-mode)
  )

;;; Other
;;;; Fontaine
;; Test faces without restart Emacs!
(use-package fontaine
  :init
  (setq fontaine-presets
        '((regular
           :default-family "Hack"
           :default-weight normal
           :default-height 100
           :fixed-pitch-family "Fira Code"
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :variable-pitch-family "Noto Sans"
           :variable-pitch-weight normal
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family "Source Code Pro"
           :italic-slant italic
           :line-spacing 1)
          (large
           :default-family "Iosevka"
           :default-weight normal
           :default-height 150
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :variable-pitch-family "FiraGO"
           :variable-pitch-weight normal
           :variable-pitch-height 1.05
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil ; use whatever the underlying face has
           :italic-slant italic
           :line-spacing 1)
          ;; Mine
          (test
           :default-family "Iosevka Term SS04"
           :default-weight normal
           :default-height 140
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-weight nil
           :fixed-pitch-height 1.0
           :variable-pitch-family "Open Sans"
           :variable-pitch-weight normal
           :variable-pitch-height 1.10
           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing 1)))
  )

;;; themes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'themes-rcp)
