;;; ui-rcp.el --- Appearance of Emacs                -*- lexical-binding: t; -*-

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

;; General Emacs appearance stuff.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'fonts-rcp)

;;;; Install themes
(setq custom-theme-load-path load-path)

;;;;; Dark
(use-package atom-one-dark-theme :disabled)
(use-package apropospriate-theme :disabled)
(use-package nano-theme)
(use-package mood-one-theme
  :disabled
  :init
  (mood-one-theme-arrow-fringe-bmp-enable)
  (eval-after-load 'flycheck #'mood-one-theme-flycheck-fringe-bmp-enable))
(require 'uninspiring-dark-theme)

;;;;; Light
(use-package modus-themes
  :autoload modus-themes-with-colors
  :hook ((modus-themes-after-load-theme . kb/themes-setup-base-faces)
         ((modus-themes-after-load-theme kb/themes) . kb/modus-themes-solaire-faces))
  :custom
  ;; Also make sure these are set before `modus-themes-load-themes' is called
  (modus-themes-custom-auto-reload t) ; only applies to `customize-set-variable' and related
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(semibold))
  (modus-themes-completions '(((matches . (heavy))
                               (selection . (semibold)))))
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-headings '((t . (semibold))))
  :init
  ;; Taken from (info "(modus-themes) Add support for solaire-mode")
  (defun kb/modus-themes-solaire-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       `(solaire-default-face ((,c :inherit default :background ,bg-dim :foreground ,fg-dim)))
       `(solaire-line-number-face ((,c :inherit solaire-default-face :foreground ,fg-line-number-inactive)))
       `(solaire-hl-line-face ((,c :background ,bg-active)))
       `(solaire-org-hide-face ((,c :background ,bg-dim :foreground ,bg-dim))))))
  :config
  ;; Overrides
  (setopt modus-themes-common-palette-overrides
          `(;; Completion
            (fg-completion-match-0 fg-main) ; See (info "(modus-themes) Make headings more or less colorful")
            (fg-completion-match-1 fg-main)
            (fg-completion-match-2 fg-main)
            (fg-completion-match-3 fg-main)
            (bg-completion-match-0 bg-blue-intense)
            (bg-completion-match-1 bg-yellow-intense)
            (bg-completion-match-2 bg-cyan-intense)
            (bg-completion-match-3 bg-red-intense)

            ;; "Invisible" border in mode line. See (info "(modus-themes) Make the
            ;; mode line borderless")
            (border-mode-line-active bg-mode-line-active)
            (border-mode-line-inactive bg-mode-line-inactive)

            ;; Headings
            (fg-heading-1 red-faint)
            (fg-heading-6 rainbow-0)

            ;; Make the fringe invisible
            (fringe unspecified)

            ;; More noticeable block (e.g. org) backgrounds
            (bg-prose-block-contents bg-active-value))
          modus-operandi-palette-overrides
          `(
            ;; I like `modus-*-tinted's mode line colors. I like to keep
            ;; `border-mode-line-active' and `border-mode-line-inactive'
            ;; "invisible" though
            (bg-mode-line-active        "#cab9b2")
            (fg-mode-line-active        "#000000")
            (bg-mode-line-inactive      "#dfd9cf")
            (fg-mode-line-inactive      "#585858"))
          modus-vivendi-palette-overrides
          `(
            ;; I like `modus-*-tinted's mode line colors. I like to keep
            ;; `border-mode-line-active' and `border-mode-line-inactive'
            ;; "invisible" though
            (bg-mode-line-active        "#484d67")
            (fg-mode-line-active        "#ffffff")
            (bg-mode-line-inactive      "#292d48")
            (fg-mode-line-inactive      "#969696"))))
(use-package solo-jazz-theme :disabled)
(use-package kaolin-themes  :disabled)
(when (fboundp 'elpaca-wait)
  (elpaca-wait))

;;;; kb/theme-switcher
;;;;; Variable declarations
(defvar kb/themes-dark 'modus-vivendi
  "My chosen dark theme.")

(defvar kb/themes-light 'modus-operandi
  "My chosen light theme.")

(defvar kb/themes-hook nil
  "Hook that runs after the `kb/proper-load-theme-*'.")
(add-hook 'window-setup-hook #'(lambda () (run-hooks 'kb/themes-hook)) 100)

;;;;; Function definitions
(defun kb/themes-setup-base-faces (&optional frame)
  "Set up the default, fixed-pitch, and variable-pitch faces in FRAME."
  (setq frame (or frame (selected-frame)))
  (select-frame frame)

  (set-face-attribute 'mode-line-active nil
                      :background (modus-themes-with-colors bg-mode-line-active)
                      :box `( :line-width 3
                              :color ,(modus-themes-with-colors bg-mode-line-active)))
  (require 'color)
  (let ((bg-color
         (if (eq (car custom-enabled-themes) kb/themes-dark)
             (color-darken-name (modus-themes-with-colors bg-mode-line-inactive) 13)
           (color-lighten-name (modus-themes-with-colors bg-mode-line-inactive) 13))))
    (set-face-attribute 'mode-line-inactive nil
                        :background bg-color
                        :box `( :line-width 3
                                :color ,bg-color)))

  (modus-themes-with-colors
    (set-face-attribute 'cursor nil :background magenta-cooler))

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

;;;;; Command
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
(define-key global-map (kbd "<f8>") 'kb/theme-switcher)

;;;;; Load appropriate theme based on time of day
(let ((hour (string-to-number (format-time-string "%H"))))
  ;; Dark theme between 7 PM or 8 AM
  (if (or (<= 19 hour) (<= hour 8))
      (kb/proper-load-theme-dark)
    (kb/proper-load-theme-light)))
(add-hook 'elpaca-after-init-hook #'kb/themes-setup-base-faces) ; Initialize for daemon
(add-hook 'server-after-make-frame-hook #'kb/themes-setup-base-faces)
(when (fboundp 'elpaca-wait)
  (elpaca-wait))

(provide 'kb-themes)

;;;; Modeline
;;;;; Nerd-icons
(use-package nerd-icons)

;;;;; Doom-modeline
;; Sleek modeline from Doom Emacs
(use-package doom-modeline
  :disabled
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

;;;;; Kb-mood-line
(use-package kb-mood-line
  :disabled
  :ensure nil
  ;; :hook ((window-setup server-after-make-frame) . kb/mood-line-setup)
  :init
  (require 'doom-modeline)
  (use-package mood-line))

;;;;; Minions
(use-package minions
  :hook
  ((elpaca-after-init after-init) . minions-mode)
  :custom
  (minions-mode-line-lighter "…")
  (minions-mode-line-delimiters '("[" . "]"))
  (minions-prominent-modes
   '(kb/lisp-keyword-indent-mode tree-sitter-mode)))

;;;;; Diminish
(use-package diminish
  :hook (after-init . kb/diminish-setup)
  :init
  (defun kb/diminish-setup ()
    "Set up `diminish’ lighters for pre-loaded packages (packages that
are troublesome)."
    (with-eval-after-load 'subword
      (diminish 'subword-mode))
    (with-eval-after-load 'simple
      (diminish 'visual-line-mode))
    ;; (with-eval-after-load (diminish 'hs-minor-mode))
    (with-eval-after-load 'face-remap
      (diminish 'buffer-face-mode))
    (with-eval-after-load 'abbrev
      (diminish 'abbrev-mode))))

;;;;; Default mode line
(unless (bound-and-true-p mood-line-mode)
  (defvar kb/mode-line-modes
    (let ((recursive-edit-help-echo
           "Recursive edit, type M-C-c to get out"))
      (list (propertize "%[" 'help-echo recursive-edit-help-echo)
            `(:propertize ("" mode-name)
                          help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                          mouse-face mode-line-highlight
                          local-map ,mode-line-major-mode-keymap)
            '("" mode-line-process)
            `(:propertize ("" minor-mode-alist)
                          mouse-face mode-line-highlight
                          help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                          local-map ,mode-line-minor-mode-keymap)
            (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                        'mouse-face 'mode-line-highlight
                        'local-map (make-mode-line-mouse-map
                                    'mouse-2 #'mode-line-widen))
            (propertize "%]" 'help-echo recursive-edit-help-echo)
            " "))
    "Mode line construct for displaying major and minor modes.
This version removes delimiters.")

  (setq mode-line-defining-kbd-macro (propertize " Macro" 'face 'mode-line-emphasis)
        mode-line-compact 'long        ; Emacs 28
        mode-line-right-align-edge 'window
        mode-line-percent-position nil ; Don't show percentage of position in buffer
        mode-line-position-line-format '(" %l")
        mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28

  (setq-default mode-line-buffer-identification (propertized-buffer-identification "%20b")
                mode-line-format
                '("%e" mode-line-front-space
                  mode-line-mule-info   ; Input method info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-window-dedicated " "
                  mode-line-buffer-identification
                  mode-line-position
                  (anzu-mode anzu--mode-line-format)
                  (:eval (when (and (bound-and-true-p mlscroll-mode)
                                    (mode-line-window-selected-p)
                                    (not (derived-mode-p 'pdf-view-mode)))
                           (mlscroll-mode-line)))
                  mode-line-format-right-align
                  mode-line-process
                  (flymake-mode flymake-mode-line-format) " "
                  (:eval (when (mode-line-window-selected-p)
                           mode-line-misc-info))
                  (:eval kb/mode-line-modes)
                  mode-line-end-spaces))

  ;; Add things to `global-mode-string'
  (add-to-list 'global-mode-string '(:eval
                                     (when-let ((branch (and (featurep 'vc) (car (vc-git-branches)))))
                                       (unless (or (string= branch "master")
                                                   (string= branch "main"))
                                         vc-mode))))
  (add-to-list 'global-mode-string '(project-mode-line project-mode-line-format)))

;;;;; Time
;; Enable time in the mode-line
(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t)
  (display-time-format "%R")
  (display-time-interval 60)
  (display-time-default-load-average nil)
  (world-clock-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Nicosia" "Nicosia (capital of Cyprus)")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Tokyo" "Tokyo")
     ("Asia/Shanghai" "Beijing")))
  :init
  (display-time-mode 1))

;;;;; Battery
;; Display batter percentage
(use-package battery
  :disabled
  :ensure nil
  :custom
  (battery-load-critical 15)
  (battery-load-low 25)
  (battery-mode-line-limit 95)
  ;; (battery-mode-line-format "%cmAh")
  ;; (battery-mode-line-format "  %p%%")
  (battery-mode-line-format "%b%p%% ")
  :init
  (display-battery-mode 1))

;;;;; Display-line-numbers
;; Show line numbers on the left fringe
(use-package display-line-numbers
  :ensure nil
  :bind
  ( :map kb/toggle-keys
    ("l" . display-line-numbers-mode))
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width-start t)) ; Keep width consistent in buffer

;;;;; Mlscroll
;; Adds an interactive indicator for the view's position in the current buffer
;; to the modeline
(use-package mlscroll
  :hook ((on-first-buffer . mlscroll-mode)
         (kb/themes . kb/mlscroll-set-colors))
  :custom
  (mlscroll-right-align nil) ; Doesn't work well with `mode-line-right-align-edge'
  (mlscroll-alter-percent-position nil) ; I position it myself
  :config
  (defun kb/mlscroll-set-colors ()
    "Set colors for `mlscroll'."
    (let ((m (bound-and-true-p mlscroll-mode)))
      (when m (mlscroll-mode -1))
      (modus-themes-with-colors
        (customize-set-variable 'mlscroll-in-color bg-mode-line-inactive)
        (customize-set-variable 'mlscroll-out-color bg-mode-line-active))
      (when m (mlscroll-mode 1))))
  (kb/mlscroll-set-colors))

;;;; Other UI
;;;;; Hide-mode-line
;; Hide the modeline when you don't want to see it
(use-package hide-mode-line
  :commands hide-mode-line-mode
  :bind
  ( :map kb/toggle-keys
    ("m" . hide-mode-line-mode)))

;;;;; Transparency toggle
;; Set the alpha-background parameter. Initially arose from a patch of Emacs
;; 29.0.50: https://github.com/TheVaffel/emacs, but was merged on January 30,
;; 2022:
;; https://github.com/emacs-mirror/emacs/commit/5c87d826201a5cae242ce5887a0aa7e24ad6f5ee
(unless kb/linux-ubuntu
  (set-frame-parameter nil 'alpha-background 100)
  (add-to-list 'default-frame-alist '(alpha-background . 100)))

(defun kb/toggle-window-transparency (&optional arg)
  "Toggle the value of `alpha-background'.

Toggles between 100 and 72 by default. Can choose which value to
change to if called with ARG."
  (interactive "P")
  (let ((transparency (pcase arg
                        ((pred numberp) arg)
                        ((pred car) (read-number "Change the transparency to which value (0-100)? "))
                        (_
                         (cl-case (frame-parameter nil 'alpha-background)
                           (72 100)
                           (100 72)
                           (t 100))))))
    (set-frame-parameter nil 'alpha-background transparency)))
(bind-key "<f9>" #'kb/toggle-window-transparency)

;;;;; Solaire-mode
;; Have "non-real" (by my own predicate) buffers and other faces swapped.
;; NOTE 2023-07-13: Make sure you set solaire's faces if my theme does not
;; already set them
(use-package solaire-mode
  :disabled
  ;; :hook ((after-change-major-mode dictionary-mode) . turn-on-solaire-mode)
  :custom
  (solaire-mode-real-buffer-fn
   '(lambda ()                  ; Real buffers have at least one of these properties:
      (or (buffer-file-name (buffer-base-buffer)) ; Connected to a file
          ;; (string-match "*[Ss]cratch" (buffer-name)) ; Is a scratch buffer
          (string-match "*Minimap*" (buffer-name)) ; Demap minimap
          )))
  :init
  ;; NOTE 2022-01-21: Enable `solaire-global-mode' if I'm okay swapping the
  ;; background faces which solaire remaps, i.e., non-real buffers dark and real
  ;; light to non-real light and real dark.
  (solaire-global-mode))

;;;;; Lin
;; Lin is a stylistic enhancement for Emacs' built-in `hl-line-mode'. It remaps
;; the `hl-line' face (or equivalent) buffer-locally to a style that is optimal
;; for major modes where line selection is the primary mode of interaction.
(use-package lin
  :hook (on-first-buffer . lin-global-mode)
  :custom
  (lin-face 'lin-cyan)
  :config
  (add-to-list 'lin-mode-hooks 'LaTeX-mode-hook))

;;;;; Pulsar
(use-package pulsar
  :hook ((on-first-buffer . pulsar-global-mode)
         (consult-after-jump . pulsar-recenter-top)
         (consult-after-jump . pulsar-reveal-entry)
         (imenu-after-jump . pulsar-recenter-top)
         (imenu-after-jump . pulsar-reveal-entry))
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.05)
  (pulsar-iterations (/ 2 pulsar-delay)))

(provide 'ui-rcp)
;;; ui-rcp.el ends here
