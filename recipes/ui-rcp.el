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

;;;;; Light
(use-package solo-jazz-theme :disabled)
(use-package kaolin-themes  :disabled)
(when (fboundp 'elpaca-wait)
  (elpaca-wait))

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
                  ;; Beframe
                  (:eval (when (and (bound-and-true-p beframe-mode)
                                    (mode-line-window-selected-p))
                           (propertize (concat "@" (frame-parameter nil 'name))
                                       'face (list :inherit 'mode-line
                                                   :slant 'italic
                                                   :foreground (face-foreground 'font-lock-string-face)))))
                  ;; Bufferlo
                  (:eval (when-let ((name (frame-parameter nil 'bufferlo-bookmark-frame-name))
                                    (bound-and-true-p bufferlo-mode)
                                    (mode-line-window-selected-p))
                           (propertize (concat "@" name)
                                       'face (list :inherit 'mode-line
                                                   :slant 'italic
                                                   :foreground (face-foreground 'font-lock-string-face)))))
                  mode-line-end-spaces
                  ;; REVIEW 2024-10-17: For some reason the right side of my
                  ;; mode line gets cut off... maybe font is?
                  " "))

  ;; Add things to `global-mode-string'
  (add-to-list 'global-mode-string '(:eval
                                     (when-let ((branch (and (featurep 'vc) (car (vc-git-branches)))))
                                       (unless (or (string= branch "master")
                                                   (string= branch "main"))
                                         vc-mode))))
  (add-to-list 'global-mode-string '(project-mode-line project-mode-line-format)))

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
  ( :map krisb-toggle-keymap
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
  ( :map krisb-toggle-keymap
    ("m" . hide-mode-line-mode)))

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

(provide 'ui-rcp)
;;; ui-rcp.el ends here
