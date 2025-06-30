;; -*- lexical-binding: t; -*-

;;; Environment
;;;; Visual-wrap
;; Visually indent lines wrapped visually! This makes long-lines in lists
;; properly indented!
;; NOTE: This package is the same as the more often referred to
;; `adaptive-wrap-prefix-mode'.
(use-package visual-wrap
  ;; 2024-10-30: Using adaptive-wrap for now since there seems to be some kind
  ;; of error between the interactions of visual-wrap and org-modern.  See
  ;; https://github.com/minad/org-modern/discussions/238.
  :disabled t
  :ensure nil
  :config
  (global-visual-wrap-prefix-mode 1))

;;;; Darkroom
(use-package darkroom
  :bind ( :map krisb-toggle-keymap
          ("d" . darkroom-mode)
          ("D" . darkroom-tentative-mode))
  :custom
  (darkroom-text-scale-increase 1.3))

;;;; Typewriter-roll-mode
(use-package typewriter-roll-mode
  :bind ( :map krisb-toggle-keymap
          ("r" . typewriter-roll-mode)))

;;;; Timers
;;;;; Tmr
(use-package tmr
  :bind ( :map krisb-open-keymap
          ("t" . krisb-tmr-dispatch))
  :custom
  ;; Useful variables
  (tmr-descriptions-list
   '("Stop working!" "Work time 😄"))
  (tmr-notification-urgency 'normal)
  (tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
  :config
  (require 'transient)
  (transient-define-prefix krisb-tmr-dispatch ()
    "Invoke a transient menu for `tmr'."
    ["Create or remove timers"
     [("t" "Create a timer" tmr)
      ("T" "Create a timer with description" tmr-with-details)
      ("C" "Clone a timer" tmr-clone)]
     [("r" "Remove finished" tmr-remove-finished)
      ("c" "Cancel timer" tmr-cancel)]]
    ["View timers"
     [("v" "Tabulated view" tmr-tabulated-view)]]))

;;; Spell checking
;;;; Ispell
(use-package ispell
  :ensure nil
  ;; For AUR:
  ;; :ensure-system-package aspell
  :custom
  (ispell-program-name (executable-find "aspell")) ; Aspell is better for English than hunspell
  (ispell-silently-savep t)
  :config
  ;; Use my personal enchant en_US dictionary
  (with-eval-after-load 'jinx
    (setopt ispell-personal-dictionary
            (expand-file-name "enchant/en_US.dic" (xdg-config-home)))))

;;; Grammar

;;; Other
;;;; Cm-mode (CriticMarkup minor mode)
;; Track suggested changes in plain text files.
(use-package cm-mode
  :disabled t                     ; 2025-04-14: Haven't found a use for this yet
  :vc ( :url "https://github.com/joostkremers/criticmarkup-emacs.git"
        :rev :newest)
  ;; For AUR:
  ;; :ensure-system-package (pandiff . nodejs-pandiff) ; Prose diffs for CriticMarkup
  )

;;; Provide
(provide 'krisb-prose)
