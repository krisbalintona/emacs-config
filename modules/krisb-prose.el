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
;;;; Harper language server
(with-eval-after-load 'eglot
  (if (executable-find "harper-ls")
      (progn
        (add-to-list 'eglot-server-programs
                     '(markdown-mode . ("harper-ls" "--stdio")))
        (add-to-list 'eglot-server-programs
                     ;; NOTE 2025-03-19: We give a language ID of "markdown" to harper
                     ;; (see supported languages and their corresponding language IDs
                     ;; here:
                     ;; https://writewithharper.com/docs/integrations/language-server#Supported-Languages)
                     ;; because org-mode is currently not supported.  Markdown is the
                     ;; closest we have.  (Note: it is better than the "plaintext"
                     ;; language ID; see the recommendation here:
                     ;; https://github.com/Automattic/harper/issues/149#issuecomment-2619515397.)
                     ;;
                     ;; To check the status of adding org-mode to the list of
                     ;; supported languages, see
                     ;; https://github.com/Automattic/harper/issues/79#issuecomment-2638110954.
                     '((org-mode :language-id "markdown") . ("harper-ls" "--stdio"))))
    (message "Harper-ls not installed; not configuring with eglot")))

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
