;;; checking-spelling-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for spell checking
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Spell-fu
;; Fast and simple spell checking
(use-package spell-fu
  :disabled t ; Testing flyspell for now
  :ensure-system-package (aspell)
  :hook
  ((text-mode . spell-fu-mode)
   (spell-fu-mode . (lambda () ; Change personal dictionary
                      (setq ispell-personal-dictionary (concat spell-fu-directory "spell-fu-ispell-personal-dict-en"))
                      ))
   )
  :custom
  (spell-fu-idle-delay 0.6)
  (spell-fu-directory (concat no-littering-var-directory "spell-fu/"))
  :config
  (kb/leader-keys
    "ts" '(spell-fu-mode :which-key "Toggle spell-fu")
    "ta" '(spell-fu-word-add :which-key "Spell-fu-word-add")
    )
  )

;;;; Flyspell packages
;;;;; Flyspell
;; Feature-rich spell-checker
(use-package flyspell
  :ensure-system-package ((aspell)
                          (proselint))
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)
   (flyspell-mode . (lambda () ; Change personal dictionary
                      (setq ispell-personal-dictionary (concat no-littering-var-directory "flyspell/flyspell-ispell-personal-dict-en"))
                      ))
   )
  :custom
  (flyspell-abbrev-p t) ; Save changes made by flyspell to abbrev_defs file (abbrev mode)
  (flyspell-issue-message-flag nil) ; Disable to prevent massive slowdown
  (flyspell-issue-welcome-flag nil) ; Don't display welcome message
  :config
  ;; Face for incorrect words
  (set-face-attribute 'flyspell-incorrect nil :underline '(:color "red2" :style wave))
  (set-face-attribute 'flyspell-duplicate nil :underline '(:color "#D19A66" :style line))
  )

;;;;; Flyspell-lazy
;; Check on idle instead of instantly
(use-package flyspell-lazy
  :after flyspell
  :hook (flyspell-mode . flyspell-lazy-mode)
  :custom
  (flyspell-lazy-changes-threshold 100) ; Force check if this many changes are pending
  (flyspell-lazy-idle-seconds 2)
  (flyspell-lazy-window-idle-seconds 15)
  (flyspell-lazy-use-flyspell-word nil) ; Immediately recheck when leaving a marked word
  (flyspell-correct-interface #'flyspell-correct-dummy) ; Use default (now selectrum) minibuffer
  )

;;;;; Flyspell-correct
;; Suggest correct spelling for words flyspell marks as incorrect
(use-package flyspell-correct
  :after flyspell
  :config
  (kb/leader-keys
    "." '(flyspell-correct-next :which-key "Flyspell next")
    "," '(flyspell-correct-previous :which-key "Flyspell prev")
    "c" '(flyspell-buffer :which-key "Flyspell check buffer")
    "tc" '(flyspell-mode :which-key "Flyspell toggle")
    )
  )

;;;;; Flyspell-correct-helm
;; Flyspell-correct using Helm interface
(use-package flyspell-correct-helm
  :disabled t ; Now use selectrum (flyspell-correct-dummy) for cleaner interface
  :after flyspell-correct
  )

;;;;; Auto-dictionary
;; Automatically change ispell dictionary based on contents of buffer
(use-package auto-dictionary
  :disabled t ; Seeing what effect it has
  :after flyspell
  :hook (flyspell-mode . auto-dictionary-mode)
  )

;;; checking-spelling-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-spelling-rcp)
