;;; checking-spelling-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for spell checking.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Flyspell
;; Feature-rich spell-checker
(use-package flyspell
  :ensure-system-package ((aspell . "sudo apt install aspell aspell-en")
                          (proselint . "sudo add-apt-repository universe && sudo apt install python3-proselint"))
  :ghook
  ('text-mode-hook 'flyspell-mode)
  ('prog-mode-hook 'flyspell-prog-mode)
  :custom
  (flyspell-abbrev-p t) ; Save changes made by flyspell to abbrev_defs file (abbrev mode)
  (flyspell-issue-message-flag nil) ; Disable to prevent massive slowdown
  (flyspell-issue-welcome-flag nil) ; Don't display welcome message
  (flycheck-proselint-executable "/usr/bin/proselint")

  ;; Personal dictionary
  (ispell-personal-dictionary (concat no-littering-var-directory "flyspell/flyspell-ispell-personal-dict-en"))
  :config
  ;; Face for incorrect words
  (set-face-attribute 'flyspell-incorrect nil :underline '(:color "red2" :style wave))
  (set-face-attribute 'flyspell-duplicate nil :underline '(:color "#D19A66" :style line))
  )

;;;; Flyspell-lazy
;; Check on idle instead of instantly
(use-package flyspell-lazy
  :requires flyspell
  :after flyspell
  :ghook 'flyspell-mode-hook
  :custom
  (flyspell-lazy-changes-threshold 10) ; Force check if this many changes are pending
  (flyspell-lazy-idle-seconds 2)
  (flyspell-lazy-window-idle-seconds 7)
  (flyspell-lazy-use-flyspell-word t) ; Immediately recheck when leaving a marked word?
  )

;;;; Flyspell-correct
;; Suggest correct spelling for words flyspell marks as incorrect
(use-package flyspell-correct
  :requires flyspell
  :after flyspell
  :general (kb/leader-keys
             "." '(flyspell-correct-next :which-key "Flyspell next")
             "," '(flyspell-correct-previous :which-key "Flyspell prev")
             "/" '(flyspell-buffer :which-key "Flyspell check buffer")
             "tc" '(flyspell-mode :which-key "Flyspell toggle")
             )
  :custom
  (flyspell-correct-interface 'flyspell-correct-completing-read)
  )

;;; checking-spelling-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-spelling-rcp)
