;;; checking-spelling-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for spell checking.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Flyspell
;; Feature-rich spell-checker
(use-package flyspell
  :ensure-system-package ((aspell . "sudo apt install aspell aspell-en")
                          (proselint . "sudo add-apt-repository universe && sudo apt install python3-proselint"))
  :custom
  (flyspell-issue-message-flag nil)     ; Disable to prevent massive slowdown
  (flyspell-issue-welcome-flag nil)     ; Don't display welcome message

  (flyspell-delay 1)                    ; Number of seconds to wait
  (flyspell-sort-corrections t)         ; Sort candidates

  (flyspell-abbrev-p t) ; Save changes made by flyspell to abbrev_defs file (`abbrev-mode')
  (flycheck-proselint-executable (kb/shell-command-to-string "which proselint"))
  ;; Personal dictionary
  (ispell-personal-dictionary (concat no-littering-var-directory "flyspell/flyspell-ispell-personal-dict-en"))
  )

;;; Wucuo
;; A complete solution to the lag of flyspell
(use-package wucuo
  :after flyspell
  :hook ((text-mode . wucuo-start)
         (prog-mode . wucuo-start)
         )
  :general (kb/leader-keys
             "/" '(wucuo-spell-check-visible-region :which-key "Spellcheck buffer"))
  :custom
  (wucuo-flyspell-start-mode "fast")
  ;; (ispell-extra-args "--run-together")  ; Faster aspell?
  (wucuo-spell-check-buffer-predicate
   (lambda ()
     (not (memq major-mode ; Skip spell checking under these conditions
                '(dired-mode
                  log-edit-mode
                  compilation-mode
                  help-mode
                  profiler-report-mode
                  speedbar-mode
                  gud-mode
                  calc-mode
                  Info-mode
                  ))
          )))
  )

;;; Flyspell-lazy
;; Check on idle instead of instantly
(use-package flyspell-lazy
  :disabled t ; Not necessary since flyspell is fast enough, especially with wucuo
  :after flyspell
  :ghook 'flyspell-mode-hook
  :custom
  (flyspell-lazy-changes-threshold 10) ; Force check if this many changes are pending
  (flyspell-lazy-idle-seconds 2)
  (flyspell-lazy-window-idle-seconds 7)
  (flyspell-lazy-use-flyspell-word t) ; Immediately recheck when leaving a marked word?
  )

;;; Flyspell-correct
;; Suggest correct spelling for words flyspell marks as incorrect
(use-package flyspell-correct
  :after flyspell
  :general (kb/leader-keys
             "." '(flyspell-correct-next :which-key "Flyspell next")
             "," '(flyspell-correct-previous :which-key "Flyspell prev")
             )
  :custom
  (flyspell-correct-interface 'flyspell-correct-completing-read)
  )

;;; Abbrev-mode
;; Automatically correct typed strings (e.g. words). Most useful for correcting
;; spelling mistakes as they are made.
(use-package abbrev-mode
  :straight nil
  :ghook 'text-mode-hook 'prog-mode-hook
  :custom
  (abbrev-file-name (concat no-littering-var-directory "abbrev-mode/abbrev.el"))
  )

;;; checking-spelling-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-spelling-rcp)
