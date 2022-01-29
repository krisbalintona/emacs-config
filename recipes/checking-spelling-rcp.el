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
  :ensure-system-package aspell
  :hook ((text-mode . (lambda ()             ; Prevent conflicts
                        (unless wucuo-mode
                          (flyspell-mode))))
         (prog-mode . (lambda ()             ; Prevent conflicts
                        (unless wucuo-mode
                          (flyspell-prog-mode)))))
  :general
  (kb/general-keys
    "/" '(flyspell-buffer :wk "Spellcheck buffer")
    )
  (:keymaps 'flyspell-mode-map
            "C-;" nil       ; I don't like `flyspell-auto-correct-previous-word'
            )
  :custom
  (flyspell-issue-message-flag nil)     ; Disable to prevent massive slowdown
  (flyspell-issue-welcome-flag nil)     ; Don't display welcome message

  (flyspell-delay 1)                    ; Time to wait
  (flyspell-sort-corrections t)         ; Sort candidates?

  (flyspell-abbrev-p t) ; Save changes made by flyspell to abbrev_defs file (`abbrev-mode')
  ;; Personal dictionary
  (ispell-personal-dictionary (no-littering-expand-var-file-name "flyspell/flyspell-ispell-personal-dict-en")))

;;; Wucuo
;; A complete solution to the lag of flyspell
(use-package wucuo
  :after flyspell
  :hook ((text-mode prog-mode) . (lambda ()
                                   (interactive)
                                   ;; Make sure this isn't enabled before starting
                                   ;; wucuo
                                   (flyspell-mode -1)
                                   (wucuo-start)))
  :general (kb/general-keys
             [remap flyspell-buffer] '(wucuo-spell-check-visible-region :wk "Spellcheck buffer"))
  :custom
  (wucuo-flyspell-start-mode "fast")
  (wucuo-spell-check-buffer-predicate
   (lambda ()                            ; Skip spell checking under these conditions
     (not (memq major-mode
                '(dired-mode
                  log-edit-mode
                  compilation-mode
                  help-mode
                  profiler-report-mode
                  speedbar-mode
                  gud-mode
                  calc-mode
                  Info-mode
                  ))))))

;;; Flyspell-correct
;; Suggest correct spelling for words flyspell marks as incorrect
(use-package flyspell-correct
  :after flyspell
  :general (kb/general-keys
             "." '(flyspell-correct-next :wk "Flyspell next")
             "," '(flyspell-correct-previous :wk "Flyspell prev")
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
  (save-abbrevs 'silently)
  )

;;; checking-spelling-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-spelling-rcp)
