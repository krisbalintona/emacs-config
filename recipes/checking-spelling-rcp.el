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
  :commands flyspell-detect-ispell-args
  :hook ((text-mode . (lambda ()             ; Prevent conflicts
                        (unless wucuo-mode
                          (flyspell-mode))))
         (prog-mode . (lambda ()             ; Prevent conflicts
                        (unless wucuo-mode
                          (flyspell-prog-mode)))))
  :general
  (kb/general-keys
    "/" '(flyspell-buffer :wk "Spellcheck buffer"))
  (:keymaps 'flyspell-mode-map
            "C-;" nil)      ; I don't like `flyspell-auto-correct-previous-word'
  :custom
  (flyspell-issue-message-flag nil)     ; Disable to prevent massive slowdown
  (flyspell-issue-welcome-flag nil)     ; Don't display welcome message

  (flyspell-delay 1)                    ; Time to wait
  (flyspell-sort-corrections t)         ; Sort candidates?

  (flyspell-abbrev-p t) ; Save changes made by flyspell to abbrev_defs file (`abbrev-mode')
  ;; Personal dictionary
  (ispell-personal-dictionary (no-littering-expand-var-file-name "flyspell/flyspell-ispell-personal-dict-en"))
  (ispell-extra-args (flyspell-detect-ispell-args t))
  :init
  ;; Taken from https://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs/
  (defun flyspell-detect-ispell-args (&optional run-together)
    "if RUN-TOGETHER is true, spell check the CamelCase words."
    (let (args)
      (cond
       ((string-match  "aspell$" ispell-program-name)
        ;; Force the English dictionary for aspell
        ;; Support Camel Case spelling check (tested with aspell 0.6)
        (setq args (list "--sug-mode=ultra" "--lang=en_US"))
        (when run-together
          (cond
           ;; Kevin Atkinson said now aspell supports camel case directly
           ;; https://github.com/redguardtoo/emacs.d/issues/796
           ((string-match-p "--camel-case"
                            (shell-command-to-string (concat ispell-program-name " --help")))
            (setq args (append args '("--camel-case"))))

           ;; old aspell uses "--run-together". Please note we are not dependent on this option
           ;; to check camel case word. wucuo is the final solution. This aspell options is just
           ;; some extra check to speed up the whole process.
           (t
            (setq args (append args '("--run-together" "--run-together-limit=16")))))))

       ((string-match "hunspell$" ispell-program-name)
        ;; Force the English dictionary for hunspell
        (setq args "-d en_US")))
      args)))

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

;;; Languagetool
(use-package languagetool
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :custom
  (languagetool-java-arguments '("-Dfile.encoding=UTF-8"))
  ;; NOTE 2022-02-13: Necessary so languagetool doesn't stop when not sure which
  ;; English dialect to use
  (languagetool-correction-language "en-US")
  (languagetool-console-command (concat kb/langtool-install-dir "languagetool-commandline.jar"))
  (languagetool-server-command (concat kb/langtool-install-dir "languagetool-server.jar"))
  :preface
  (defvar kb/langtool-install-dir (no-littering-expand-var-file-name "languagetool/")))

;;; checking-spelling-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-spelling-rcp)
