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
  :ensure-system-package (aspell
                          ("/usr/share/licenses/aspell-en/" . aspell-en))
  :diminish
  :hook ((text-mode . (lambda ()             ; Prevent conflicts
                        (unless (featurep 'wucuo)
                          (flyspell-mode))))
         (prog-mode . (lambda ()             ; Prevent conflicts
                        (unless (featurep 'wucuo)
                          (flyspell-prog-mode)))))
  :general
  ;; Unbind all the keys from the mode-map because they're all annoying...
  (:keymaps 'flyspell-mode-map
   "C-," nil
   "C-." nil
   "C-;" nil
   "C-c $" nil)
  :custom
  (flyspell-use-meta-tab nil)

  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (flyspell-consider-dash-as-word-delimiter-flag t)

  (flyspell-delay 3)
  (flyspell-sort-corrections t)

  (flyspell-prog-text-faces '(font-lock-string-face
                              font-lock-comment-face
                              font-lock-doc-face
                              tree-sitter-hl-face:string
                              tree-sitter-hl-face:string.special ; For things like regexps
                              tree-sitter-hl-face:comment
                              tree-sitter-hl-face:doc
                              ))

  ;; Personal dictionary
  (flyspell-abbrev-p t) ; Save changes made by flyspell to abbrev file
  (flyspell-use-global-abbrev-table-p nil)
  (ispell-personal-dictionary (no-littering-expand-var-file-name "flyspell/flyspell-ispell-personal-dict-en"))
  (ispell-extra-args (list "--sug-mode=ultra" "--lang=en_US" "--camel-case")))

;;; Wucuo
;; A complete solution to the lag of flyspell
(use-package wucuo
  :disabled
  :diminish
  :after flyspell
  :hook ((text-mode prog-mode) . (lambda ()
                                   (interactive)
                                   ;; `wucuo' is incompatible with `flyspell'
                                   (flyspell-mode -1)
                                   (wucuo-start)))
  :general ([remap flyspell-buffer] 'wucuo-spell-check-visible-region)
  :custom
  (wucuo-flyspell-start-mode "normal")
  (wucuo-personal-font-faces-to-check flyspell-prog-text-faces)
  (wucuo-double-check-font-faces '(font-lock-string-face
                                   tree-sitter-hl-face:string
                                   ))
  (wucuo-modes-whose-predicate-ignored nil)
  (wucuo-spell-check-buffer-predicate
   '(lambda ()                           ; Skip spell checking under these conditions
       (not (memq major-mode
                  '(dired-mode
                    log-edit-mode
                    compilation-mode
                    help-mode
                    helpful-mode
                    profiler-report-mode
                    speedbar-mode
                    gud-mode
                    calc-mode
                    Info-mode
                    )))))
  :config
  (defun kb/wucuo-mode-on ()
    "Turn wucuo mode on.  Do not use this; use `wucuo-mode' instead."
    (if flyspell-mode
        (message "Please turn off `flyspell-mode' and `flyspell-prog-mode' before wucuo starts!")
      (wucuo-enhance-flyspell)
      ;; Add to `before-save-hook' instead so it is compatible with `super-save'
      ;; + `eyebrowse-pre-window-switch-hook'
      (add-hook 'before-save-hook #'wucuo-spell-check-buffer nil t)))
  (advice-add 'wucuo-mode-on :override #'kb/wucuo-mode-on))

;;; Flyspell-correct
;; Suggest correct spelling for words flyspell marks as incorrect
(use-package flyspell-correct
  :after flyspell
  :chords (("<<" . flyspell-correct-previous)
           (">>" . flyspell-correct-next))
  :custom
  (flyspell-correct-interface 'flyspell-correct-completing-read))

;;; Abbrev-mode
;; Automatically correct typed strings (e.g. words). Most useful for correcting
;; spelling mistakes as they are made.
(use-package abbrev
  :straight nil
  :diminish
  :ghook 'text-mode-hook 'prog-mode-hook
  :custom
  (abbrev-file-name (concat no-littering-var-directory "abbrev-mode/abbrev.el"))
  (save-abbrevs 'silently))

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
