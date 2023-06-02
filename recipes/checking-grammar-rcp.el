;;; checking-grammar-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages relevant to checking grammar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Langtool
;; Use langtool program to check grammar of current buffer
(use-package langtool
  ;; NOTE 2021-08-19: Can't use `ensure-system-package' becuase the installation
  ;; of `languagetool' involves many steps (unless on Arch).
  :if (system-packages-package-installed-p "languagetool")
  :custom
  (langtool-default-language "en-US")
  (langtool-autoshow-message-function 'langtool-autoshow-detail-popup)
  ;; (langtool-java-classpath "~/Downloads/LanguageTool-5.3-stable")

  ;; See https://github.com/languagetool-org/languagetool for installation
  ;; instructions
  (langtool-bin "~/Downloads/LanguageTool-5.3-stable/languagetool.jar")
  (langtool-language-tool-jar "~/Downloads/LanguageTool-5.3-stable/languagetool-commandline.jar")
  (langtool-language-tool-server-jar "~/Downloads/LanguageTool-5.3-stable/languagetool-server.jar")
  (langtool-java-bin "/usr/bin/java")

  (langtool-server-user-arguments '("-p" "8082"))
  (langtool-http-server-host 'nil)
  (langtool-http-server-port 8082)
  (langtool-http-server-stream-type 'tls)
  (langtool-default-language "en-US")
  :init
  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  :config
  (pretty-hydra-define hydra:langtool
                       (:color green :hint t :foreign-keys run :quit-key "q" :exit t)
                       ("Correct grammar"
                        (("b" #'langtool-check-buffer "Check")
                         ("c" #'langtool-correct-buffer "Correct")
                         ("d" #'langtool-check-done "Done")))))

;;; Flymake-languagetool
(use-package flymake-languagetool
  :hook ((text-mode LaTeX-mode org-mode markdown-mode) . flymake-languagetool-load)
  :general (:keymaps 'flymake-mode-map
            (general-chord "``") 'flymake-languagetool-correct-dwim)
  :custom
  ;; See https://github.com/languagetool-org/languagetool for installation
  ;; instructions
  (flymake-languagetool-server-jar
   (expand-file-name "languagetool.jar" "~/Downloads/languagetool/LanguageTool-6.0-stable/"))
  (flymake-languagetool-active-modes
   '(text-mode latex-mode org-mode markdown-mode message-mode))
  (flymake-languagetool-check-spelling nil)
  ;; See https://community.languagetool.org/rule/list?lang=en for IDs
  (flymake-languagetool-disabled-rules
   '("DATE_NEW_YEAR" "WHITESPACE_RULE" "ARROWS")))

;;; Lsp-grammarly
(use-package lsp-grammarly
  :after lsp-mode
  :hook (lsp-grammarly-ls-after-open . (lambda () (lsp-ui-mode -1))))

;;; Eglot-grammarly
;; NOTE 2023-01-10: As reported here
;; (https://github.com/emacs-grammarly/eglot-grammarly/issues/7),
;; eglot-grammarly doesn't execute code actions. Thus, though I prefer this
;; package in every other way over lsp-grammarly, it is not usable
(use-package eglot-grammarly
  :demand
  :after eglot
  :ensure-system-package ("~/node_modules/@emacs-grammarly" . "npm install @emacs-grammarly/grammarly-languageserver")
  :straight (:host github :repo "emacs-grammarly/eglot-grammarly"))

;;; checking-grammar-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-grammar-rcp)
