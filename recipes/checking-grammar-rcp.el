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
(use-package eglot-grammarly
  :demand
  :after eglot
  :ensure-system-package (grammarly-languageserver . "sudo npm install -g @emacs-grammarly/grammarly-languageserver")
  :straight (:type git :host github :repo "emacs-grammarly/eglot-grammarly")
  :config
  ;; Copied from
  ;; https://github.com/emacs-grammarly/eglot-grammarly/issues/7#issuecomment-1548616325
  (defun eglot--read-execute-code-action@override (actions server &optional action-kind)
    "Support for codeAction/resolve."
    (let* ((menu-items
            (or (cl-loop for a in actions
                         collect (cons (plist-get a :title) a))
                (apply #'eglot--error
                       (if action-kind `("No \"%s\" code actions here" ,action-kind)
                         `("No code actions here")))))
           (preferred-action (cl-find-if
                              (lambda (menu-item)
                                (plist-get (cdr menu-item) :isPreferred))
                              menu-items))
           (default-action (car (or preferred-action (car menu-items))))
           (chosen (if (and action-kind (null (cadr menu-items)))
                       (cdr (car menu-items))
                     (if (listp last-nonmenu-event)
                         (x-popup-menu last-nonmenu-event `("Eglot code actions:"
                                                            ("dummy" ,@menu-items)))
                       (cdr (assoc (completing-read
                                    (format "[eglot] Pick an action (default %s): "
                                            default-action)
                                    menu-items nil t nil nil default-action)
                                   menu-items))))))
      (cl-labels ((apply-code-action (chosen first-p)
                    (eglot--dcase chosen
                      (((Command) command arguments)
                       (eglot-execute-command server (intern command) arguments))
                      (((CodeAction) edit command)
                       (when edit (eglot--apply-workspace-edit edit))
                       (when command
                         (eglot--dbind ((Command) command arguments) command
                           (eglot-execute-command server (intern command) arguments)))
                       (when (and (eglot--server-capable :codeActionProvider
                                                         :resolveProvider)
                                  first-p)
                         (apply-code-action (eglot--request server :codeAction/resolve chosen) nil))))))
        (apply-code-action chosen t))))
  (advice-add #'eglot--read-execute-code-action :override #'eglot--read-execute-code-action@override))

;;; checking-grammar-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-grammar-rcp)
