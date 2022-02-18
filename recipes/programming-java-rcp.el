;;; programming-java-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to developing in Java.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'keybinds-general-rcp)

;;; Lsp-java
(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :general (:keymaps 'java-mode-map
                     "H-b" 'lsp-java-build-project)
  :custom
  (lsp-java-project-referenced-libraries ["lib/**/*.jar" "src/**/*.jar"])
  )

;;; Dap-java
(use-package dap-java
  :straight nil
  :general (:keymaps 'java-mode-map
                     "C-c C-c" '(kb/dap-java-debug :wk "Dap-java-debug"))
  :config
  (defun kb/dap-java-debug ()
    "`dap-java-debug' but with the \"enable-preview\" vmArg."
    (interactive)
    (dap-java-debug (dap-java--populate-default-args (list :vmArgs "--enable-preview"))))

  ;; Copy and pasted from ~/main-emacs/straight/repos/lsp-java/dap-java.el
  (dap-register-debug-template "Java Run Configuration"
                               (list :type "java"
                                     :request "launch"
                                     :vmArgs "--enable-preview" ; Needed
                                     :args ""
                                     :cwd nil
                                     :stopOnEntry :json-false
                                     :host "localhost"
                                     :request "launch"
                                     :modulePaths (vector)
                                     :classPaths nil
                                     :projectName nil
                                     :mainClass nil)))

;;; Helm-lsp
;; Helm interface -- really the only option...
(use-package helm-lsp
  :after lsp-mode
  :config
  (require 'helm)
  )

;;; programming-java-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-java-rcp)
