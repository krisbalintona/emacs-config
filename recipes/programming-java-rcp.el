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
  :custom
  (lsp-java-project-referenced-libraries ["lib/**/*.jar" "src/**/*.jar"])
  :config
  (dap-register-debug-template
   "Java Run Configuration"
   (list :name "Java Run Configuration"
         :vmArgs "--enable-preview"     ; Needed
         :host "localhost"
         :console "internalConsole")))

;;; Dap-java
(use-package dap-java
  :straight nil
  :general (:keymaps 'java-mode-map
                     "C-c C-c" '((lambda () (dap-java-debug (dap-java--populate-default-args (list :vmArgs "--enable-preview"))))
                                 :wk "Dap-java-debug"))

  )

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
