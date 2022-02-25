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
  :hook ((java-mode . lsp-deferred)
         (java-mode . lsp-java-boot-lens-mode))
  :general (:keymaps 'java-mode-map
                     "H-b" 'lsp-java-build-project
                     "H-l j" '(lsp-jt-browser :wk "Lsp-jt"))
  :custom
  (lsp-java-inhibit-message t) ; Inhibit java messages echo via `inhibit-message'.
  (lsp-java-content-provider-preferred "fernflower")
  (lsp-java-signature-help-enabled t)

  ;; Code lenses
  (lsp-java-references-code-lens-enabled t)
  (lsp-java-implementations-code-lens-enabled t)

  ;; Current VSCode arguments; larger heap size to increase speed in larger
  ;; projects. According to https://github.com/emacs-lsp/lsp-java#faq
  (lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))
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
  :defer 4
  :after lsp-mode
  :config
  (require 'helm)
  )

;;; programming-java-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-java-rcp)
