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
  :requires lsp
  :hook ((java-mode . lsp-deferred)
         (java-mode . lsp-java-lens-mode)       ; For run/debug overlays
         (java-mode . lsp-java-boot-lens-mode)) ; For code-lens overlays
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
  :config
  ;; Disable multi-root functionality
  (lsp-register-client
   (make-lsp--client
    :new-connection (lsp-stdio-connection #'lsp-java--ls-command
                                          #'lsp-java--locate-server-jar)
    :major-modes '(java-mode jdee-mode)
    :server-id 'jdtls
    :multi-root nil                     ; Disable multi-root
    :notification-handlers (ht ("language/status" #'lsp-java--language-status-callback)
                               ("language/actionableNotification" #'lsp-java--actionable-notification-callback)
                               ("language/progressReport" #'lsp-java--progress-report)
                               ("workspace/notify" #'lsp-java--workspace-notify)
                               ("language/eventNotification" #'ignore))
    :request-handlers (ht ("workspace/executeClientCommand" 'lsp-java-boot--workspace-execute-client-command))
    :action-handlers (ht ("java.apply.workspaceEdit" #'lsp-java--apply-workspace-edit)
                         ("java.action.generateToStringPrompt" #'lsp-java--action-generate-to-string)
                         ("java.action.hashCodeEqualsPrompt" #'lsp-java--action-generate-equals-and-hash-code)
                         ("java.action.organizeImports" #'lsp-java--action-organize-imports)
                         ("java.action.overrideMethodsPrompt" #'lsp-java--override-methods-prompt)
                         ("java.action.generateAccessorsPrompt" #'lsp-java--generate-accessors-prompt)
                         ("java.action.generateConstructorsPrompt" #'lsp-java--generate-constructors-prompt)
                         ("java.action.applyRefactoringCommand" #'lsp-java--apply-refactoring-command)
                         ("java.action.rename" #'lsp-java--action-rename)
                         ("java.show.references" #'lsp-java--show-references)
                         ("java.show.implementations" #'lsp-java--show-implementations))
    :uri-handlers (ht ("jdt" #'lsp-java--resolve-uri))
    :initialization-options (lambda ()
                              (list :settings (lsp-configuration-section "java")
                                    :extendedClientCapabilities
                                    (list :progressReportProvider (lsp-json-bool lsp-java-progress-reports-enabled)
                                          :classFileContentsSupport t
                                          :classFileContentsSupport t
                                          :overrideMethodsPromptSupport t
                                          :hashCodeEqualsPromptSupport t
                                          :advancedOrganizeImportsSupport t
                                          :generateConstructorsPromptSupport t
                                          :generateToStringPromptSupport t
                                          :advancedGenerateAccessorsSupport t
                                          :advancedExtractRefactoringSupport t
                                          :moveRefactoringSupport t
                                          :resolveAdditionalTextEditsSupport t)
                                    :bundles (lsp-java--bundles)
                                    :workspaceFolders (->> (lsp-session)
                                                           lsp-session-server-id->folders
                                                           (gethash 'jdtls)
                                                           (-uniq)
                                                           (-map #'lsp--path-to-uri)
                                                           (apply #'vector))))
    :library-folders-fn (lambda (_workspace) (list lsp-java-workspace-cache-dir))
    :before-file-open-fn (lambda (_workspace)
                           (let ((metadata-file-name (lsp-java--get-metadata-location buffer-file-name)))
                             (setq-local lsp-buffer-uri
                                         (when (file-exists-p metadata-file-name)
                                           (with-temp-buffer (insert-file-contents metadata-file-name)
                                                             (buffer-string))))))
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                                          (lsp--set-configuration (lsp-configuration-section "java"))
                                          (lsp--server-register-capability
                                           (lsp-make-registration
                                            :id "test-id"
                                            :method "workspace/didChangeWatchedFiles"
                                            :register-options? (lsp-make-did-change-watched-files-registration-options
                                                                :watchers
                                                                (vector (lsp-make-file-system-watcher :glob-pattern "**/*.java")
                                                                        (lsp-make-file-system-watcher :glob-pattern "**/pom.xml")
                                                                        (lsp-make-file-system-watcher :glob-pattern "**/*.gradle")
                                                                        (lsp-make-file-system-watcher :glob-pattern "**/.project")
                                                                        (lsp-make-file-system-watcher :glob-pattern "**/.classpath")
                                                                        (lsp-make-file-system-watcher :glob-pattern "**/settings/*.prefs")))))))
    :completion-in-comments? t

    :download-server-fn #'lsp-java--ensure-server)))

;;; Dap-java
(use-package dap-java
  :requires dap-mode
  :elpaca nil
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
  (require 'helm))

;;; programming-java-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-java-rcp)
