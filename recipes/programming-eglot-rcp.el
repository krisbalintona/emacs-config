;;; programming-eglot-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All configuration related to eglot.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Eglot
(use-package eglot
  :hook ((eglot-managed-mode . (lambda ()
                                 "Configure `eldoc'"
                                 ;; Use `eglot--setq-saving' to restore original
                                 ;; values. Make sure "eldoc," or a similar
                                 ;; regexp, isn't in `eglot-stay-out-of'
                                 (when (bound-and-true-p eldoc-box-hover-mode)
                                   (eglot--setq-saving eldoc-box-cleanup-interval 2))
                                 (eglot--setq-saving eldoc-echo-area-use-multiline-p nil)))
         (eglot-managed-mode . (lambda ()
                                 "Add `eglot-flymake-backend' to the beginning of
`flymake-diagnostic-functions', appending to the original
functions."
                                 (push (cons 'flymake-diagnostic-functions flymake-diagnostic-functions)
                                       eglot--saved-bindings) ; Manually add to saved values
                                 (add-to-list 'flymake-diagnostic-functions 'eglot-flymake-backend))))
  :general
  ("C-c e e" 'eglot)
  (:keymaps 'eglot-mode-map
            :prefix "C-c e"
            "r" 'eglot-rename
            "a" 'eglot-code-actions
            "=" 'eglot-format
            "R" 'eglot-reconnect
            "s" 'eglot-shutdown
            "S" 'eglot-shutdown-all)
  (:keymaps 'eglot-mode-map
            (general-chord "``") 'eglot-code-actions)
  :custom
  ;; NOTE 2023-07-11: Set to 0 if I want performance in exchange for no events
  ;; printed to a buffer
  (eglot-events-buffer-config '(:size 2000000 :format full))
  (eglot-connect-timeout 15)
  (eglot-autoreconnect 3)
  (eglot-sync-connect 3)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  (eglot-report-progress 'messages)
  (eglot-ignored-server-capabilities '(:inlayHintProvider)) ; Disable inlay hints globally
  :custom-face
  (eglot-highlight-symbol-face ((t (:box (:line-width -1 :style nil)))))
  :config
  ;; Not a `defcustom', so use `setq'
  (setq eglot-stay-out-of '("flymake")))

;;; Eglot-booster
;; Boosts Eglot's communication with the server. There's also a version for LSP.
;; FIXME 2024-02-03: Commit 4f017f5f0e breaks eglot-booster because of changes
;; to `eglot-alternatives'. Maybe check in later.
(use-package eglot-booster
  ;; NOTE 2024-01-10: Must install the `emacs-lsp-booster' binary from
  ;; https://github.com/blahgeek/emacs-lsp-booster/releases
  :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :init
  (eglot-booster-mode))

;;; Languages
;;;; Eglot-java
(use-package eglot-java
  :ensure-system-package jdtls
  :requires eglot
  :hook ((java-mode . eglot-ensure))
  :custom
  (eglot-java-server-install-dir (no-littering-expand-var-file-name "eglot-java/"))
  (eglot-java-workspace-folder (expand-file-name eglot-java-server-install-dir "workspace/"))
  (eglot-java-eclipse-jdt-args (list "--enable-preview"))
  (eglot-java-prefix-key "C-c e")
  (eglot-java-default-bindings-enabled t) ; See `eglot-java--setup'
  :config
  (eglot-java-init))                    ; Add to hook to `java-mode-hook'

;;; Consult-eglot
;; Equivalent to `consult-lsp'; adds `consult-eglot-symbols'.
(use-package consult-eglot
  :after eglot
  :general (:keymaps 'eglot-mode-map
                     [remap xref-find-apropos] #'consult-eglot-symbols))

;;; programming-eglot-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-eglot-rcp)
