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
  :hook ((eglot-managed-mode . eglot-inlay-hints-mode) ; Only available if server supports it
         (eglot-managed-mode . (lambda ()
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
  (:keymaps 'eglot-mode-map
            :prefix "C-c e"
            "r" 'eglot-rename
            "a" 'eglot-code-actions
            "=" 'eglot-format
            "s" 'eglot-shutdown
            "S" 'eglot-shutdown-all)
  (:keymaps 'eglot-mode-map
            (general-chord "``") 'eglot-code-actions)
  :custom
  ;; NOTE 2023-07-11: Set to 0 if I want no events printed to a buffer so that
  ;; performance is increased
  (eglot-events-buffer-size 2000000)
  (eglot-connect-timeout 10)
  (eglot-autoreconnect 3)
  (eglot-sync-connect 3)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.7)
  (eglot-extend-to-xref nil)
  (eglot-report-progress t)
  :custom-face
  (eglot-highlight-symbol-face ((t (:box (:line-width -1 :style nil)))))
  :config
  ;; Not a `defcustom', so use `setq'
  (setq eglot-stay-out-of '("flymake")))

;;; Eglot-booster
;; Boosts Eglot's communication with the server. There's also a version for LSP.
;; FIXME 2024-01-10: UNTESTED. See
;; https://github.com/jdtsmith/eglot-booster?tab=readme-ov-file#installusage
(use-package eglot-booster
  ;; NOTE 2024-01-10: Must install the `emacs-lsp-booster' binary from
  ;; https://github.com/blahgeek/emacs-lsp-booster/releases
  :elpaca (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :init
  (eglot-booster-mode)
  :config
  (unless (executable-find "emacs-lsp-booster")
    (message "Please install the `emacs-lsp-booster' executable!")))

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
  :demand t
  :requires consult
  :after eglot
  :general (:keymaps 'eglot-mode-map
                     [remap xref-find-apropos] #'consult-eglot-symbols))

;;; programming-eglot-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-eglot-rcp)
