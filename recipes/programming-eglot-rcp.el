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
  :ensure-system-package (pyright bash-language-server)
  :hook (((python-mode lua-mode sh-mode js2-mode java-mode) . eglot-ensure)
         (eglot-managed-mode . (lambda ()
                                 (setq-local corfu-auto t
                                             corfu-auto-delay 0.1
                                             corfu-auto-prefix 1
                                             corfu-quit-at-boundary t
                                             orderless-component-separator "·"
                                             corfu-separator ?·)
                                 ;; Force re-enable `corfu-mode' in order for it
                                 ;; to be aware of the local change to
                                 ;; `corfu-auto'
                                 (corfu-mode 1))))
  :custom
  (eglot-autoshutdown t)
  (eglot-stay-out-of '("flymake"))
  :config
  (setf (alist-get 'python-mode eglot-server-programs)
        `("pyright-langserver" "--stdio")
        (alist-get 'lua-mode eglot-server-programs)
        '("lua-language-server"))

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              "Add `eglot-flymake-backend' to `flymake-diagnostic-functions',
preserving the initial list."
              (when (eglot-managed-p)
                (add-to-list 'flymake-diagnostic-functions 'eglot-flymake-backend)))))

;;; Languages
;;;; Eglot-java
(use-package eglot-java
  :ensure-system-package jdtls
  :requires eglot
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
  :requires eglot
  :general (:keymaps 'eglot-mode-map
                     [remap xref-find-apropos] #'consult-eglot-symbols))

;;; programming-eglot-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-eglot-rcp)
