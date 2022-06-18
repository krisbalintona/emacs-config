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
  :ensure-system-package pyright
  :hook ((python-mode lua-mode) . eglot-ensure)
  :config
  (setf (alist-get 'python-mode eglot-server-programs)
        `("pyright-langserver" "--stdio" "--tcp" "--host" "localhost" "--port" :autoport)
        (alist-get 'lua-mode eglot-server-programs)
        '("lua-language-server")))

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
  :after eglot
  :general (:keymaps 'eglot-mode-map
                     "C-c e" '(consult-eglot-symbols :wk "Consult-eglot")))

;;; programming-eglot-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-eglot-rcp)
