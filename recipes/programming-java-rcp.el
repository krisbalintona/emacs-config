;;; programming-java-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to developing in Java.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'general)
(require 'keybinds-general-rcp)
(require 'keybinds-evil-rcp)
(require 'programming-projects-rcp)

;;; Lsp-java
(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
  (require 'helm)
  )

;;; Dap-java
(use-package dap-java
  :straight nil
  :general (:keymaps 'java-mode-map
                     "C-c C-c" '(dap-java-debug :wk "Dap-java-debug"))
;;; Helm-lsp
;; Helm interface -- really the only option...
(use-package helm-lsp
  :after 'lsp
  )

;;; programming-java-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-java-rcp)
