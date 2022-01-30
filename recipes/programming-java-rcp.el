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
  :general (:keymaps 'java-mode-map
                     "C-c C-c" '(lsp-java-build-project :wk "Lsp build project"))
  )

;;; Dap-java
(use-package dap-java
  :straight nil
  )

;;; programming-java-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-java-rcp)
