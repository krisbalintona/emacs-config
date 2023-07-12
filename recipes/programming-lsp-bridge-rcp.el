;;; programming-lsp-bridge-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All configuration related to lsp-bridge.
;;
;;;;;;;;p;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Lsp-bridge
;; Minimal LSP client whose benefit is asynchrony
(use-package lsp-bridge
  :elpaca (lsp-bridge :type git
                      :host github
                      :repo "manateelazycat/lsp-bridge"
                      :files (:defaults "*.py" "langserver" "acm"))
  :hook (lsp-bridge-mode . (lambda ()
                             "Disable `eglot' and `corfu' when enabling `lsp-bridge-mode'."
                             (when (bound-and-true-p eglot--managed-mode)
                               (corfu-mode 0)
                               (eglot-shutdown (eglot-current-server)))))
  :config
  ;; (global-lsp-bridge-mode)
  )

;;; programming-lsp-bridge-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-lsp-bridge-rcp)
