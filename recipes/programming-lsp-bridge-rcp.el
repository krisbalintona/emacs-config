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
  :straight (lsp-bridge :type git
                        :host github
                        :repo "manateelazycat/lsp-bridge"
                        :files (:defaults "*.py" "langserver" "acm"))
  :config
  ;; (global-lsp-bridge-mode)
  )

;;; programming-lsp-bridge-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-lsp-bridge-rcp)
