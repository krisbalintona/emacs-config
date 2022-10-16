;;; programming-rust-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to using Rust.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'keybinds-general-rcp)

;;; Rustic
;; Rust-mode but with improvements for IDE experience (e.g. lsp-mode and eglot
;; integration)
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-client 'lsp-mode)
  (rustic-format-on-save nil)           ; I use apheleia
  )

;;; programming-rust-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-rust-rcp)
