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
  :custom
  (rustic-format-on-save nil)           ; I use apheleia
  (rustic-lsp-client 'eglot)
  )

;;; programming-rust-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-rust-rcp)
