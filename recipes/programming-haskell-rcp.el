;;; programming-haskell-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to using Haskell.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'keybinds-general-rcp)

;;; Haskell-mode
(use-package haskell-mode
  :hook ((haskell-mode . turn-on-haskell-doc-mode)
         ;; hslint on the command line only likes this indentation mode;
         ;; alternatives commented out below.
         (haskell-mode . turn-on-haskell-indentation)
         ;; (haskell-mode . turn-on-haskell-indent)
         ;; (haskell-mode . turn-on-haskell-simple-indent)
         ))

;;; Lsp-haskell
(use-package lsp-haskell
  :after lsp-mode
  :hook ((haskell-mode . lsp)
         (haskell-literate-mode . lsp)))

;;; programming-haskell-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-haskell-rcp)
