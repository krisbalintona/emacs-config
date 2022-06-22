;;; programming-haskell-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to developing in Haskell.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'general)
(require 'keybinds-general-rcp)

;;; Haskell-mode
(use-package haskell-mode
  ;; :hook (haskell-mode . lsp-deferred)
  ;; :hook (haskell-mode . eglot-ensure)
  :init (require 'haskell-mode-autoloads))

;;; programming-haskell-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-haskell-rcp)
