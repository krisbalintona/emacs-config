;;; programming-haskell-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to developing in Haskell.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'general)
(require 'keybinds-frameworks-rcp)

;;;; Haskell-mode
(use-package haskell-mode
  :init (require 'haskell-mode-autoloads)
  )

;;; programming-haskell-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-haskell-rcp)
