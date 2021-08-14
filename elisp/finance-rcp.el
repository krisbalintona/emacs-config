;;; finance-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Finance-related packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Ledger-mode
(use-package ledger-mode
  :ensure-system-package (ledger . "sudo apt install ledger")
  :hook (ledger-mode . (lambda () (mixed-pitch-mode 0)))
  :custom
  (ledger-complete-in-steps t)
  )

;;; finance-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'finance-rcp)
