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
  :ensure-system-package (ledger . "sudo dnf install ledger")
  :hook ((ledger-mode . (lambda () (outshine-mode) (mixed-pitch-mode 0)))
         (ledger-mode . (lambda ()
                          (setq-local tab-always-indent 'complete)
                          (setq-local completion-cycle-threshold t)
                          ))
         )
  :custom
  ;; Administration
  (ledger-accounts-file (concat no-littering-var-directory "ledger/accounts"))

  ;; .ledger files
  (ledger-complete-in-steps t)
  (ledger-post-auto-align t)
  (ledger-post-amount-alignment-column 55)
  (ledger-copy-transaction-insert-blank-line-after t)
  (ledger-reconcile-finish-force-quit t)
  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-reconcile-mode)

  (general-define-key
   :keymaps 'ledger-mode-map
   "C-c C-t" '(ledger-toggle-current :which-key "Reconcile mode")
   )
  )

;;;; Flycheck-ledger
(use-package flycheck-ledger
  :after flycheck
  :custom
  (flycheck-ledger-explicit t) ; Check even cleared transactions
  (flycheck-ledger-pedantic t) ; Check account names
  )

;;; finance-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'finance-rcp)
