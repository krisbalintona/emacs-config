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
  :config
  ;; Redefine the checker to use `ledger-master-file' rather than
  ;; `source-inplace' (current buffer)
  (flycheck-define-checker ledger
    "A checker for ledger files, showing unmatched balances and failed checks."
    :command ("ledger"
              (option-flag "--explicit" flycheck-ledger-explicit)
              (option-flag "--pedantic" flycheck-ledger-pedantic)
              (eval (when (eq flycheck-ledger-pedantic 'check-payees) "--check-payees"))

              ;; "-f" source-inplace
              "-f" (eval ledger-master-file)
              ;; (setq-local ledger-master-file (concat no-littering-var-directory "ledger/2020.ledger"))

              "balance"
              ;; to find non-zero zero accounts:
              "--flat" "--no-total"
              "--balance-format" "%(scrub(display_total))\t\t%(account())\n"
              (eval flycheck-ledger-zero-accounts))
    :error-patterns
    ((error line-start "While parsing file \"" (file-name) "\", line " line ":" (zero-or-more whitespace) "\n"
            (zero-or-more line-start (or "While " "> ") (one-or-more not-newline) "\n" )
            (message (minimal-match (zero-or-more line-start (zero-or-more not-newline) "\n"))
                     "Error: " (one-or-more not-newline) "\n")))
    :error-parser
    (lambda (output checker buffer)
      (let ((pattern-errors (flycheck-parse-with-patterns output checker buffer)))
        (or pattern-errors
            (when (> (length flycheck-ledger-zero-accounts) 0)
              (flycheck-ledger--zero-error-parser output checker buffer)))))
    :verify
    (lambda (checker)
      (let ((has-accounts (> (length flycheck-ledger-zero-accounts) 0)))
        (list
         (flycheck-verification-result-new
          :label "accounts"
          :message (if has-accounts (format "%s" flycheck-ledger-zero-accounts) "none")
          :face 'success))))
    :modes ledger-mode)
  )

;;; finance-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'finance-rcp)
