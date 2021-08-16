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
  :hook (after-save . kb/ledger-add-blank-lines) ; Add a blank line to the end of every xact
  :functions kb/ledger-add-blank-lines
  :gfhook
  #'outshine-mode
  '(lambda () (mixed-pitch-mode 0))
  '(lambda () (display-line-numbers-mode 0))
  :custom
  ;; Administration
  ;; (ledger-source-directory (concat no-littering-var-directory "ledger/source/"))
  ;; (ledger-init-file-name (concat no-littering-var-directory "ledger/ledgerrc.ledger"))
  (ledger-accounts-file (concat no-littering-var-directory "ledger/accounts.ledger"))
  (ledger-schedule-file (concat no-littering-var-directory "ledger/schedule.ledger"))

  ;; .ledger files (ledger-complete-in-steps t)
  (ledger-post-auto-align t)
  (ledger-post-amount-alignment-column 55)
  (ledger-copy-transaction-insert-blank-line-after nil)
  (ledger-reconcile-finish-force-quit t)

  ;; Reconcile
  (ledger-narrow-on-reconcile nil)

  ;; Reports
  (ledger-reports
   '(("bal this month" "%(binary) -f %(ledger-file) bal -p %(month) -S amount")
     ("bal 2020"       "%(binary) -f %(ledger-file) bal -p 2020")
     ("bal"            "%(binary) -f %(ledger-file) bal")
     ("reg monthly"    "%(binary) -f %(ledger-file) reg -M")
     ("reg"            "%(binary) -f %(ledger-file) reg")
     ("accounts"       "%(binary) -f %(ledger-file) accounts")
     ("account"        "%(binary) -f %(ledger-file) reg %(account)"))
   )
  (ledger-report-auto-refresh nil) ; Don't continually bother me with a new window
  (ledger-report-resize-window nil)
  (ledger-report-use-strict t) ; Adds `--strict' flag, make sure accounts are already defined
  (ledger-report-auto-refresh-sticky-cursor t) ; Don't lose cursor position on auto-refresh
  :init
  (defun kb/ledger-add-blank-lines ()
    "Add a line to the end of every xact entry for visual clarity."
    (interactive)
    (if (string= major-mode "ledger-mode")
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (if (not (ledger-navigate-start-xact-or-directive-p))
                (ledger-navigate-next-xact))
            (while (not (equal (point-max) (point)))
              (ledger-navigate-end-of-xact)
              (if (not (looking-at "\n\n")) (insert "\n"))
              (ledger-navigate-next-xact))
            ))
      )
    )
  :config
  (setq-local tab-always-indent nil ; Indent first then complete
              completion-cycle-threshold t)

  ;; Administration
  (setq-default ledger-master-file (concat no-littering-var-directory "ledger/master.ledger"))

  (add-to-list 'evil-emacs-state-modes 'ledger-reconcile-mode)

  (general-define-key
   :keymaps 'ledger-mode-map
   "C-c C-t" '(ledger-toggle-current :which-key "Toggle check on current")
   [remap consult-flycheck] '(list-flycheck-errors :which-key "List flycheck errors")
   )
  (general-define-key
   :keymaps 'ledger-mode-map
   :state 'insert
   "TAB" 'tab-to-tab-stop
   )
  (kb/leader-keys
    :keymaps 'ledger-mode-map
    )

  (general-unbind ; Doesn't kill window as it should
    :keymaps 'ledger-report-mode-map
    :states '(normal visual motion)
    "q"
    )
  (general-define-key
   :keymaps 'ledger-report-mode-map
   :states '(normal visual)
   "RET" '(ledger-report-visit-source :which-key "Visit transaction")
   "C-c C-o C-k" '(ledger-report-quit :which-key "Quit")
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
