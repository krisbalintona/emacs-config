;;; finance-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Finance-related packages.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Ledger-mode
(use-package ledger-mode
  :ensure-system-package (ledger . "sudo dnf install ledger")
  :defines evil-emacs-state-modes
  :commands (ledger-navigate-start-xact-or-directive-p ledger-navigate-end-of-xact ledger-navigate-next-xact ledger-read-commodity-with-prompt)
  :hook ((before-save . kb/ledger-add-blank-lines) ; Add a blank line to the end of every xact
         (calc-mode . evil-emacs-state))
  :gfhook
  'outshine-mode
  '(lambda () (mixed-pitch-mode 0)
     (display-line-numbers-mode 0)
     (setq-local tab-always-indent nil ; Indent first then complete
                 completion-cycle-threshold t
                 fill-column 90
                 comment-column fill-column)
     (setq-default ledger-master-file
                   (concat no-littering-var-directory "ledger/master.ledger"))
     )
  :general
  (:keymaps 'ledger-mode-map
            "C-c C-t" '(ledger-toggle-current :which-key "Toggle check on current")
            [remap consult-flycheck] '(list-flycheck-errors :which-key "List flycheck errors")
            )
  (:keymaps 'ledger-mode-map
            :states 'insert
            "TAB" 'tab-to-tab-stop)
  (kb/leader-keys
    :keymaps 'ledger-mode-map
    :states '(normal insert)
    "id" '(kb/insert-date :which-key "Insert date")
    "ie" '(ledger-insert-effective-date :which-key "Insert effective date")
    )
  (:keymaps 'ledger-report-mode-map
            :states '(normal visual motion)
            "q" nil) ; Doesn't kill window as it would normally
  (:keymaps 'ledger-report-mode-map
            :states '(normal visual)
            "RET" '(ledger-report-visit-source :which-key "Visit transaction")
            "C-c C-o C-k" '(ledger-report-quit :which-key "Quit")
            )
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
  (ledger-report-format-specifiers
   '(("ledger-file" . ledger-report-ledger-file-format-specifier)
     ("binary" . ledger-report-binary-format-specifier)
     ("payee" . ledger-report-payee-format-specifier)
     ("account" . ledger-report-account-format-specifier)
     ("month" . ledger-report-month-format-specifier)
     ("tagname" . ledger-report-tagname-format-specifier)
     ("tagvalue" . ledger-report-tagvalue-format-specifier)
     ("commodity" . kb/ledger-report-commodity-format-specifier)
     ))
  (ledger-reports
   '(("bal this month" "%(binary) -f %(ledger-file) bal -p %(month) -S amount")
     ("bal 2020"       "%(binary) -f %(ledger-file) bal -p 2020")
     ("bal"            "%(binary) -f %(ledger-file) bal")
     ("reg monthly"    "%(binary) -f %(ledger-file) reg -M")
     ("reg"            "%(binary) -f %(ledger-file) reg")
     ("account"        "%(binary) -f %(ledger-file) reg %(account)")
     ("account single commodity" "%(binary) -f %(ledger-file) reg %(account) --exchange %(commodity)"))
   )
  (ledger-report-auto-refresh nil) ; Don't continually bother me with a new window
  (ledger-report-resize-window nil)
  (ledger-report-use-strict t) ; Adds `--strict' flag, make sure accounts are already defined
  (ledger-report-auto-refresh-sticky-cursor t) ; Don't lose cursor position on auto-refresh
  :init
  ;; For `ledger-reports' expansions
  (defun kb/ledger-report-commodity-format-specifier ()
    "Substitute for a chosen (prompted) commodity."
    (ledger-read-commodity-with-prompt "Commodity?"))

  ;; Add blank lines
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
      ))
  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-reconcile-mode)

  (set-face-attribute 'ledger-font-xact-highlight-face nil :inherit nil)
  )

;;;; Flycheck-ledger
(use-package flycheck-ledger
  :demand t
  :requires flycheck
  :after (flycheck ledger-mode)
  :commands (flycheck-define-command-checker flycheck-parse-with-patterns flycheck-ledger--zero-error-parser flycheck-add-mode)
  :custom
  (flycheck-ledger-explicit t) ; Check even cleared transactions
  (flycheck-ledger-pedantic t) ; Check account names
  :config
  (flycheck-define-checker kb/ledger
    "Redefine the original checker to check `ledger-master-file' rather than
`source-inplace' (current buffer)

Original docstring:
A checker for ledger files, showing unmatched balances and failed checks."
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
              (eval flycheck-ledger-zero-accounts)
              )
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
  (add-to-list 'flycheck-checkers 'kb/ledger)
  )

;;; finance-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'finance-rcp)
