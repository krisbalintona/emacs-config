;;; programming-linting-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Everything to do with checking syntax and forseeing errors.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'personal-variables-rcp)

;;; Flycheck
;; Check your code
(use-package flycheck
  :demand t
  :gfhook
  '(lambda ()
      (when flymake-mode
        (flymake-mode -1)))
  :general
  (kb/nav-keys
    "E" '(flycheck-list-errors :wk "List flycheck errors"))
  (:keymaps 'flycheck-mode-map
   "M-n" 'flycheck-next-error
   "M-p" 'flycheck-previous-error)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit) ; Use load-path for Emacs session

  (flycheck-check-syntax-automatically
   '(save mode-enabled idle-change idle-buffer-switch)) ; When to check
  (flycheck-idle-buffer-switch-delay 1.5) ; Wait 1.5 second after buffer switch

  ;; Don't create temp files in current directory, create in /tmp/
  (flycheck-temp-prefix "/tmp/flycheck")

  (flycheck-relevant-error-other-file-show nil) ; Errors from other files?
  (flycheck-display-errors-delay 0.5)           ; Time to show an error on point
  (flycheck-indication-mode 'right-margin)
  (flycheck-highlighting-mode 'symbols)
  :config
  ;; Set prefix map
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "H-f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
              flycheck-command-map)

  (global-flycheck-mode))

;;; Consult-flycheck
;; List flycheck errors in minibuffer with consult
(use-package consult-flycheck
  :after (consult flycheck)
  :general (:keymaps 'lsp-mode-map
            "C-c e" '(consult-flycheck :wk "Consult flycheck")))

;;; Flymake
(use-package flymake
  :ghook 'prog-mode-hook 'text-mode-hook
  :gfhook
  '(lambda ()
      (when flycheck-mode
        (flycheck-mode -1)))
  :general
  (:keymaps 'flymake-mode-map
   "M-n" 'flymake-goto-next-error
   "M-p" 'flymake-goto-prev-error)
  ("C-c e" '(flymake-show-buffer-diagnostics :wk "Consult flymake"))
  :custom
  (flymake-wrap-around nil)
  (flymake-fringe-indicator-position nil) ; Disable fringe indicators
  (flymake-mode-line-format
   '(flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format
   '("[" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter "]")))

;;; Flymake-collection
(use-package flymake-collection
  :requires flymake
  :ensure-system-package ((proselint . "pip install proselint")
                          (luacheck))
  :hook (after-init . flymake-collection-hook-setup))

;;; Package-lint-flymake
(use-package package-lint-flymake
  :commands package-lint-flymake
  :hook (emacs-lisp-mode . kb/package-lint-flymake-setup)
  :config
  (defun kb/package-lint-flymake-setup ()
    (unless (string-match-p (expand-file-name "recipes/" user-emacs-directory) default-directory)
      (add-hook 'flymake-diagnostic-functions #'package-lint-flymake nil t))))

;;; programming-linting-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-linting-rcp)
