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
  :disabled t
  :general
  (kb/nav-keys
    "E" '(flycheck-list-errors :wk "List flycheck errors"))
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
  :disabled t
  :after (consult flycheck)
  :general (kb/nav-keys
             "e" '(consult-flycheck :wk "Consult flycheck")))

;;; Flymake
(use-package flymake
  :ghook 'prog-mode-hook 'text-mode-hook
  :general
  ("M-n" 'flymake-goto-next-error
   "M-p" 'flymake-goto-prev-error)
  (kb/nav-keys
    "e" '(consult-flymake :wk "Consult flymake"))
  :custom
  (flymake-fringe-indicator-position nil) ; Disable fringe indicators
  (flymake-mode-line-format
   '(flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format
   '("[" flymake-mode-line-error-counter flymake-mode-line-warning-counter "]")))

;;; Flymake-collection
(use-package flymake-collection
  :requires flymake
  :ensure-system-package ((proselint . "pip install proselint")
                          (luacheck))
  :hook (after-init . flymake-collection-hook-setup))

;;; Package-lint-flymake
(use-package package-lint-flymake
  :hook (emacs-lisp-mode . kb/package-lint-flymake-setup)
  :init
  (defun kb/package-lint-flymake-setup ()
    (when (string-match-p (expand-file-name "recipes/" user-emacs-directory) default-directory)
      (add-hook 'flymake-diagnostic-functions #'package-lint-flymake nil t))))

;;; programming-linting-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-linting-rcp)
