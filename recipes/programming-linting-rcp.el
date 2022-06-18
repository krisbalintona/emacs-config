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
  :after (consult flycheck)
  :general (kb/nav-keys
             "e" '(consult-flycheck :wk "Consult flycheck")))

;;; Flymake
(use-package flymake
  :ghook 'prog-mode-hook 'text-mode-hook
  :general (kb/nav-keys
             "e" '(consult-flymake :wk "Consult flymake"))
  ;; :custom
  ;; ;; Yes, I want my copies in the same dir as the original.
  ;; (flymake-run-in-place t)

  ;; ;; Nope, I want my copies in the system temp dir.
  ;; (flymake-run-in-place nil)
  ;; ;; This lets me say where my temp dir is.
  ;; (temporary-file-directory "~/.emacs.d/tmp/")

  ;; (flymake-max-parallel-syntax-checks 8)

  ;; ;; I don't want no steekin' limits.
  ;; (flymake-max-parallel-syntax-checks nil)
  )

;;; Flymake-collection
(use-package flymake-collection
  :requires flymake
  :hook (after-init . flymake-collection-hook-setup))

;;; programming-linting-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-linting-rcp)
