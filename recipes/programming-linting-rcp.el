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
(require 'convenient-functions-rcp)     ; For `kb/shell-command-to-string'

;;; Flycheck
;; Check your code
(use-package flycheck
  ;; :demand t
  :general
  (kb/nav-keys
    "E" '(flycheck-list-errors :which-key "List flycheck errors")
    )
  (kb/general-keys
    "?" '(flycheck-buffer :which-key "Flycheck buffer")
    )
  :custom
  (flycheck-emacs-lisp-load-path 'inherit) ; Use load-path for Emacs session

  (flycheck-check-syntax-automatically
   '(save mode-enabled idle-change idle-buffer-switch)) ; When to check
  (flycheck-idle-buffer-switch-delay 1.5) ; Wait 1.5 second after buffer switch

  ;; Don't create temp files in current directory, create in /tmp/
  (flycheck-temp-prefix "/tmp/flycheck")

  (flycheck-relevant-error-other-file-show nil) ; Errors from other files?
  (flycheck-display-errors-delay 0.5) ; Time to show an error on point
  (flycheck-indication-mode 'right-margin)
  (flycheck-highlighting-mode 'symbols)
  :config (global-flycheck-mode)
  )

;;; Flycheck-pos-tip-mode
;; Shows flycheck errors in pos-tip popup
(use-package flycheck-pos-tip
  :disabled t ; NOTE 2021-11-12: Causes Emacs to crash when using the `alpha-background' patch of Emacs
  :requires flycheck
  :ghook 'flycheck-mode-hook
  )

;;; Flycheck-status-emoji
;; Use emojis to display flycheck statuses
(use-package flycheck-status-emoji
  :requires flycheck
  :ghook 'flycheck-mode-hook
  )

;;; Consult-flycheck
;; List flycheck errors in minibuffer with consult
(use-package consult-flycheck
  :requires (consult flycheck)
  :general (kb/nav-keys
             "e" '(consult-flycheck :which-key "Consult flycheck"))
  )

;;; programming-linting-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-linting-rcp)
