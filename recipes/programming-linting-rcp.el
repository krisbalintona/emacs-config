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
              flycheck-command-map))

;;; Consult-flycheck
;; List flycheck errors in minibuffer with consult
(use-package consult-flycheck
  :after (consult flycheck)
  :general (:keymaps 'lsp-mode-map
                     "C-c e" '(consult-flycheck :wk "Consult flycheck")))

;;; Flymake
(use-package flymake
  :diminish
  :ghook 'prog-mode-hook 'org-mode-hook
  :hook (eglot-managed-mode . (lambda ()
                                (cond
                                 ((eglot--managed-mode)
                                  (setq-local flymake-show-diagnostics-at-end-of-line t))
                                 (t
                                  (setq-local flymake-show-diagnostics-at-end-of-line nil)))))
  :general
  ([remap flymake-show-buffer-diagnostics] 'consult-flymake)
  (:keymaps 'flymake-mode-map
            "M-n" 'flymake-goto-next-error
            "M-p" 'flymake-goto-prev-error)
  :custom
  (flymake-wrap-around nil)
  (flymake-fringe-indicator-position nil) ; Disable fringe indicators
  (flymake-show-diagnostics-at-end-of-line nil) ; I enable this selectively via a hook
  (flymake-mode-line-format
   '(flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format
   '(:eval (when (cl-plusp (length (flymake-diagnostics)))
             '(flymake-mode-line-error-counter
               flymake-mode-line-warning-counter
               flymake-mode-line-note-counter))))
  (elisp-flymake-byte-compile-load-path
   (append (list
            "./"
            (when (featurep 'straight) (straight--dir)))
           load-path)))

;;; Flymake-collection
(use-package flymake-collection
  :requires flymake
  :ensure-system-package luacheck
  :hook (elpaca-after-init . flymake-collection-hook-setup))

;;; Flymake-flycheck
;; For extending flycheck checkers into flymake. This allows flymake to use
;; flycheck backends; check readme on how to do so. I use this when only
;; flycheck is available (e.g. `lsp-mode'), otherwise I try to rely on
;; `flymake-collection'.
(use-package flymake-flycheck
  ;; As the readme warns, "Flycheck UI packages will have no idea of what the
  ;; checkers are doing, because they are run without flycheck's coordination."
  :hook (flycheck-mode . kb/enable-flymake-flycheck)
  :init
  (defun kb/enable-flymake-flycheck ()
    (when flycheck-mode (flycheck-mode -1))
    (flymake-mode 1)
    ;; Existing flymake backends take precedence over the flycheck ones here.
    ;; Reverse order of the append if this isn't desired
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        (flymake-flycheck-all-chained-diagnostic-functions))))
  :config
  ;; Disable flycheck checkers for which we have flymake equivalents
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)))))

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
