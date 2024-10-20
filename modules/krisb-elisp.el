;;; Documentation
;;;; Help
(use-package help
  :ensure nil
  :bind ("C-h C-k" . describe-keymap)
  :custom
  (help-window-select t)
  (help-window-keep-selected t)

  (help-enable-variable-value-editing t)
  (help-clean-buttons t)
  (help-enable-symbol-autoload t)

  (describe-bindings-outline t)
  (describe-bindings-show-prefix-commands t)
  :config
  (add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function))

;;;; Apropos
(use-package apropos
  :ensure nil
  :bind ("C-h u" . apropos-user-option))

;;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :config
  (add-hook 'help-fns-describe-function-functions #'elisp-demos-advice-describe-function-1)

  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;; Debugging
;;;; Eros-mode
;; Overlay lisp evaluations into the current buffer (near cursor)
(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  :custom
  (eros-eval-result-prefix "⟹  "))

;;;; Inspector
;; Introspect list expressions. Also integrates with the debugging backtrace and
;; edebug (see
;; https://github.com/mmontone/emacs-inspector?tab=readme-ov-file#from-the-emacs-debugger).
(use-package inspector)

;;;; IELM
(use-package ielm
  :ensure nil
  :custom
  (ielm-noisy nil)
  (ielm-dynamic-return nil))

;;;; Edebug
(use-package edebug
  :ensure nil
  :custom
  (edebug-initial-mode 'step)
  :config
  ;; Better indication for evaluated sexps in during edebugging. Taken from
  ;; https://xenodium.com/inline-previous-result-and-why-you-should-edebug/.

  (with-eval-after-load 'eros
    (defun krisb-edebug-previous-result--around (_ &rest r)
      "Adviced `edebug-previous-result'."
      (require 'eros)
      (eros--make-result-overlay edebug-previous-result
        :where (point)
        :duration eros-eval-result-duration))
    (advice-add #'edebug-previous-result :around #'krisb-edebug-previous-result--around))

  (defun krisb-edebug-compute-previous-result (previous-value)
    (if edebug-unwrap-results
        (setq previous-value
              (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
          (concat "Result: "
                  (edebug-safe-prin1-to-string previous-value)
                  (eval-expression-print-format previous-value))))

  (defun edebug-previous-result ()
    "Print the previous result."
    (interactive)
    (message "%s" edebug-previous-result))

  (defun adviced:edebug-compute-previous-result (_ &rest r)
    "Adviced `krisb-edebug-compute-previous-result'."
    (let ((previous-value (nth 0 r)))
      (if edebug-unwrap-results
          (setq previous-value
                (edebug-unwrap* previous-value)))
      (setq edebug-previous-result
            (edebug-safe-prin1-to-string previous-value))))
  (advice-add #'krisb-edebug-compute-previous-result :around #'adviced:edebug-compute-previous-result))

;;; Packages
;;;; Package-lint-flymake
(use-package package-lint-flymake
  :hook (emacs-lisp-mode . package-lint-flymake-setup))

;;;; Try
;; Install a package only for the current Emacs session.
(use-package try)

;;; Provide
(provide 'krisb-elisp)
