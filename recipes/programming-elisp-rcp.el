;;; programming-elisp-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages that are helpful for programming or working in elisp.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'buffers-and-windows-rcp)

;;; Ielm
(use-package ielm
  :elpaca nil
  :custom
  (ielm-noisy nil)
  (ielm-dynamic-return nil))

;;; Lisp-keyword-indent
;; Better keyword, cl-defun, and cl-loop indenting. See
;; https://github.com/twlz0ne/lisp-keyword-indent.el#usage for examples
(use-package lisp-keyword-indent
  :demand
  :elpaca (lisp-keyword-indent :type git
                               :host github
                               :repo "twlz0ne/lisp-keyword-indent.el")
  :functions kb/lisp-keyword-indent-mode
  :init
  (defvar-local kb/lisp-keyword-indent-allow nil
    "Whether to allow `lisp-keyword-indent-mode' in current buffer.")
  (define-minor-mode kb/lisp-keyword-indent-mode
    "Minor mode for keyword indent of Emacs Lisp."
    :init-value nil
    :lighter (:eval (when (and kb/lisp-keyword-indent-mode
                               (derived-mode-p 'emacs-lisp-mode))
                      (propertize " LKI" 'face '(:inherit shadow))))
    :keymap nil
    :global t
    (if kb/lisp-keyword-indent-mode
        (advice-add 'lisp-indent-function :override 'lisp-keyword-indent)
      (when kb/lisp-keyword-indent-allow
        (advice-remove 'lisp-indent-function 'lisp-keyword-indent))))
  :config
  (kb/lisp-keyword-indent-mode))

;;; Eros-mode
;; Overlay lisp evaluations into the current buffer (near cursor)
(use-package eros
  :ghook 'emacs-lisp-mode-hook
  :custom
  (eros-eval-result-prefix "⟹  "))      ; Fancy!

;;; Syntax highlighting
;;;; Lisp-extra-font-lock
;; Give faces to elisp symbols
(use-package lisp-extra-font-lock
  :disabled t
  :init
  (lisp-extra-font-lock-global-mode))

;;;; Highlight-function-calls
;; Give function calls a special face (default is underline)
(use-package highlight-function-calls
  :disabled t
  :ghook 'emacs-lisp-mode-hook)

;;;; Rainbow-delimiters
;; Highlight matching delimiters (e.g. parenthesis)
(use-package rainbow-delimiters
  :ghook 'prog-mode-hook)

;;; Help
(use-package help
  :elpaca nil
  :general ("C-h M-k" 'describe-keymap)
  :custom
  (help-window-select t)
  (help-window-keep-selected t)

  (help-enable-variable-value-editing t)
  (help-clean-buttons t)

  (describe-bindings-outline t)
  (describe-bindings-show-prefix-commands t)
  :config
  (add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function))

;;; Help-find
;; Provides `help-find-function' and `help-find-keybinding'
(use-package help-find
  :general
  (:keymaps 'help-map
   :prefix "M-f"
   "f" 'help-find-function
   "r" 'help-find-keybinding))

;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
  :disabled                          ; Trying out built-in `help' functionality
  :gfhook 'visual-line-mode
  :general
  (:keymaps 'helpful-mode-map
   (general-chord "jj") 'helpful-at-point)
  ;; NOTE 2021-08-20: Emacs' describe-function includes both functions and
  ;; macros
  ([remap describe-function] 'helpful-function
   [remap describe-command] 'helpful-command
   [remap describe-variable] 'helpful-variable
   [remap describe-symbol] 'helpful-symbol
   [remap describe-key] 'helpful-key
   [remap apropos-command] 'helpful-command))

;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :demand
  :config
  (add-hook 'help-fns-describe-function-functions #'elisp-demos-advice-describe-function-1)
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;; Suggest
;; Query `suggest' for elisp coding suggestions!
(use-package suggest
  :general (kb/open-keys
             "S" 'suggest)
  :custom
  (suggest-insert-example-on-start nil))

;;; programming-elisp-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-elisp-rcp)
