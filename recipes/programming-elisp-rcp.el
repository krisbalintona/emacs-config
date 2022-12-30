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

;;; Lisp-keyword-indent
;; Better keyword, cl-defun, and cl-loop indenting. See
;; https://github.com/twlz0ne/lisp-keyword-indent.el#usage for examples
(use-package lisp-keyword-indent
  :demand
  :straight (lisp-keyword-indent :type git
                                 :host github
                                 :repo "twlz0ne/lisp-keyword-indent.el")
  :functions kb/lisp-keyword-indent-mode
  :init
  (defvar-local kb/lisp-keyword-indent-allow nil
    "Whether to allow `lisp-keyword-indent-mode' in current buffer.")
  (define-minor-mode kb/lisp-keyword-indent-mode
    "Minor mode for keyword indent of Emacs Lisp."
    :init-value nil
    :lighter ""
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
  (eros-eval-result-prefix "⟹  ")       ; Fancy!
  )

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
  :straight nil
  :general ("C-h M-k" 'describe-keymap)
  :custom
  (describe-bindings-outline t)
  (help-enable-variable-value-editing t)
  (help-window-keep-selected t))

;;; Help-find
;; Provides `help-find-function' and `help-find-keybinding'
(use-package help-find
  :general
  (:keymaps 'help-map
   "uu" 'help-find-function
   "ui" 'help-find-keybinding))

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
  :demand t
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;; Suggest
;; Query `suggest' for elisp coding suggestions!
(use-package suggest
  :general ("C-c S" '(suggest :wk "Suggest.el buffer")))

;;; programming-elisp-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-elisp-rcp)
