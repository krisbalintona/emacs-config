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
  :demand t
  :straight (lisp-keyword-indent :type git
                                 :host github
                                 :repo "twlz0ne/lisp-keyword-indent.el")
  :config
  (defvar-local kb/lisp-keyword-indent-allow nil
    "Whether to allow `lisp-keyword-indent-mode' in current buffer.")
  (define-globalized-minor-mode global-lisp-keyword-indent-mode
    lisp-keyword-indent-mode
    (lambda ()
      (when kb/lisp-keyword-indent-allow
        (lisp-keyword-indent-mode 1))))

  (global-lisp-keyword-indent-mode))

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

;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
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
