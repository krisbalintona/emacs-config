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

;;; Elisp-mode
;; Elisp-mode overwrites my eyebrowse-last-window-config binding
(use-package elisp-mode
  :straight nil
  )

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
  :init
  (lisp-extra-font-lock-global-mode)
  )

;;;; Highlight-function-calls
;; Give function calls a special face (default is underline)
(use-package highlight-function-calls
  :ghook 'emacs-lisp-mode-hook
  )

;;;; Rainbow-delimiters
;; Highlight matching delimiters (e.g. parenthesis)
(use-package rainbow-delimiters
  :ghook 'prog-mode-hook
  )

;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
  :gfhook 'visual-line-mode
  :ryo
  ("f" helpful-at-point)
  :general
  ;; NOTE 2021-08-20: Emacs' describe-function includes both functions and
  ;; macros
  ([remap describe-function] '(helpful-callable :wk "Helpful function")
   [remap describe-command] '(helpful-command :wk "Helpful command")
   [remap describe-variable] '(helpful-variable :wk "Helpful variable")
   [remap describe-symbol] '(helpful-symbol :wk "Helpful symbol")
   [remap describe-key] '(helpful-key :wk "Helpful key")
   [remap apropos-command] '(helpful-command :wk "Helpful command")
   )
  (:states '(visual normal motion)
           "f" 'helpful-at-point
           )
  (kb/help-keys
    "K" '(describe-key-briefly :wk "Desc key echo")
    "a" '(apropos-command :wk "Apropos command")
    "f" '(describe-function :wk "Desc func")
    "ha" '(consult-apropos :wk "Consult apropos")
    "k" '(helpful-key :wk "Desc key")
    "o" '(describe-symbol :wk "Desc sym")
    "v" '(describe-variable :wk "Desc var")
    "w" '(where-is :wk "Where is...?")
    )
  :custom
  (describe-bindings-outline t) ; Include interactive outline headings for each major mode in `describe-keys' buffer
  )

;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :demand t
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  )

;;; programming-elisp-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-elisp-rcp)
