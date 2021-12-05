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
  )

;;; Syntax highlighting
;;;; Lisp-extra-font-lock
;; Give faces to elisp symbols
(use-package lisp-extra-font-lock
  :ghook 'emacs-lisp-mode-hook
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
  :general
  ;; NOTE 2021-08-20: Emacs' describe-function includes both functions and
  ;; macros
  ([remap describe-function] '(helpful-callable :which-key "Helpful function")
   [remap describe-command] '(helpful-command :which-key "Helpful command")
   [remap describe-variable] '(helpful-variable :which-key "Helpful variable")
   [remap describe-symbol] '(helpful-symbol :which-key "Helpful symbol")
   [remap describe-key] '(helpful-key :which-key "Helpful key")
   [remap apropos-command] '(helpful-command :which-key "Helpful command")
   )
  (:states '(visual normal motion)
           "f" 'helpful-at-point
           )
  (kb/help-keys
    "K" '(describe-key-briefly :which-key "Desc key echo")
    "a" '(apropos-command :which-key "Apropos command")
    "f" '(describe-function :which-key "Desc func")
    "ha" '(consult-apropos :which-key "Consult apropos")
    "k" '(helpful-key :which-key "Desc key")
    "o" '(describe-symbol :which-key "Desc sym")
    "v" '(describe-variable :which-key "Desc var")
    "w" '(where-is :which-key "Where is...?")
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
