;;; programming-elisp-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages that are helpful for programming or working in elisp.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)
(require 'buffer-and-window-management-rcp)

;;;; Elisp-mode
;; Elisp-mode overwrites my eyebrowse-last-window-config binding
(use-package elisp-mode
  :straight nil
  :general
  (:keymaps 'emacs-lisp-mode-map
            :states '(normal visual motion)
            "gz" nil)
  )

;;;; Eros-mode
;; Overlay lisp evaluations into the current buffer (near cursor)
(use-package eros
  :ghook 'emacs-lisp-mode-hook
  )

;;;; Syntax highlighting
;;;;; Lisp-extra-font-lock
;; Give faces to elisp symbols
(use-package lisp-extra-font-lock
  :ghook 'emacs-lisp-mode-hook
  )

;;;;; Highlight-function-calls
;; Give function calls a special face (default is underline)
(use-package highlight-function-calls
  :ghook 'emacs-lisp-mode-hook
  )

;;;;; Rainbow-delimiters
;; Highlight matching delimiters (e.g. parenthesis)
(use-package rainbow-delimiters
  :ghook 'prog-mode-hook
  )

;;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :requires helpful
  :commands elisp-demos-advice-helpful-update
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  )

;;; programming-elisp-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-elisp-rcp)
