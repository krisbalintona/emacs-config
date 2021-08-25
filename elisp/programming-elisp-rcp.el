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

;;;; Custom font lock words
;; Mainly for `prot-comment-timestamp-keyword'. Faces taken from
;; `org-todo-keyword-faces' in org-agenda-general-rcp.el.
(font-lock-add-keywords 'emacs-lisp-mode
                        '(;; TODO wrapped between whitespace
                          ("\\s-TODO\\s-" 0 '(t :foreground "orange") t)
                          ;; NOTE wrapped between whitespace
                          ("\\s-NOTE\\s-" 0 '(t :foreground "turquoise") t)
                          ;; REVIEW wrapped between whitespace
                          ("\\s-REVIEW\\s-" 0 '(t :foreground "orchid") t)
                          ;; FIXME wrapped between whitespace
                          ("\\s-FIXME\\s-" 0 '(t :foreground "deep pink") t)
                          ))

;;; programming-elisp-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-elisp-rcp)
