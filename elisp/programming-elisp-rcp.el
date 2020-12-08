;;; programming-elisp-rcp.el --- Summary
;;
;; These are packages that are helpful for programming or working in elisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Lisp-extra-font-lock
;; Give faces to elisp symbols
(use-package lisp-extra-font-lock
  :hook (emacs-lisp-mode . (lambda ()
                             (lisp-extra-font-lock-mode)
                             (visual-line-mode)
                             ))
  )

;;;; Highlight-function-calls
;; Give function calls a special face (default is underline)
(use-package highlight-function-calls
  :hook (emacs-lisp-mode . highlight-function-calls-mode)
  )

;;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-elisp-rcp)
;;; Commentary:
;;
;;; programming-elisp-rcp.el ends here
