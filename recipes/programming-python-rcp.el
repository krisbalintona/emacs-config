;;; programming-python-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to developing in Python.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'general)
(require 'keybinds-general-rcp)

;;; Python
;; Built-in python major mode
(use-package python
  :commands (python python-mode)
  :hook ((inferior-python-mode . hide-mode-line-mode)
         (py-shell-mode . (lambda ()
                            (setq-local scroll-margin 0)
                            )))
  :custom
  (python-shell-interpreter "python3")
  )

;;; Python-mode
;; A little better than the built-in python package
(use-package python-mode
  :ensure-system-package (pytest . "pip install --user pytest")
  :gfhook
  'lsp-deferred
  'dap-mode
  '(lambda ()
     (require 'prog-mode)
     (push '("->" . ?⟹) prettify-symbols-alist)
     (prettify-symbols-mode)
     )
  )

;;; Dap-python
;; Compatibility with dap
(use-package dap-python
  :demand t
  :after dap-mode
  :ensure-system-package (debugpy . "pip install --user debugpy") ; For debugging in python using dap
  :straight nil
  :custom
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)      ; Updated version of ptvsd
  )

;;; Lps-pyright
;; Best python language server
(use-package lsp-pyright
  :demand t
  :after lsp-mode
  :custom
  (lsp-pyright-python-executable-cmd "python3")
  (lsp-pyright-disable-language-services nil)
  (lsp-pyright-disable-organize-imports nil)
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-venv-path "venv")
  ;; lsp-pyright-venv-directory
  )

;;; Virtualenv
;; Install packages to a local directory rather than globally
;; Call `pyvenv-activate' and select a directory with virtual environment packages
(use-package pyvenv
  :after python
  :ghook 'python-mode-hook
  )

;;; programming-python-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-python-rcp)
