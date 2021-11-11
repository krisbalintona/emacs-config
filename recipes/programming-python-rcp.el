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
  :demand t
  :after (python pyvenv)
  :hook ((python-mode . (lambda ()
                          (interactive)
                          (if (project-current) ; Set virtual environment to ./venv/ if currently in a project
                              (pyvenv-activate (concat default-directory "venv/")))
                          ))
         (py-shell-mode . (lambda ()
                            (hide-mode-line-mode)
                            (setq-local scroll-margin 0)
                            )))
  :ensure-system-package (pytest . "pip install --user pytest")
  :hook (py-shell-mode . hide-mode-line-mode)
  :gfhook
  'lsp-deferred
  'dap-mode
  '(lambda ()
     (require 'prog-mode)
     (push '("->" . ?⟹) prettify-symbols-alist)
     (prettify-symbols-mode)
     )
  :custom
  (py-shell-name "ipython3")

  ;; When using `py-execute-' commands
  (py-split-windows-on-execute-function 'split-window-horizontally) ; How window gets split
  (py-split-window-on-execute-threshold 1) ; Number of current displayed windows until no splitting
  (py-keep-windows-configuration nil)   ; Retain current window configuration?
  (py-split-window-on-execute t)        ; Reuse existing windows?
  (py-switch-buffers-on-execute-p nil)  ; Switch to buffer?
  )

;;; Dap-python
;; Compatibility with dap
(use-package dap-python
  :demand t
  :after dap-mode
  :ensure-system-package (debugpy . "pip install --user debugpy") ; For debugging in python using dap
  :straight nil
  :custom
  (dap-python-executable "ipython3")
  (dap-python-debugger 'debugpy)        ; Updated version of ptvsd
  (dap-debug-compilation-keep t)        ; Keep output window in success?
  (dap-debug-restart-keep-session nil)  ; Delete previous sessions
  )

;;; Lsp-pyright
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

;;; Pyvenv
;; Install packages to a local directory rather than globally Call
;; `pyvenv-activate' and select a directory with virtual environment packages
(use-package pyvenv
  :after (python lsp-mode)
  :ghook 'python-mode-hook
  :gfhook 'pyvenv-tracking-mode
  :general (:keymaps 'lsp-mode-map
                     (concat lsp-keymap-prefix "v") '((lambda () (interactive) (call-interactively 'pyvenv-activate)) :which-key "Pvenv activate"))
  :custom (pyvenv-default-virtual-env-name "venv")
  )

;;; programming-python-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-python-rcp)
