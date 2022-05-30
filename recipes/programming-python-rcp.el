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
(require 'keybinds-evil-rcp)
(require 'programming-projects-rcp)

;;; Python-mode
;; A little better than the built-in python package
(use-package python-mode
  :demand t
  :ensure-system-package (pytest . python-pytest-pacman)
  :hook (py-shell-mode . (lambda ()
                           (hide-mode-line-mode)
                           (setq-local scroll-margin 0)))
  :gfhook
  'lsp-deferred
  'dap-mode
  '(lambda ()
     (require 'prog-mode)
     (push '("->" . ?») prettify-symbols-alist)
     (push '("lambda" . ?λ) prettify-symbols-alist)
     (prettify-symbols-mode))
  'display-fill-column-indicator-mode
  :general (:keymaps 'python-mode-map
                     :states '(normal insert)
                     "C-<backspace>" '(lambda () (interactive) (backward-kill-word 1)) ; Python oddly replaces the normal C-<backspace>
                     "M-[" 'python-nav-backward-block
                     "M-]" 'python-nav-forward-block
                     "M-{" 'python-nav-beginning-of-block
                     "M-}" 'python-nav-end-of-block
                     )
  :custom
  (py-python-command "ipython3")
  (py-shell-fontify-p 'all)             ; Fontify shell
  (py-session-p nil)
  (py-dedicated-process-p nil)

  ;; When using `py-execute-' commands
  (py-split-windows-on-execute-function 'split-window-horizontally) ; How window gets split
  (py-split-window-on-execute-threshold 1) ; Number of current displayed windows until no splitting
  (py-keep-windows-configuration nil)   ; Retain current window configuration?
  (py-split-window-on-execute t)        ; Reuse existing windows?
  (py-switch-buffers-on-execute-p nil) ; Switch to buffer?
  :config
  (evil-set-initial-state 'py-shell-mode 'normal))

;;; Dap-python
;; Compatibility with dap
(use-package dap-python
  :demand t
  :after (python-mode dap-mode)
  :ensure-system-package debugpy-run    ; For debugging in python using dap
  :straight nil
  :custom
  (dap-python-executable "ipython3")
  (dap-python-debugger 'debugpy))       ; Updated version of ptvsd

;;; Lsp-pyright
;; Best python language server
(use-package lsp-pyright
  :after (python-mode lsp-mode)
  :custom
  (lsp-pyright-multi-root nil)          ; Useful!
  (lsp-pyright-python-executable-cmd "python3")
  (lsp-pyright-disable-language-services nil)
  (lsp-pyright-disable-organize-imports nil)
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-venv-path "venv")
  ;; lsp-pyright-venv-directory
  )

;;; Anaconda
;; More IDE features to Python
(use-package anaconda-mode
  :hook (python-mode . anaconda-mode)
  :gfhook 'anaconda-eldoc-mode
  :general
  (:keymaps 'anaconda-mode-map        ; The bindings I want from evil-collection
            :states 'normal
            "gd" 'anaconda-mode-find-definitions
            "gA" 'anaconda-mode-find-assignments
            "gr" 'anaconda-mode-find-references
            )
  )

;;; Pyvenv
;; Install packages to a local directory rather than globally call
;; `pyvenv-activate' and select a directory with virtual environment packages
(use-package pyvenv
  :hook (python-mode . kb/pyvenv-auto-activate)
  :ghook 'python-mode-hook
  :gfhook
  'pyvenv-tracking-mode
  'kb/pyvenv-setup-variables
  :general (kb/lsp-keys
             "v" '(ignore :wk "Pyvenv")
             "vv" '(pyvenv-activate :wk "Activate")
             "vV" '(pyvenv-workon :wk "Workon")
             "vc" '(pyvenv-create :wk "Create"))
  :init
  (defun kb/pyvenv-auto-activate ()
    "Activate virtual environment, checking present directory then
project root."
    (require 'pyvenv)               ; Functions require the package to be loaded
    (let* ((venv-dir (concat default-directory pyvenv-default-virtual-env-name))
           (venv-root (concat (project-root (project-current)) pyvenv-default-virtual-env-name))
           )
      (cond ((file-exists-p venv-dir) (pyvenv-activate venv-dir)) ; Check present directory
            ((file-exists-p venv-root) (pyvenv-activate venv-root)) ; Then project root
            (t (message "No virtual environment found."))
            )))
  (defun kb/pyvenv-setup-variables ()
    "Set up pyvenv variables.

Set here since :custom declaration doesn't work for some reason."
    (setq pyvenv-default-virtual-env-name ".venv"
          pyvenv-mode-line-indicator
          (concat "[V] " (format-mode-line pyvenv-virtual-env-name 'mode-line-emphasis))
          ))
  )

;;; Python-pytest
(use-package python-pytest
  :general (kb/lsp-keys
             "t" '(ignore :wk "Pyvenv")
             "tt" '(python-pytest-file :wk "Pyvenv activate")
             "tT" '(python-pytest-dispatch :wk "Pyvenv activate"))
  )

;;; programming-python-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-python-rcp)
