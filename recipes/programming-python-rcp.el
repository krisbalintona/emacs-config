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
  ;; 'lsp-deferred
  ;; 'dap-mode
  'eglot-ensure
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
  :requires dap-mode
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
  :requires lsp
  :after python-mode
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

;;; Pyvenv
;; Install packages to a local directory rather than globally call
;; `pyvenv-activate' and select a directory with virtual environment packages
(use-package pyvenv
  :hook (pyvenv-mode . (lambda ()
                         (if pyvenv-tracking-mode
                             (add-hook 'python-mode-hook 'kb/pyvenv-auto-activate)
                           (remove-hook 'python-mode-hook 'kb/pyvenv-auto-activate))))
  :ghook 'python-mode-hook
  :gfhook
  'pyvenv-tracking-mode
  :general (kb/lsp-keys
             "v" '(ignore :wk "Pyvenv")
             "vv" '(pyvenv-activate :wk "Activate")
             "vV" '(pyvenv-workon :wk "Workon")
             "vc" '(pyvenv-create :wk "Create"))
  :custom
  (pyvenv-default-virtual-env-name ".venv")
  (pyvenv-mode-line-indicator (concat "[" kb/pyvenv-type "]"))
  :config
  (defvar kb/pyvenv-possible-env-names
    `(,pyvenv-default-virtual-env-name "env" ".env" "venv" ".venv" "virtualenv" ".virtualenv"))
  (defvar kb/pyvenv-type nil)
  (defun kb/pyvenv-auto-activate ()
    "Automate activation of python virtual environment.

Activates if `pyvenv-workon' is non-nil (e.g. from .dir-locals.el
or `WORKON_HOME' environment variable).

Otherwise, checks for directories whose names appear in
`kb/pyvenv-possible-env-names'; the first directory name
successfully found in this list will be the virtual environment.
The following is the procedure for checking whether a directory
exists:

1. Check present directory
2. Check project root
3. Check in `pyvenv-workon-home' for a directory matching the
name of the current project according to `project-current'
4. Check in `pyvenv-workon-home' for a directory matching the
name of the `default-directory'

If all of these fail, then create a virtual environment with
`pyvenv-create' and activate it."
    (interactive)
    ;; Used functions need to be loaded
    (require 'pyvenv)
    (require 'project)
    (unless (equal major-mode 'python-mode)
      (user-error "[kb/pyvenv-auto-activate] Not in python-mode, can't activate venv!"))
    (dolist (current-virtual-env (delete-dups kb/pyvenv-possible-env-names))
      (let* ((venv-in-dir
              (file-name-as-directory (expand-file-name current-virtual-env default-directory)))
             (venv-in-project
              (when (project-current)
                (expand-file-name current-virtual-env
                                  (project-root (project-current)))))
             (venv-expected-name
              (or (when (project-current)
                    (file-name-nondirectory (directory-file-name (project-root (project-current)))))
                  (file-name-nondirectory (directory-file-name default-directory))
                  (user-error "[kb/pyvenv-auto-activate] Can't calculate `venv-expected-virtualenv-path'!")))
             (venv-expected-virtualenv-path
              (file-name-as-directory (expand-file-name venv-expected-name (pyvenv-workon-home))))
             (venv-chosen
              (cond ((and (stringp pyvenv-workon)
                          (file-exists-p pyvenv-workon))
                     pyvenv-workon
                     (setq-local kb/pyvenv-type "E"))
                    ((file-exists-p venv-in-dir)
                     venv-in-dir
                     (setq-local kb/pyvenv-type "D"))
                    ((and (stringp venv-in-project)
                          (file-exists-p venv-in-project))
                     venv-in-project
                     (setq-local kb/pyvenv-type "P"))
                    ((file-exists-p venv-expected-virtualenv-path)
                     venv-expected-virtualenv-path
                     (setq-local kb/pyvenv-type "W")))))
        (if venv-chosen
            (progn (pyvenv-activate venv-chosen)
                   (message "[kb/pyvenv-auto-activate] Using python venv at %s" venv-chosen))
          (pyvenv-create venv-expected-name
                         ;; Taken from when `pyvenv-create' is run interactively
                         (let ((dir (if pyvenv-virtualenvwrapper-python
                                        (file-name-directory pyvenv-virtualenvwrapper-python)
                                      nil))
                               (initial (if pyvenv-virtualenvwrapper-python
                                            (file-name-base pyvenv-virtualenvwrapper-python)
                                          nil)))
                           (read-file-name "Python interpreter to use: " dir nil nil initial)))
          (pyvenv-activate venv-expected-virtualenv-path)
          (message "[kb/pyvenv-auto-activate] Venv created and activated at %s"
                   venv-expected-virtualenv-path)
          (setq-local kb/pyvenv-type "W"))))))

;;; Python-pytest
(use-package python-pytest
  :general (kb/lsp-keys
             "t" '(ignore :wk "Pyvenv")
             "tt" '(python-pytest-file :wk "Pyvenv activate")
             "tT" '(python-pytest-dispatch :wk "Pyvenv activate")))


;;; programming-python-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-python-rcp)
