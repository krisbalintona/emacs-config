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
  (when (bound-and-true-p evil-local-mode)
    (evil-set-initial-state 'py-shell-mode 'normal)))

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
  :commands kb/pyvenv-pred-project-root
  :hook (pyvenv-mode . (lambda ()
                         ;; TODO 2022-06-20: `pyvenv-tracking-mode' relies on
                         ;; `post-command-hook' which is expensive.
                         ;; Consequently, it would be preferable if we can avoid
                         ;; using it. Possible solutions may lie in `envrc' and
                         ;; `buffer-env'
                         (if (and pyvenv-tracking-mode pyvenv-mode)
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
  :config
  ;; Variables
  (defvar kb/pyvenv-possible-env-names
    `(,pyvenv-default-virtual-env-name "env" ".env" "venv" ".venv" "virtualenv" ".virtualenv"))
  (defvar kb/pyvenv-venv-type nil)
  (defvar kb/pyvenv-dir-predicates
    '(kb/pyvenv-pred-default-directory kb/pyvenv-pred-project-root kb/pyvenv-pred-workon)
    "Predicates that check for the existence of virtual environments.

Each predicate in this list should have a directory as its first
argument. A non-nil returned value should be a cons whose car is
a file path and cdr is a string used by `kb/pyvenv-venv-type' in
the mode line.")

  ;; Functions/commands
  (defun kb/pyvenv--directory-slug (dir)
    "Return a pyvenv slug based on DIR.

DIR should be a file path. If DIR is in a project according to
`project-current', then return the directory name of the
project's root. Otherwise, return the directory name of DIR."
    (require 'project)
    (unless dir
      (setq dir default-directory))
    (or (when-let (proj (project-current nil dir))
          (file-name-nondirectory (directory-file-name (project-root proj))))
        (file-name-nondirectory (directory-file-name dir))
        (user-error "[kb/pyvenv--directory-slug] Can't calculate slug!")))
  (defun kb/pyvenv-pred-default-directory (dir &optional signifier)
    "Check if a virtual environment exists in DIR.

Go through `kb/pyvenv-possible-env-names' in order, checking if
there exists a directory within DIR whose name matches any of its
strings. If so, return a cons whose car is the file path to that
directory and cdr is SIGNIFIER, if provided. Otherwise, return
nil.

SIGNIFIER should be a string, and defaults to \"D\" if not
provided."
    (when (and signifier
               (not (stringp signifier)))
      (user-error "Argument is not a string!"))
    (let (matches)
      (dolist (env-name kb/pyvenv-possible-env-names)
        (when (file-exists-p (file-name-as-directory
                              (expand-file-name env-name dir)))
          (add-to-list 'matches env-name t)))
      (when matches
        (cons (expand-file-name (car matches) dir)
              (or signifier "D")))))
  (defun kb/pyvenv-pred-project-root (dir)
    "Check if a virtual environment exists in DIR's project root.

If DIR is not within a project according to `project-current',
then return nil. Otherwise, call
`kb/pyvenv-pred-default-directory' on this project's root
directory. This returns car is the `project-root' and cdr is
\"P\"."
    (require 'project)
    (when-let (proj (project-current nil dir))
      (kb/pyvenv-pred-default-directory (project-root proj) "P")))
  (defun kb/pyvenv-pred-workon (dir)
    "Check if DIR has an associated virtual environment in `pyvenv-workon-home'.

If not, return nil. Otherwise, return a cons whose car is the
path to the virtual environment and whose cdr is \"W\".

The directory name we're checking for is determined by
`kb/pyvenv--directory-slug'."
    (when-let ((slug (expand-file-name
                      (kb/pyvenv--directory-slug dir)
                      (pyvenv-workon-home)))
               (path (file-name-as-directory slug)))
      (when (file-exists-p path)
        (cons path "W"))))

  (defun kb/pyvenv-auto-activate ()
    "Automate selection of python virtual environment.

This function sets the `pyvenv-activate' and/or `pyvenv-workon'
variable so that the later invocate of `pyvenv-mode' activates
the virtual environment. Requires `pyvenv-tracking-mode' to be
enabled. Additionally, this function should be run before
`pyvenv-mode' since it sets the variables needed to be known pror
to the mode's activation. Finally, this also sets
`kb/pyvenv-venv-type', for the mode line, appropriately.

The following is the behavior, in order:

Checks if `pyvenv-activate' or `pyvenv-workon' are non-nil and
valid. IF so, set `kb/pyvenv-venv-type' appropriately.

Go through the predicates in `kb/pyvenv-dir-predicates' in order.
A non-nil value sets `pyvenv-activate' and `kb/pyvenv-venv-type'
to a particular string specified by the first non-nil predicate.

If all returned values are nil, prompt to create a virtual
environment in `pyvenv-workon-home' and set `pyvenv-workon' to
it."
    (interactive)
    ;; Used functions need to be loaded
    (require 'pyvenv)
    (unless (equal major-mode 'python-mode)
      (user-error "[kb/pyvenv-auto-activate] Not in python-mode, can't activate venv!"))

    ;; First check if `pyvenv-workon' or `pyvenv-activate' are non-nil
    (when (and (stringp pyvenv-workon)
               (file-exists-p pyvenv-workon))
      (setq-local kb/pyvenv-venv-type "W"))
    (when (and (stringp pyvenv-activate)
               (file-exists-p pyvenv-activate))
      (setq-local kb/pyvenv-venv-type "A"))

    ;; Then go through predicates
    (let* ((preds kb/pyvenv-dir-predicates)
           venv-cons
           current-pred)
      ;; Go through preds and set venv-cons if one of them returns non-nil
      (while (and (not venv-cons) preds)
        (setq current-pred (car preds))
        (setq venv-cons (funcall current-pred default-directory))
        (setq preds (cdr preds)))

      ;; Ensure venv-cons doesn't have unexpected data types
      (when (and venv-cons
                 (not (stringp (car venv-cons)))
                 (not (stringp (cdr venv-cons))))
        (user-error "[kb/pyvenv-auto-activate] Something went wrong!"))

      ;; If venv-consp is non-nil at this point, activate that path. Otherwise,
      ;; create a new venv in `venv-workon-home' and activate it. Also set
      ;; `kb/pyvenv-venv-type' appropriately.
      (if venv-cons
          (progn (setq-local pyvenv-activate (car venv-cons))
                 (setq-local kb/pyvenv-venv-type (cdr venv-cons)))
        (let ((slug (kb/pyvenv--directory-slug))
              (path (expand-file-name slug (pyvenv-workon-home))))
          (pyvenv-create slug (cl-loop for bin in py-known-shells-extended-commands
                                       when (and (not (string= bin "ipython"))
                                                 (executable-find bin))
                                       return bin))
          (setq-local pyvenv-workon path)
          (setq-local kb/pyvenv-venv-type "W"))))))

;;; Python-pytest
(use-package python-pytest
  :general (kb/lsp-keys
             "t" '(ignore :wk "Pyvenv")
             "tt" '(python-pytest-file :wk "Pyvenv activate")
             "tT" '(python-pytest-dispatch :wk "Pyvenv activate")))


;;; programming-python-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-python-rcp)
