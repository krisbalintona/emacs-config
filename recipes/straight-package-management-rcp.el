;;; straight-package-management-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Install straight.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;; Set straight.el variables
;; Set all variables before bootstrapping straight.el
(defvar straight-use-package-by-default t) ; Automatically :straight t for use-package
(defvar straight-repository-branch "develop") ; Use development branch
(defvar straight-check-for-modifications
  '(find-at-startup find-when-checking))
;; Start after path from `exec-path-from-shell' is set
(add-hook 'emacs-startup-hook 'straight-watcher-start)

;;; Bootstrap (install straight.el)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Exec-path-from-shell
;; Ensure Emacs' and system shell have same path
(straight-use-package 'exec-path-from-shell)
(require 'exec-path-from-shell)
;; Set to nil, particularly removing the `-i' (interactive) flag to quicken
;; startup. However, my variables need to be accessible by non-interactive
;; shells, meaning the proper installation steps need to be followed. Thus,
;; variables should be set in `~/.profile', `~/.bash_profile', or `~/.zshenv'
;; instead of `~/.bashrc' or `~/.zshrc'. See here for more:
;; https://github.com/purcell/exec-path-from-shell
(setq exec-path-from-shell-arguments nil)
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "BROWSER"))
(exec-path-from-shell-initialize)

;;; System-packages
;; Install system packages within Emacs. Necessary for use-package's
;; `:ensure-system-package' flag
(straight-use-package 'system-packages)
(require 'system-packages)
(setq system-packages-use-sudo t
      system-packages-noconfirm t)      ; Bypass its prompt

;;; straight-package-management-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'straight-package-management-rcp)
