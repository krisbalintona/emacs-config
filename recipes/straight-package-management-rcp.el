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
(defvar straight-repository-branch "master") ; Use development branch
(defvar straight-check-for-modifications
  '(watch-files check-on-save find-when-checking))

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
(setq exec-path-from-shell-variables
      '("PATH" "MANPATH" "BROWSER"
        ;; For `ssh-agent'. Also reliant on systemd service. See
        ;; https://wiki.archlinux.org/title/SSH_keys#Start_ssh-agent_with_systemd_user
        "SSH_AGENT_PID" "SSH_AUTH_SOCK"
        ;; For LSP-mode. See
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
        "LSP_USE_PLISTS"))
(exec-path-from-shell-initialize)
;; Found here:
;; https://www.reddit.com/r/emacs/comments/s6zkb6/comment/ht794j7/?utm_source=share&utm_medium=web2x&context=3
(exec-path-from-shell-copy-env "GOPATH")
(when (eq (length (getenv "NODE_PATH")) 0) ; For npm
  (setenv "NODE_PATH" "/usr/local/lib/node_modules"))

;;; System-packages
;; Install system packages within Emacs. Necessary for use-package's
;; `:ensure-system-package' flag
(straight-use-package 'system-packages)
(require 'system-packages)
(setq system-packages-noconfirm t)      ; Bypass its prompt
(when (executable-find "paru")
  (setf (cdr (assoc 'pacman system-packages-supported-package-managers))
        '((default-sudo . t)
          (install . "paru -S")
          (search . "paru -Ss")
          (uninstall . "paru -Rns")
          (update . "paru -Syu")
          (clean-cache . "paru -Sc")
          (change-log . "paru -Qc")
          (log . "cat /var/log/paru.log")
          (get-info . "paru -Qi")
          (get-info-remote . "paru -Si")
          (list-files-provided-by . "paru -qQl")
          (owning-file . "paru -Qo")
          (owning-file-remote . "paru -F")
          (verify-all-packages . "paru -Qkk")
          (verify-all-dependencies . "paru -Dk")
          (remove-orphaned . "paru -Rns $(paru -Qtdq)")
          (list-installed-packages . "paru -Qe")
          (list-installed-packages-all . "paru -Q")
          (list-dependencies-of . "paru -Qi")
          (noconfirm . "--noconfirm"))))

;;; straight-package-management-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'straight-package-management-rcp)
