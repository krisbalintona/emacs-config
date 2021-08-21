;;; straight-package-management-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Install straight.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Set straight.el variables
;; Set all variables before bootstrapping straight.el
(defvar straight-use-package-by-default t) ; Automatically :straight t for use-package
(defvar straight-check-for-modifications '(watch-files find-when-checking)) ; Faster init, requires python3 and watchexec
(defvar straight-fix-org t)
(defvar straight-repository-branch "develop") ; Use development branch

;;;; Bootstrap (install straight.el)
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

;;;; Make sure watchexec is installed
;; This automatically pulls package repo changes via straight.el
(unless (executable-find "watchexec")
  (async-shell-command "sudo cargo install watchexec-cli"))

;;; straight-package-management-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'straight-package-management-rcp)
