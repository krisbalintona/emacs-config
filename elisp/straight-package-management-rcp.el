;;; straight-package-management-rcp.el --- Summary
;;
;; Set up package management packages and settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Set straight.el variables
;; Set all variables before bootstrapping straight.el
(setq straight-use-package-by-default t) ; Automatically :straight t for use-package
(setq straight-check-for-modifications '(watch-files find-when-checking)) ; Faster init, requires python3 and watchexec
(setq straight-fix-org t)
(setq straight-repository-branch "develop") ; Use development branch

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

;;;; Install use-package and set settings
(straight-use-package 'use-package)

(eval-and-compile
  ;; (setq use-package-always-ensure t) ; May cause issues with straight.el
  (setq use-package-expand-minimally t) ; Less verbose
  ;; (setq use-package-compute-statistics t) ; Need this at "loadup time" or else errors about undefined variables will appear
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

;; Set use-package-verbose to t for interpreted .emacs, and to nil for
;; byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;;;; Load repos
;; All of this is package.el and it conflicts with straight.el. Package-archives
;; can be used if loaded after straight.el. Useful if you want to still peruse
;; packages with M-x package-list-packages
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ("org" . "http://orgmode.org/elpa/")
        ;; ("gnu"   . "https://elpa.gnu.org/packages/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ;; ;; Chinese servers
        ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'straight-package-management-rcp)
;;; Commentary:
;;
;; Install straight.el, use-package, and other early installs
;;
;;; straight-package-management-rcp.el ends here
