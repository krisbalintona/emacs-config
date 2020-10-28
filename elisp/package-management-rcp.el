;;; package-management-rcp.el --- Configure use-package and quelpa

;;; Code:
;; Straight.el
;; Set all variables before bootstrapping straight.el
(setq straight-use-package-by-default t) ; Automatically :straight t for use-package
(setq straight-check-for-modifications '(watch-files find-when-checking)) ; Faster init, requires python3 and watchexec
(setq straight-fix-org t)
(setq straight-repository-branch "develop") ; Use development branch

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
;; Straight.el

;; InstallOrgContrib
;; Must be loaded before any org file or package
;; Install org-plus-contrib if not installed - INCOMPATIBLE with straight.el
;; (unless (package-installed-p 'org-plus-contrib)
;;   (progn
;;     (package-refresh-contents)
;;     (package-install 'org-plus-contrib)))

(straight-use-package 'org-plus-contrib)
;; (straight-use-package 'org)
;; InstallOrgContrib

;; UsePackage
;; Install use-package if not installed - INCOMPATIBLE with straight.el
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; Install use-package with straight.el
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
;; UsePackage

;; Diminish
(use-package diminish)
;; Diminish

;; Quelpa
;; Install packages directly from sources. INCOMPATIBLE with straight.el
;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

;; (setq quelpa-upgrade-interval 2) ; Update quelpa packages every X days after init
;; (add-hook #'after-init-hook #'quelpa-upgrade-all-maybe)

;; (setq quelpa-checkout-melpa-p t) ; Update (or not) local MELPA git repo since I'll be using quelpa for non-MELPA packages
;; (setq quelpa-upgrade-p t) ; Only applies to MELPA packages?: Upgrade packages automatically
;; Quelpa

;; QuelpaUsePackage
;; Install packages directly from sources. INCOMPATIBLE with straight.el
;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)

;; (quelpa-use-package-activate-advice) ; Necessary for use-package-always-ensure to take from ELPA but all quelpa packages to take from quelpa
;; QuelpaUsePackage

;; AutoPackageUpdate
;; Automatically keep packages updaetd. INCOMPATIBLE with straight.el
;; since ithas its own way to keep packages updated
;; (use-package auto-package-update
;;   :if (not (daemonp))
;;   :custom
;;   (auto-package-update-interval 1) ;; in days
;;   (auto-package-update-prompt-before-update nil)
;;   (auto-package-update-delete-old-versions t)
;;   (auto-package-update-hide-results nil)
;;   :config
;;   (auto-package-update-maybe)
;;   )
;; AutoPackageUpdate

(provide 'package-management-rcp)
;;; Commentary:
;;
;;; package-management-rcp.el ends here
