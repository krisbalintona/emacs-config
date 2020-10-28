;;; default-package-management-rcp.el
;;
;;; Code:

;;; LoadRepos
;; All of this is package.el and it conflicts with
;; straight.el. Package-archives can be used if loaded after
;; straight.el. Useful if you want to still peruse packages with M-x
;; package-list-packages. Here, since we aren't using straight.el, we
;; must load it beforehand to be able to install the following
;; packages
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
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)) ; Select folder to store packages

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) ; Don't package initialize at Emacs startup (?)
  (package-initialize))
;;; LoadRepos

;;; UsePackage
;; Install use-package if not installed - INCOMPATIBLE with straight.el
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

;;; Quelpa
;; Install packages directly from sources. INCOMPATIBLE with straight.el
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(setq quelpa-upgrade-interval 2) ; Update quelpa packages every X days after init
(add-hook #'after-init-hook #'quelpa-upgrade-all-maybe)

(setq quelpa-checkout-melpa-p t) ; Update (or not) local MELPA git repo since I'll be using quelpa for non-MELPA packages
(setq quelpa-upgrade-p t) ; Only applies to MELPA packages?: Upgrade packages automatically
;;; Quelpa

;;; QuelpaUsePackage
;; Install packages directly from sources. INCOMPATIBLE with straight.el
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(quelpa-use-package-activate-advice) ; Necessary for use-package-always-ensure to take from ELPA but all quelpa packages to take from quelpa
;;; QuelpaUsePackage

;;; EarlyInstalls
;; Must be loaded before any org file or package
;; Install org-plus-contrib if not installed - INCOMPATIBLE with straight.el
(unless (package-installed-p 'org-plus-contrib)
  (progn
    (package-refresh-contents)
    (package-install 'org-plus-contrib)))

(use-package diminish)
;;; EarlyInstalls

;;; AutoPackageUpdate
;; Automatically keep packages updaetd. INCOMPATIBLE with straight.el
;; since ithas its own way to keep packages updated
(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 1) ;; in days
  (auto-package-update-prompt-before-update nil)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results nil)
  :config
  (auto-package-update-maybe)
  )
;;; AutoPackageUpdate

(provide 'default-package-management-rcp)
;;; Commentary:
;; Install use-package, quelpa, quelpa-use-package, and other early
;; installs as well as configure auto-package-update
;;
;;; default-package-management-rcp.el ends here
