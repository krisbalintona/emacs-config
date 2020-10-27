;;; package-management-rcp.el --- Configure use-package and quelpa

;;; Code:
;; UsePackage
(unless (package-installed-p 'use-package) ; Install use-package if not installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))
;; UsePackage

;; Diminish
(use-package diminish)
;; Diminish

;; Quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(setq quelpa-upgrade-interval 2) ; Update quelpa packages every X days after init
(add-hook #'after-init-hook #'quelpa-upgrade-all-maybe)

(setq quelpa-checkout-melpa-p t) ; Update (or not) local MELPA git repo since I'll be using quelpa for non-MELPA packages
(setq quelpa-upgrade-p t) ; Only applies to MELPA packages?: Upgrade packages automatically
;; Quelpa

;; QuelpaUsePackage
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(quelpa-use-package-activate-advice) ; Necessary for use-package-always-ensure to take from ELPA but all quelpa packages to take from quelpa
;; QuelpaUsePackage

;; AutoPackageUpdate
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
;; AutoPackageUpdate

(provide 'package-management-rcp)
;;; Commentary:
;;
;;; package-management-rcp.el ends here
