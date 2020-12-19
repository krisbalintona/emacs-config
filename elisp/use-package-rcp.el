;;; use-package-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Install use-package, set it up, and load repositories
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Install use-package
(require 'straight)
(straight-use-package 'use-package)

;;;; Settings
(use-package use-package
  :custom
  ;; (setq use-package-always-ensure t) ; May cause issues with straight.el
  (use-package-expand-minimally t) ; Less verbose
  ;; (setq use-package-compute-statistics t) ; Need this at "loadup time" or else errors about undefined variables will appear
  (use-package-enable-imenu-support t)

  ;; Set use-package-verbose to t for interpreted .emacs, and to nil for
  ;; byte-compiled .emacs.elc.
  (use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;;;; Load repos
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

;;; use-package-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'use-package-rcp)
