;;; use-package-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Install use-package, set it up, and load repositories.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'straight-package-management-rcp)

;;;; Use-package
(straight-use-package 'use-package)
(require 'use-package)
;; (setq use-package-always-ensure t) ; May cause issues with straight.el
(setq use-package-expand-minimally t) ; Less verbose
;; (setq use-package-compute-statistics t) ; Need this at "loadup time" or else errors about undefined variables will appear

(setq use-package-always-defer t)       ; Always try to defer

;; Set use-package-verbose to t for interpreted .emacs, and to nil for
;; byte-compiled .emacs.elc.
(setq use-package-verbose (not (bound-and-true-p byte-compile-current-file)))

;;;; Use-package-ensure-system-package
;; Pair with `exec-path-from-shell' to enable ensure-system-package keyword. Requires `system-packages'
(use-package use-package-ensure-system-package
  :after (exec-path-from-shell system-packages)
  )

;;; use-package-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'use-package-rcp)
