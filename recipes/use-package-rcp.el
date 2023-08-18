;;; use-package-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Install use-package, set it up, and load repositories.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(use-package use-package
  :custom
  ;; (use-package-always-ensure t)   ; Causes issues with external package managers
  ;; (use-package-expand-minimally t)      ; Less verbose
  (use-package-always-defer t)          ; Always defer

  ;; Set use-package-verbose to t for interpreted .emacs, and to nil for
  ;; byte-compiled .emacs.elc.
  (use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;;; use-package-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'use-package-rcp)
