;;; completion-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are settings and/or packages which are package agnostic, some involved
;; with the default Emacs completion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Prescient
;; Sorting and filtering of minibuffer candidates. The difference between
;; `orderless' and this package is that `orderless' filters but does not sort -
;; it leaves that up to the "candidate source and the completion UI."
;; Additionally, `orderless' has style "dispatchers," i.e., I can define
;; predicates for what filtering style to use for which token
(use-package prescient
  :demand
  :custom
  (completion-styles '(prescient flex))
  (prescient-filter-method
   '(literal literal-prefix initialism regexp))
  (prescient-sort-full-matches-first t)
  (prescient-history-length 200)
  :config
  (prescient-persist-mode))

;;;; Vertico-prescient
(use-package vertico-prescient
  :after vertico
  :custom
  (vertico-prescient-enable-filtering t)
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting t)
  :init
  (vertico-prescient-mode))

;;;; Corfu-prescient
(use-package corfu-prescient
  :after corfu
  :custom
  (corfu-prescient-enable-filtering t)
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting t)
  :init
  (corfu-prescient-mode))

;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-field-width 80)
  (marginalia-align-offset -2)          ; Two to the left
  :init
  (marginalia-mode))

;;; completion-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-general-rcp)
