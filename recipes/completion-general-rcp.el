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
;; Sorting and filtering of minibuffer candidates. Big benefit is having most
;; recent candidate shown on top
(use-package prescient
  :if (or (bound-and-true-p ivy-mode)
          (bound-and-true-p selectrum-mode)
          (bound-and-true-p company-mode))
  :hook (elpaca-after-init . prescient-persist-mode)
  :custom
  (prescient-filter-method '(literal initialism regexp anchored))
  (prescient-sort-full-matches-first t)

  (prescient-aggressive-file-save t)
  (prescient-history-length 200)
  (prescient-frequency-decay 0.999)
  (prescient-frequency-threshold 0.10))

;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :general (:keymaps 'minibuffer-local-map
            "M-m" 'marginalia-cycle)
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
