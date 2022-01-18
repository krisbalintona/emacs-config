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
  :ghook ('window-setup-hook 'prescient-persist-mode)
  :custom
  ;; How does it filter?
  (prescient-filter-alist '((literal . prescient-literal-regexp)
                            (literal-prefix . prescient-literal-prefix-regexp)
                            (initialism . prescient-initials-regexp)
                            (regexp . prescient-regexp-regexp)
                            (fuzzy . prescient-fuzzy-regexp)
                            (prefix . prescient-prefix-regexp)
                            (anchored . prescient-anchored-regexp))
                          )
  (prescient-filter-method '(literal regexp anchored initialism))

  (prescient-use-char-folding t)
  (prescient-use-case-folding t)
  (prescient-sort-full-matches-first t)

  (prescient-history-length 200)
  (prescient-frequency-decay 0.999)
  (prescient-frequency-threshold 0.10)
  )

;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :ghook 'window-setup-hook
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" '(marginalia-cycle :which-key "Marginalia cycle"))
  :custom
  (marginalia-max-relative-age 0)       ; Don't show relative ages
  (marginalia-align 'right)
  )

;;; completion-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-general-rcp)
