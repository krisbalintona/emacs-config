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
(require 'keybinds-frameworks-rcp)

;;;; Prescient
;; Sorting and filtering of minibuffer candidates. Big benefit is having most
;; recent candidate shown on top
(use-package prescient
  :after selectrum
  :hook (selectrum-prescient-mode . prescient-persist-mode)
  )

;;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :ghook 'after-init-hook
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" '(marginalia-cycle :which-key "Marginalia cycle"))
  :custom
  (marginalia-max-relative-age 0)       ; Don't show relative ages
  :config
  ;; Marginalia faces
  (set-face-attribute 'marginalia-documentation nil
                      :inherit nil
                      :foreground "#98C379"
                      :slant 'italic)
  )

;;; completion-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-general-rcp)
