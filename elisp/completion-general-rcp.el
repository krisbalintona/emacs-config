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

;;;; Default completion settings
;; Taken from https://karthinks.com/software/more-batteries-included-with-emacs/
;; Unfortunately this isn’t the case, because ivy and helm are off doing their
;; own thing. Emacs’ built in completion styles do work with icomplete, ido and
;; possibly selectrum though.
(setq completion-styles '(basic initials partial-completion flex)
      completion-cycle-threshold 10)

;;;; Prescient
;; Simple sorting of minibuffer candidates. Big benefit is having most recent
;; candidate shown on top
(use-package prescient
  ;; :after (selectrum counsel) ; Needs to be called after counsel so that counsel doesn't overwrite stuff
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
