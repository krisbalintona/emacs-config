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
  :after counsel ; Needs to be called after counsel so that counsel doesn't overwrite stuff
  :config
  (prescient-persist-mode)
  )

;;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :ghook 'after-init
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)) ; Show as much information as possible
  :config
  ;; Marginalia faces
  (set-face-attribute 'marginalia-documentation nil
                      :inherit nil
                      :foreground "#98C379"
                      :slant 'italic)
  )

;;;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :ghook 'after-init
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)) ; Show as much information as possible
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
