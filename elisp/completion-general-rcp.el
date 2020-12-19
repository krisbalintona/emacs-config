;;; completion-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are settings and/or packages which are package agnostic, some involved
;; with the default Emacs completion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Default completion settings
;; Taken from https://karthinks.com/software/more-batteries-included-with-emacs/
;; Unfortunately this isn’t the case, because ivy and helm are off doing their
;; own thing. Emacs’ built in completion styles do work with icomplete, ido and
;; possibly selectrum though.
(setq completion-styles '(basic initials partial-completion flex))
(setq completion-cycle-threshold 10)

;;;; Prescient
;; Simple sorting of minibuffer candidates. Big benefit is having most recent
;; candidate shown on top
(use-package prescient
  :after counsel ; Needs to be called after counsel so that counsel doesn't overwrite stuff
  :config
  (prescient-persist-mode)
  )

;;; completion-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-general-rcp)
