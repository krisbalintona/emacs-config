;;; completion-general-rcp.el --- Summary
;;
;; These are settings and/or packages which are package agnostic, some involved
;; with the default Emacs completion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Default completion
;; Taken from https://karthinks.com/software/more-batteries-included-with-emacs/
;; Unfortunately this isn’t the case, because ivy and helm are off doing their
;; own thing. Emacs’ built in completion styles do work with icomplete, ido and
;; possibly selectrum though.
(setq completion-styles '(basic initials partial-completion flex))
(setq completion-cycle-threshold 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-general-rcp)
;;; Commentary:
;;
;;; completion-general-rcp.el ends here
