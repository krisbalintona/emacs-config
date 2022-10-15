;;; programming-racket-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to using Racket.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'keybinds-general-rcp)

;;; Racket-mode
(use-package racket-mode
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . display-fill-column-indicator-mode)))

;;; programming-racket-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-racket-rcp)
