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
  :disabled ; FIXME 2023-07-16: Haven't figured out a way to programmatically detect racket-langserver's presence
  :ensure-system-package ("/home/krisbalintona/.local/share/racket/8.9/pkgs/racket-langserver" . "raco pkg install racket-langserver")
  :hook ((racket-mode . display-fill-column-indicator-mode)
         (racket-mode . eglot-ensure)))

;;; programming-racket-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-racket-rcp)
