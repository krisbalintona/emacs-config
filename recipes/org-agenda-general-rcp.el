;;; org-agenda-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Settings related to org-agenda itself.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Org-agenda
(use-package org-agenda
  :straight nil
  :general (kb/leader-keys
             "oa" '(org-agenda :which-key "Org-agenda")
             )
  :custom
  (org-agenda-files nil)
  (org-agenda-window-setup 'current-window)
  (org-use-fast-todo-selection 'auto)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d!/@)" "CANCELLED(c@/!)")
     ))
  (org-todo-keyword-faces
   '(("TODO" :foreground "orange" :underline t)
     ("NEXT" :foreground "orchid" :weight bold)
     ("PROG" :foreground "turquoise" :underline t)
     ("DONE" :foreground "chartreuse" :weight normal)
     ("CANCELLED" :foreground "deep pink" :weight normal)
     ))
  (org-log-done 'time)       ; When done add CLOSED line with inactive timestamp
  )

;;; org-agenda-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-general-rcp)
