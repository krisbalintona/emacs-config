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
(require 'org-general-rcp)

;;; Org-agenda
(use-package org-agenda
  :straight nil
  :general (kb/open-keys
             "a" '(org-agenda :wk "Org-agenda")
             )
  :custom
  (org-agenda-files nil) ; Instead , dynamically generate agenda list with vulpea
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
  (org-log-done nil)       ; Inactive timestamp already added to logbook
  (org-log-into-drawer t)

  (org-agenda-prefix-format
   '((agenda . " %i %(vulpea-agenda-category 22)%?-12t% s")
     (todo . " %i %(vulpea-agenda-category 22) ")
     (tags . " %i %(vulpea-agenda-category 22) ")
     (search . " %i %(vulpea-agenda-category 22) ")
     ))
  :config
  ;; This loads my vulpea functions to dynamically set
  ;; `org-agenda-files'. I put here since this relies on org-roam, but I don't
  ;; want to explicitly load since that would increase startup time. Thus, I put
  ;; it here to load when I first open org-agenda.
  (require 'kb-vulpea)

  ;; (add-to-list 'org-tags-exclude-from-inheritance "blog")
  )

;;; org-agenda-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-general-rcp)
