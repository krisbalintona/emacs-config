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
  (kb/vulpea-excluded-tags '("paper"))

  (org-agenda-window-setup 'only-window)
  (org-use-fast-todo-selection 'auto)
  (org-agenda-sticky nil)
  (org-agenda-restore-windows-after-quit t)

  (org-todo-keywords
   '((sequence "TODAY(i)" "PROG(p)" "NEXT(n)" "TODO(t)" "|" "DONE(d!/@)" "CANCELLED(c@/!)")
     ))
  (org-todo-keyword-faces
   '(("TODAY" :foreground "chocolate1")
     ("PROG" :foreground "turquoise")
     ("NEXT" :foreground "orchid")
     ("TODO" :foreground "orange")
     ("DONE" :foreground "chartreuse")
     ("CANCELLED" :foreground "deep pink")))
  (org-log-done nil)       ; Inactive timestamp already added to logbook
  (org-log-into-drawer t)

  (org-use-property-inheritance '("CATEGORY" "ARCHIVE")) ; Inherit these properties
  (org-archive-subtree-save-file-p t)   ; Save archive file always

  (org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "")
       (alltodo "")))
     ("p" "Priority" todo "PROG")
     ("w" "Working" todo "PROG|NEXT")
     ("d" "Done" todo "DONE|CANCELLED")
     ))
  (org-agenda-prefix-format
   '((agenda . " %i %(vulpea-agenda-category 22)%?-12t% s")
     (todo . " %i %(vulpea-agenda-category 22) ")
     (tags . " %i %(vulpea-agenda-category 22) ")
     (search . " %i %(vulpea-agenda-category 22) ")
     ))

  :config
  ;; I set to load only the first time I need it since it relies on org-roam,
  ;; and I don't want to explicitly load since that would increase startup time.
  (general-advice-add 'org-agenda :before #'(lambda (r)
                                              (require 'kb-vulpea)
                                              (vulpea-agenda-files-update))
                      nil t)

  ;; (add-to-list 'org-tags-exclude-from-inheritance "blog")
  )

;;; Org-agenda-property
;; Display org-agenda entries' properties alongside them
(use-package org-agenda-property
  :after org-agenda
  :custom
  (org-agenda-property-list '("LOCATION" "Effort"))
  )

;;; org-agenda-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-general-rcp)
