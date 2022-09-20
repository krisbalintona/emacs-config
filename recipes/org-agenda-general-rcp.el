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
  :gfhook 'hl-line-mode
  :general (kb/open-keys
             "a" '(org-agenda :wk "Org-agenda"))
  :custom
  (org-agenda-files nil) ; Instead , dynamically generate agenda list with vulpea
  (kb/vulpea-excluded-tags '("paper"))

  (org-agenda-window-setup 'only-window)
  (org-use-fast-todo-selection 'auto)
  (org-agenda-sticky nil)
  (org-agenda-restore-windows-after-quit t)

  (org-todo-keywords
   '((sequence "TODAY(i)" "PROG(p)" "NEXT(n)" "WAITING(w@/!)" "TODO(t)" "|" "DONE(d!/@)" "CANCELLED(c@/!)")
     ))
  (org-todo-keyword-faces
   '(("TODAY" :foreground "chocolate1")
     ("PROG" :foreground "turquoise")
     ("NEXT" :foreground "orchid")
     ("WAITING" :foreground "brown")
     ("TODO" :foreground "orange")
     ("DONE" :foreground "chartreuse")
     ("CANCELLED" :foreground "deep pink")))
  (org-log-done nil)       ; Inactive timestamp already added to logbook
  (org-log-into-drawer t)

  (org-use-property-inheritance '("CATEGORY" "ARCHIVE")) ; Inherit these properties
  (org-archive-subtree-save-file-p t)   ; Save archive file always

  (org-agenda-tags-column 170)
  (org-agenda-prefix-format
   '((agenda . " %i %(vulpea-agenda-category 22)%?-12t% s")
     (todo . " %i %(vulpea-agenda-category 22) ")
     (tags . " %i %(vulpea-agenda-category 22) ")
     (search . " %i %(vulpea-agenda-category 22) ")
     ))
  (org-agenda-sorting-strategy
   '((agenda category-up time-up habit-down priority-down) ; Sort by category first
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)
     ))

  (org-capture-templates
   `(("t" "Todo" entry
      (file ,(expand-file-name "todo.org" kb/agenda-dir))
      "* TODO %?\n" :empty-lines 1)
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

;;; Org-ql
;; More powerful searching and selecting of todo headlines
(use-package org-ql
  :requires org-agenda)

;;; Org-super-agenda
(use-package org-super-agenda
  :after org-agenda
  :ghook 'org-agenda-mode-hook
  :custom
  (org-agenda-custom-commands
   '(("T" "Time sensitive"
      ((alltodo ""
                ((org-agenda-overriding-header)
                 (org-super-agenda-groups
                  '((:auto-planning t)
                    (:discard (:anything t))
                    ))
                 ))
       ))
     ("o" "Overview"
      ((alltodo ""
                ((org-agenda-overriding-header "")
                 (org-super-agenda-groups
                  '((:name "Flagged"
                     :tag "FLAGGED")
                    (:name "Reminders"
                     :tag "REMIND")
                    (:discard (:anything t))
                    ))
                 ))
       (alltodo ""
                ((org-agenda-overriding-header "Courses")
                 (org-super-agenda-groups
                  '((:discard (:not (:file-path ("cs1730" "hist1974i" "hist0244" "phil1340"))))
                    (:auto-todo t)
                    ))
                 ))
       (alltodo ""
                ((org-agenda-overriding-header "Extracurriculars")
                 (org-super-agenda-groups
                  '((:discard (:not (:file-path ("crc" "bui" "buoy"))))
                    (:auto-todo t)
                    ))
                 ))
       (alltodo ""
                ((org-agenda-overriding-header "Other")
                 (org-super-agenda-groups
                  '((:discard (:not (:file-path "todo")))
                    (:auto-todo t)
                    ))
                 ))
       ))
     ("A" "Archive" todo "DONE|CANCELLED")
     )))

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
