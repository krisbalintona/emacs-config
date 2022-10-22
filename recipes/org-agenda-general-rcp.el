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
  :hook (org-agenda-finalize . (lambda () (goto-char (point-min))))
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
   '((sequence "PROG(p)" "TODAY(i)" "NEAR(n)" "HORIZON(h)" "FAR(f)" "WAITING(w@/!)" "TODO(t)" "|" "DONE(d!/@)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces
   '(("PROG" :foreground "turquoise")
     ("TODAY" :foreground "chocolate1")
     ("NEAR" :foreground "orchid")
     ("HORIZON" :foreground "deep sky blue")
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
   '((agenda . " %i %(vulpea-agenda-category 14)%?-12t% s")
     (todo . " %i %(vulpea-agenda-category 14)  %(org-agenda-view--insert-deadline)   ")
     (tags . " %i %(vulpea-agenda-category 14) ")
     (search . " %i %(vulpea-agenda-category 14) ")
     ))
  (org-agenda-sorting-strategy
   '((agenda category-up deadline-up scheduled-up time-up habit-down priority-down)
     (todo priority-down deadline-up scheduled-up category-up)
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

  ;; For org-agenda views
  (defun org-agenda-view--insert-deadline ()
    (let* ((deadline (org-get-deadline-time (point)))
           (time-string (format-time-string "%a %m/%d" deadline)))
      (if deadline
          time-string
        (make-string 9 ? )))))

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
   '(("c" "Deadlines and scheduled"
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
                ((org-agenda-overriding-header "Revisit")
                 (org-super-agenda-groups
                  '((:name "Should be snoozed"
                     :todo "TODO"
                     :and (:todo ("FAR" "WAITING")
                           :scheduled nil))
                    (:name "Snoozed"
                     :scheduled past
                     :scheduled today)
                    (:discard (:children todo))
                    (:name "Stuck projects"
                     :tag "PROJECT")
                    (:discard (:anything t))
                    ))
                 ))
       (alltodo ""
                ((org-agenda-overriding-header "Courses")
                 (org-super-agenda-groups
                  '((:discard (:not (:file-path ("cs1730" "hist1974i" "hist0244" "phil1340"))))
                    (:discard (:tag "PROJECT"))
                    (:discard (:todo "FAR"))
                    (:discard (:scheduled future))
                    (:auto-todo t)
                    ))
                 ))
       (alltodo ""
                ((org-agenda-overriding-header "Projects")
                 (org-super-agenda-groups
                  '((:discard (:not (:file-path ("cs1730" "hist1974i" "hist0244" "phil1340"))))
                    (:discard (:todo "FAR"))
                    (:discard (:scheduled future))
                    (:auto-parent t)
                    (:discard (:anything t))
                    ))
                 ))
       (alltodo ""
                ((org-agenda-overriding-header "Extracurriculars")
                 (org-super-agenda-groups
                  '((:discard (:not (:file-path ("crc" "bui" "buoy"))))
                    (:discard (:todo "FAR"))
                    (:discard (:scheduled future))
                    (:auto-todo t)
                    ))
                 ))
       (alltodo ""
                ((org-agenda-overriding-header "Other")
                 (org-super-agenda-groups
                  '((:discard (:not (:file-path "todo")))
                    (:discard (:todo "FAR"))
                    (:discard (:scheduled future))
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
