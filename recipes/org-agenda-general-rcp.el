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
  (org-agenda-files `(,kb/agenda-dir))

  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies nil)
  (org-agenda-dim-blocked-tasks 'invisible) ; Invisible unless dependencies are done

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

  (org-use-property-inheritance '("CATEGORY" "ARCHIVE"))
  (org-use-tag-inheritance t)
  (org-tags-exclude-from-inheritance '("project" "PROJECT"))
  (org-archive-subtree-save-file-p t)   ; Save archive file always

  (org-agenda-tags-column 170)
  (org-agenda-prefix-format
   '((agenda . " %i %(kb/agenda-category 14)%?-12t% s")
     (todo . " %i %(kb/agenda-category 14)  %(org-agenda-view--insert-deadline)   ")
     (tags . " %i %(kb/agenda-category 14) ")
     (search . " %i %(kb/agenda-category 14) ")
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
      "* TODO %?\n"
      :empty-lines 1)
     ("i" "Inbox" entry
      (file ,(expand-file-name "garden/20221011T101254--inbox.org" kb/notes-dir))
      "* %U %?\n"
      :empty-lines 1
      :jump-to-captured t)
     ("e" "Email" entry
      (file ,(expand-file-name "emails.org" kb/agenda-dir))
      "* TODO Respond to%? [[%L][\"%:subject\" from %:fromname on %:date]]\n"
      :empty-lines 1)
     ;; NOTE 2023-01-01: Also see `mu4e--org-store-link-message' from mu4e-org
     ("E" "Mu4e-captured email" entry
      (file ,(expand-file-name "emails.org" kb/agenda-dir))
      ,(concat "* TODO Respond to%? "
               "[[mu4e:msgid:%(plist-get mu4e-captured-message :message-id)]"
               "[\"%(plist-get mu4e-captured-message :subject)\" "
               "from %(plist-get (car (plist-get mu4e-captured-message :from)) :name) "
               "on %(format-time-string \"%F\" (plist-get mu4e-captured-message :date))]]\n")
      :empty-lines 1)))
  (org-capture-templates-contexts
   '(("e" ((in-mode . "mu4e-headers-mode")))
     ("e" ((in-mode . "mu4e-view-mode")))
     ("E" ((lambda () (bound-and-true-p mu4e-captured-message))))))
  :config
  ;; Used in org-agenda to replace the categories with note titles. Taken from
  ;; `vulpea' library
  (defun kb/note-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
        (string-trim
         (buffer-substring-no-properties
          (match-beginning 1)
          (match-end 1))))))
  (defun kb/agenda-category (&optional len)
    "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(kb/agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
           (title (kb/note-buffer-prop-get "title"))
           (category (org-get-category))
           (result
            (or (if (and
                     title
                     (string-equal category file-name))
                    title
                  category)
                "")))
      (if (numberp len)                ; Truncates if too long, according to len
          (s-truncate len (s-pad-right len " " result))
        result)))

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
   '(("E" "Emails"
      ((alltodo ""
                ((org-agenda-overriding-header "Emails")
                 (org-super-agenda-groups
                  '((:and (:file-path "emails"
                           :deadline t))
                    (:and (:file-path "emails"
                           :scheduled t))
                    (:file-path "emails")
                    (:discard (:anything t))
                    ))
                 ))
       ))
     ("o" "Overview"
      ((alltodo ""
                ((org-agenda-overriding-header "Revisit")
                 (org-super-agenda-groups
                  '((:name "Stuck projects"
                     :tag "PROJECT")
                    (:name "Should be snoozed"
                     :todo "TODO"
                     :and (:todo ("FAR" "WAITING")
                           :scheduled nil))
                    (:name "Snoozed"
                     :scheduled past
                     :scheduled today)
                    (:discard (:children todo))
                    (:discard (:anything t))
                    ))
                 ))
       (alltodo ""
                ((org-agenda-overriding-header "Courses")
                 (org-super-agenda-groups
                  `((:discard (:not (:file-path ("cs1730" "hist1974i" "hist0244" "phil1340"))))
                    (:discard (:and (:tag "PROJECT"
                                     :deadline ; Include only those with deadline 2 days into the future
                                     ;; Taken from
                                     ;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org#concrete-dates
                                     (after ,(-let* (((sec minute hour day month year dow dst utcoff) (decode-time)))
                                               (format "%d-%02d-%02d" year month (+ 2 day)))))))
                    (:discard (:todo "FAR"))
                    (:discard (:scheduled future))
                    (:auto-todo t)
                    ))
                 ))
       (alltodo ""
                ((org-agenda-overriding-header "Projects")
                 (org-super-agenda-groups
                  '((:discard (:todo "FAR"))
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
