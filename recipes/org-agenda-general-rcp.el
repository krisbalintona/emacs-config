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
  :general (kb/open-keys "a" 'org-agenda)
  :custom
  (org-agenda-files (list kb/agenda-dir))

  ;; Todos
  (org-enforce-todo-dependencies nil)
  (org-enforce-todo-checkbox-dependencies nil)
  (org-fast-tag-selection-single-key 'expert)
  (org-todo-keywords
   '((sequence "ACTIVE(a)" "TODO(t)" "WAITING(w@/!)" "MAYBE(m)" "|" "DONE(d!/@)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces
   '(("ACTIVE" . '(bold org-todo))
     ("TODO" . 'org-todo)
     ("WAITING" . '(shadow error))
     ("MAYBE" . '(shadow org-todo))
     ("DONE" . '(bold org-done))
     ("CANCEL" . 'error)))
  (org-log-done t)
  (org-log-into-drawer t)
  (org-highest-priority ?A)
  (org-lowest-priority ?E)
  (org-default-priority ?D)
  (org-priority-faces
   '((?A . '(bold org-priority))
     (?B . org-priority)
     (?C . org-priority)
     (?D . '(shadow org-priority))
     (?E . '(shadow org-priority))))

  ;; Clocking in and out
  (org-clock-out-when-done t)
  (org-clock-persist t)

  ;; Inheritance
  (org-use-tag-inheritance t)
  (org-tags-exclude-from-inheritance '("project"))
  (org-use-property-inheritance '("CATEGORY" "ARCHIVE"))
  (org-agenda-show-inherited-tags nil)

  ;; Org agenda
  (org-agenda-file-regexp "\\`[^.].*\\.org\\'")
  (org-agenda-sticky t) ; Set to nil if frequently modifying `org-agenda-custom-commands'
  (org-agenda-window-setup 'only-window)
  (org-use-fast-todo-selection 'expert)
  (org-agenda-dim-blocked-tasks 'invisible) ; Invisible unless dependencies are done
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-tags-column 'auto)
  (org-agenda-start-on-weekday nil)     ; Start with today
  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo . " %i %-12:c %b ")
     (tags . " %i %-12:c %b ")
     (search . " %i %-12:c")))
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up ts-up priority-down category-keep)
     (todo priority-down todo-state-up category-keep)
     (tags priority-down todo-state-up category-keep)
     (search category-keep)))
  (org-archive-subtree-save-file-p t)   ; Save archive file always

  ;; Capture templates
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
  (org-clock-persistence-insinuate))

;;; Org-super-agenda
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("j" "At a glance"
           ((tags-todo "+snooze"
                       ((org-agenda-overriding-header "Snoozed without date")
                        (org-use-tag-inheritance nil)
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))))
            (tags-todo "+snooze"
                       ((org-agenda-overriding-header "Snoozed")
                        (org-use-tag-inheritance nil)
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'notscheduled))
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "+course-snooze/-MAYBE"
                       ((org-agenda-overriding-header "Courses")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "+work-snooze/-MAYBE"
                       ((org-agenda-overriding-header "Work")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "+org-snooze/-MAYBE"
                       ((org-agenda-overriding-header "Extracurriculars")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-snooze/-MAYBE"
                       ((org-agenda-overriding-header "Other")
                        (org-agenda-files (list (expand-file-name "todo.org" kb/agenda-dir)))
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)))
            (agenda ""
                    ((org-agenda-overriding-header "Upcoming deadlines")
                     (org-agenda-start-day "-3d")
                     (org-agenda-span 24)
                     (org-agenda-show-all-dates nil)
                     (org-deadline-warning-days 0)
                     (org-agenda-entry-types '(:deadline))
                     (org-agenda-include-diary t)))
            (tags-todo "+project-snooze/-MAYBE"
                       ((org-agenda-overriding-header "Projects")))
            (tags-todo "-snooze/+MAYBE"
                       ((org-agenda-overriding-header "Maybes")))))
          ("E" "Emails"
           ((agenda ""
                    ((org-agenda-overriding-header "Emails")
                     (org-agenda-start-day "-1w")
                     (org-agenda-span 21)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-files (list (expand-file-name "emails.org" kb/agenda-dir)))))))
          ("A" "Archive" todo "DONE|CANCELLED"))))

;;; Org-agenda-property
;; Display org-agenda entries' properties alongside them
(use-package org-agenda-property
  :after org-agenda
  :custom
  (org-agenda-property-list '("LOCATION" "Effort"))
  )

;;; Org-pomodoro
(use-package org-pomodoro
  :general (:keymaps 'org-agenda-mode-map "P" 'org-pomodoro)
  :custom
  (org-pomodoro-length 25)
  (org-pomodoro-long-break-length 20)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-clock-break nil)
  (org-pomodoro-ask-upon-killing t)
  (org-pomodoro-keep-killed-pomodoro-time t)
  (org-pomodoro-manual-break t))     ; Allows for a workflow of going "overtime"

;;; org-agenda-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-general-rcp)
