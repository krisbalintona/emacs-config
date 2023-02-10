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
  :hook ((org-agenda-finalize . (lambda () (goto-char (point-min))))
         (org-capture-before-finalize . kb/add-property-with-date-captured))
  :gfhook 'hl-line-mode
  :general (kb/open-keys "a" 'org-agenda)
  :custom
  (org-agenda-files (directory-files-recursively kb/agenda-dir (rx (literal ".org") eol)))

  ;; Effort
  (org-agenda-sort-noeffort-is-high nil)
  (org-effort-durations
   '(("m" . 1)
     ("h" . 60)
     ("d" . 1440)
     ("w" . 10080)
     ("mon" . 43200)
     ("y" . 525960.0)))

  ;; Clocking in and out
  (org-clock-out-when-done t)
  (org-clock-persist t)

  ;; Inheritance
  (org-use-tag-inheritance t)
  (org-tags-exclude-from-inheritance '("type"))
  (org-use-property-inheritance '("CATEGORY" "ARCHIVE"))
  (org-agenda-show-inherited-tags t)

  ;; Dependencies
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies nil)
  (org-agenda-dim-blocked-tasks t)

  ;; Org agenda
  (org-agenda-file-regexp "\\`[^.].*\\.org\\'")
  (org-agenda-sticky t) ; Set to nil if frequently modifying `org-agenda-custom-commands'
  (org-archive-subtree-save-file-p t)   ; Save archive file always
  (org-agenda-window-setup 'only-window)
  (org-use-fast-todo-selection 'expert)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-tags-column 'auto)
  (org-agenda-start-on-weekday 3)
  (org-agenda-format-date 'kb/org-agenda-format-date-aligned)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-scheduled nil)
  (org-agenda-remove-times-when-in-prefix t)
  (org-agenda-remove-tags 'prefix)
  (org-agenda-prefix-format
   '((agenda . "%2i %-12:c%?-12t% s")
     (todo . "%2i %s %b")
     (tags . "%2i %s %b")
     (search . "%2i %s %b")))
  (org-agenda-sorting-strategy
   '((agenda time-up habit-down ts-up deadline-up scheduled-up todo-state-up priority-down category-keep)
     (todo todo-state-up priority-down category-keep)
     (tags todo-state-up priority-down category-keep)
     (search todo-state-up priority-down category-keep)))
  ;; See
  ;; https://emacs.stackexchange.com/questions/17302/is-there-a-way-to-make-org-mode-count-repetitive-tasks-done-certain-hours-past-m?rq=1
  (org-extend-today-until 3)
  (org-use-effective-time t)
  (org-agenda-block-separator ?─)
  (org-agenda-scheduled-leaders
   '("Scheduled: " "Sched.%2dx: "))
  (org-agenda-deadline-leaders
   '("DEADLINE:  " "In %3d d.: " "%2d d. ago: "))
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-breadcrumbs-separator " ❱ ")

  ;; Capture templates
  (org-capture-templates
   `(("t" "Todo" entry
      (file ,(expand-file-name "todo.org" kb/agenda-dir))
      "* TODO %? %^g\n"
      :empty-lines 1)
     ("i" "Inbox" entry
      (file ,(expand-file-name "garden/20221011T101254--inbox.org" kb/notes-dir))
      "* %U %?\n"
      :empty-lines 1
      :jump-to-captured t)
     ("e" "Email" entry
      (file ,(expand-file-name "todo.org" kb/agenda-dir))
      "* TODO Respond to%? [[%L][\"%:subject\"]] :email:\n\nFrom %:from\nTo: %:to\n"
      :empty-lines 1)
     ;; NOTE 2023-01-01: Also see `mu4e--org-store-link-message' from mu4e-org
     ("E" "Mu4e-captured email" entry
      (file ,(expand-file-name "todo.org" kb/agenda-dir))
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
  :custom-face
  (org-mode-line-clock ((t (:inherit org-agenda-date))))
  :init
  ;; Todos
  (with-eval-after-load 'org
    ;; Needs to be loaded after `org' to take effect
    (setq org-fast-tag-selection-single-key 'expert
          org-todo-keywords
          '((sequence "PROG(p)" "ACTIVE(a)" "WAITING(w@/!)" "TODO(t)" "MAYBE(m)" "|" "DONE(d!/@)" "CANCELED(c@/!)"))
          org-todo-keyword-faces
          '(("PROG" . (bold success))
            ("ACTIVE" . org-warning)
            ("TODO" . org-todo)
            ("WAITING" . (shadow error))
            ("MAYBE" . (shadow org-todo))
            ("DONE" . (bold org-done))
            ("CANCEL" . error))
          org-log-done 'time
          org-log-into-drawer t
          org-highest-priority ?A
          org-lowest-priority ?E
          org-default-priority ?D
          org-priority-faces
          '((?A . (bold org-priority))
            (?B . org-priority)
            (?C . org-priority)
            (?D . (shadow org-priority))
            (?E . (shadow org-priority)))
          org-stuck-projects
          ;; FIXME 2023-01-23: Currently limits "projects" to top-level
          ;; headlines
          '("+LEVEL=1+project/-DONE-CANCELED" ("PROG" "ACTIVE") nil "")))

  ;; Taken from
  ;; https://github.com/psamim/dotfiles/blob/master/doom/config.el#L133
  (defun kb/add-property-with-date-captured ()
    "Add DATE_CAPTURED property to the current item."
    (interactive)
    (org-set-property "CREATED" (format-time-string "%F")))

  ;; Taken from
  ;; https://github.com/psamim/dotfiles/blob/master/doom/config.el#L213
  (defun kb/org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date 1 nil))
           (day (cadr date))
           (day-of-week (calendar-day-of-week date))
           (month (car date))
           (monthname (calendar-month-name month 1))
           (year (nth 2 date))
           (iso-week (org-days-to-iso-week
                      (calendar-absolute-from-gregorian date)))
           (weekyear (cond ((and (= month 1) (>= iso-week 52))
                            (1- year))
                           ((and (= month 12) (<= iso-week 1))
                            (1+ year))
                           (t year)))
           (weekstring (if (= day-of-week 1)
                           (format " W%02d" iso-week)
                         "")))
      (format " %-2s. %2d %s"
              dayname day monthname)))
  :config
  (org-clock-persistence-insinuate)

  (advice-add 'org-clock-get-clock-string
              :around (lambda (orig_fun &rest args)
                        "Truncate `org-clock-heading’."
                        (let ((org-clock-heading
                               (truncate-string-to-width org-clock-heading 40 nil nil (truncate-string-ellipsis))))
                          (apply orig_fun args)))))

;;; Org-super-agenda
(use-package org-super-agenda
  :after org-agenda
  :custom
  (org-super-agenda-hide-empty-groups t)
  (org-super-agenda-keep-order nil)
  :init
  (org-super-agenda-mode)
  :config
  (setq org-agenda-custom-commands
        '(("j" "At a glance"
           ((agenda ""
                    ((org-agenda-overriding-header "Schedule")
                     (org-agenda-show-inherited-tags t)
                     (org-agenda-use-tag-inheritance t)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-start-day "+0d")
                     (org-agenda-span 3)
                     (org-agenda-entry-types
                      '(:deadline :scheduled :timestamp :sexp))
                     (org-agenda-prefix-format
                      '((agenda . "%2i %-14c%?-12t %-7s %-7e %b")))
                     (org-agenda-skip-deadline-prewarning-if-scheduled t)
                     (org-agenda-skip-scheduled-if-deadline-is-shown t)
                     (org-deadline-warning-days 2)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-agenda-skip-deadline-if-done t)
                     (org-agenda-scheduled-leaders '("" "%2dx: "))
                     (org-habit-show-all-today t)
                     (org-habit-show-habits-only-for-today nil)
                     (org-agenda-include-diary t)
                     (org-agenda-insert-diary-extract-time t)))
            (tags-todo "-habit-reminder/-WAITING"
                       ((org-agenda-overriding-header "Unscheduled")
                        (org-agenda-dim-blocked-tasks 'invisible)
                        (org-agenda-show-inherited-tags t)
                        (org-agenda-use-tag-inheritance t)
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))
                        (org-super-agenda-groups
                         '((:discard (:and (:children t :tag "project")))
                           (:discard (:not (:todo ("PROG" "ACTIVE"))))
                           (:name ""
                            :not (:tag "project"))
                           (:name "Unscheduled project tasks"
                            :auto-parent t)
                           (:discard (:anything t))))))
            (tags-todo "-habit"
                       ((org-agenda-overriding-header "To process")
                        (org-agenda-use-tag-inheritance t)
                        (org-agenda-show-inherited-tags t)
                        (org-super-agenda-groups
                         '((:discard (:scheduled t))
                           (:discard (:deadline t))
                           (:discard (:todo ("PROG" "ACTIVE")))
                           (:name ""
                            :not (:tag "project"))
                           (:auto-parent t)
                           (:discard (:anything t))))))
            (alltodo ""
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-show-inherited-tags t)
                      (org-agenda-prefix-format
                       '((todo . "%2i %s ")))
                      (org-super-agenda-groups
                       '((:auto-parent t)
                         (:discard (:anything t))))))))
          ("E" "Emails"
           ((tags-todo "-reminder+email"
                       ((org-agenda-overriding-header "Unscheduled")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))))
            (agenda ""
                    ((org-agenda-overriding-header "Scheduled")
                     (org-agenda-start-day "-1w")
                     (org-agenda-span 21)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-scheduled-leaders '("" "%2dx: "))
                     (org-agenda-skip-function ; Only works for explicit tags
                      '(org-agenda-skip-entry-if 'notregexp ":email:"))))))
          ("A" "Archive" todo "DONE|CANCELED"))))

;;; Org-habit
(use-package org-habit
  :after org-agenda
  :straight nil
  :custom
  (org-habit-show-habits t)
  (org-habit-following-days 1)
  (org-habit-preceding-days 14)
  (org-habit-show-habits-only-for-today t)
  (org-habit-graph-column 70)
  (org-habit-today-glyph ?◌)
  (org-habit-completed-glyph ?●)
  (org-habit-missed-glyph ?○)

  ;; Useful
  (org-todo-repeat-to-state "ACTIVE")
  :config
  (add-to-list 'org-modules 'habit))

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
  (org-pomodoro-manual-break t) ; Allows for a workflow of going "overtime"
  (org-pomodoro-format "Pomodoro: %s")
  :config
  (defun kb/org-pomodoro-update-mode-line ()
    "Set the modeline accordingly to the current state."
    (let ((s (cl-case org-pomodoro-state
               (:pomodoro
                (propertize org-pomodoro-format 'face 'org-pomodoro-mode-line))
               (:overtime
                (propertize org-pomodoro-overtime-format
                            'face 'org-pomodoro-mode-line-overtime))
               (:short-break
                (propertize org-pomodoro-short-break-format
                            'face 'org-pomodoro-mode-line-break))
               (:long-break
                (propertize org-pomodoro-long-break-format
                            'face 'org-pomodoro-mode-line-break)))))
      (setq org-pomodoro-mode-line
            (when (and (org-pomodoro-active-p) (> (length s) 0))
              ;; Add space where I want it
              (list " [" (format s (org-pomodoro-format-seconds)) "]"))))
    (force-mode-line-update t))
  (advice-add 'org-pomodoro-update-mode-line :override 'kb/org-pomodoro-update-mode-line))

;;; Org-pomodoro-third-time
(use-package org-pomodoro-third-time
  :after org-agenda org-pomodoro
  :custom
  (org-pomodoro-third-time-break-to-work-ratio (/ 1.0 4.0))
  :init
  (org-pomodoro-third-time-mode))

;;; Org-depend
;; Add blocking and triggering actions when an org-todo state is changed.
(use-package org-depend
  :demand
  :after org-agenda
  :commands kb/consult-org-id-get-create)

;;; Org-edna
;; Also look at `org-edna' with `org-linker-edna'
;; (https://github.com/toshism/org-linker-edna) (the second of which requires
;; `org-linker': https://github.com/toshism/org-linker). `org-super-links' can
;; be added to see which tasks are being blocked by the current task. See
;; https://karl-voit.at/2021/01/23/org-linker-edna/ for sample workflow
(use-package org-edna
  :after org
  :diminish
  :general
  (:keymaps 'org-mode-map
   "C-c d" 'kb/consult-org-depend)
  (:keymaps 'org-agenda-mode-map
   "C-c d" 'kb/consult-org-agenda-depend)
  :init
  (org-edna-mode)
  :config
  (with-eval-after-load 'consult
    (defun kb/consult-org-depend--add-id (new-id)
      "Add an ID to the current heading’s BLOCKER property.
If none exists, automatically create the BLOCKER property. Code
based off of `org-linker-edna’."
      (let* ((value (org-entry-get (point) "BLOCKER"))
             (formatted-new-id
              (progn
                (unless (org-id-find new-id)
                  (error "This ID (%s) does not exist!" new-id))
                (list (concat "\"id:" new-id "\""))))
             (existing-ids
              ;; Get IDs if they exist in proper `org-edna' syntax as the value
              ;; of the BLOCKER property
              (when (and value (string-match "ids(\\([^\\)]*\\)).*" value))
                (split-string (match-string 1 value))))
             (all-ids (string-join (seq-uniq (append existing-ids formatted-new-id)) " "))
             (new-value (concat "ids(" all-ids ")")))
        (org-set-property "BLOCKER" new-value)))

    (defun kb/consult-org-depend (&optional match)
      "Create a dependency for the `org-todo’ at point.
A dependency is defined by `org-depend’s `BLOCKER’ property. IDs
are created in the todo dependency with `org-id-get-create’.
MATCH is a query sent to `org-map-entries’."
      (interactive)
      (let ((current-heading (org-get-heading))
            new-id dependency)
        (if (not (org-entry-is-todo-p))
            ;; Error if not currently on an `org-todo'
            (error "Not on an `org-todo’ heading!")
          ;; Add and ID to the dependency if necessary
          (save-excursion
            (consult-org-agenda (or match "/-DONE-CANCELED"))
            (setq dependency (org-get-heading))
            (when (equal current-heading dependency)
              (error "Cannot depend on the same `org-todo’!"))
            (setq new-id (org-id-get-create)))
          ;; Modify the BLOCKER property of the current todo
          (kb/consult-org-depend--add-id new-id)
          (message "‘%s’ added as a dependency to this todo"
                   (substring-no-properties dependency)))))

    (defun kb/consult-org-agenda-depend (&optional match)
      "Create a dependency for the `org-agenda’ item at point.
See `kb/consult-org-depend’."
      (interactive)
      (let* ((bufname-orig (buffer-name))
             (marker (or (org-get-at-bol 'org-marker)
                         (org-agenda-error)))
             (buffer (marker-buffer marker))
             (pos (marker-position marker))
             dependency)
        (org-with-remote-undo buffer
          (with-current-buffer buffer
            (save-excursion
              (goto-char pos)
              ;; FIXME 2023-01-17: Janky workaround. Remove all
              ;; `consult-after-jump-hook' hooks since we if there is a
              ;; `recenter' hook then an error will be returned since it'll be
              ;; attempting to `recenter' a non-present buffer
              (let ((consult-after-jump-hook nil))
                (setq dependency (funcall 'kb/consult-org-depend match))))))))

    (consult-customize kb/consult-org-depend
                       :prompt "Select dependency for the heading at point: "
                       kb/consult-org-agenda-depend
                       :prompt "Select dependency for this agenda item: ")))

;;; Org-gcal
(use-package org-gcal
  :custom
  ;; NOTE 2023-01-22: If syncing broke for some reason, try
  ;; `org-gcal-sync-tokens-clear'
  (org-gcal-client-id "477180658883-q2ok2j39ko4bfp88e2tqd9qi6c1r6ebm.apps.googleusercontent.com")
  (org-gcal-client-secret "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")
  (org-gcal-fetch-file-alist
   `(;; University
     ("v7tpr3s152ao11tlf93lu3don4@group.calendar.google.com" . ,(expand-file-name "gcal/brown.org" kb/agenda-dir))
     ("brown.edu_d61gju3thc3a7e58k84qbn1nc8@group.calendar.google.com" . ,(expand-file-name "gcal/crc_events.org" kb/agenda-dir))
     ("c_8ri64bp98ab1oj28704npqhig4@group.calendar.google.com" . ,(expand-file-name "gcal/assignments.org" kb/agenda-dir))
     ("c_g2s8uc0cufru3ruq7g7tbd1a7k@group.calendar.google.com" . ,(expand-file-name "gcal/bui.org" kb/agenda-dir))
     ("c_pr2pb1gdf5dkogh3h13kvs8uf0@group.calendar.google.com" . ,(expand-file-name "gcal/office_hours.org" kb/agenda-dir))
     ("independent_study@brown.edu" . ,(expand-file-name "gcal/independent_study.org" kb/agenda-dir))
     ("kristoffer_balintona@brown.edu" . ,(expand-file-name "gcal/kristoffer_balintona_events.org" kb/agenda-dir))
     ;; Personal
     ("ic4ecccdo60mub7raqhear02vg@group.calendar.google.com" . ,(expand-file-name "gcal/birthdays.org" kb/agenda-dir))))
  (org-gcal-up-days 31)
  (org-gcal-down-days 62)
  (org-gcal-recurring-events-mode 'top-level)
  (org-gcal-notify-p t)
  ;; Time zone
  ;; (org-gcal-local-timezone "America/Chicago")
  (org-gcal-local-timezone "America/New_York")
  :config
  (use-package plstore
    :demand
    :custom
    (plstore-cache-passphrase-for-symmetric-encryption t)))

;;; org-agenda-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-general-rcp)
