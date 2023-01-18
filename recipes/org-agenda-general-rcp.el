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
  (org-agenda-files (list kb/agenda-dir))

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
  (org-tags-exclude-from-inheritance '("project" "type"))
  (org-use-property-inheritance '("CATEGORY" "ARCHIVE"))
  (org-agenda-show-inherited-tags nil)

  ;; Dependencies
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-agenda-dim-blocked-tasks t)

  ;; Org agenda
  (org-agenda-file-regexp "\\`[^.].*\\.org\\'")
  (org-agenda-sticky t) ; Set to nil if frequently modifying `org-agenda-custom-commands'
  (org-agenda-window-setup 'only-window)
  (org-use-fast-todo-selection 'expert)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-tags-column 'auto)
  (org-agenda-start-on-weekday nil)     ; Start with today
  (org-agenda-format-date 'kb/org-agenda-format-date-aligned)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-scheduled 'future) ; This is for my own workflow
  (org-agenda-remove-times-when-in-prefix t)
  (org-agenda-remove-tags 'prefix)
  (org-agenda-prefix-format
   '((agenda . "%2i %-12:c%?-12t% s")
     (todo . "%2i %s %b")
     (tags . "%2i %s %b")
     (search . "%2i %s %b")))
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up ts-up priority-down category-keep)
     (todo todo-state-up priority-down category-keep)
     (tags todo-state-up priority-down category-keep)
     (search todo-state-up priority-down category-keep)))
  (org-archive-subtree-save-file-p t)   ; Save archive file always
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

  ;; Org habit
  ;; REVIEW 2023-01-08: The following is untested. I just copied from
  ;; https://github.com/psamim/dotfiles/blob/master/doom/config.el#L239
  (org-habit-today-glyph ?◌)
  (org-habit-graph-column 40)
  (org-habit-following-days 1)
  (org-habit-show-habits t)
  (org-habit-completed-glyph ?●)
  (org-habit-preceding-days 10)
  (org-habit-show-habits-only-for-today t)
  (org-habit-missed-glyph ?○)

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
      "* TODO Respond to%? [[%L][\"%:subject\" from %:fromto]] :email:\n"
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
          '((sequence "PROG(p)" "ACTIVE(a)" "WAITING(w@/!)" "TODO(t)" "MAYBE(m)" "|" "DONE(d!/@)" "CANCELLED(c@/!)"))
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
          '("+project/-DONE-CANCELLED" ("*") nil "")))

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

;;; Org-agenda-custom-commands
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("j" "At a glance"
           ((tags-todo "+snooze"
                       ((org-agenda-overriding-header "Snoozed without date")
                        (org-use-tag-inheritance nil)
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))))
            (agenda ""
                    ((org-agenda-overriding-header "Snoozed")
                     (org-use-tag-inheritance nil)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-start-day "-30d")
                     (org-agenda-span 32)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notscheduled))
                     (org-agenda-skip-deadline-prewarning-if-scheduled t)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-agenda-scheduled-leaders '("" ""))
                     (org-agenda-include-diary nil)))
            (agenda ""
                    ((org-agenda-overriding-header "Upcoming deadlines")
                     (org-agenda-start-day "+0d")
                     (org-agenda-span 14)
                     (org-agenda-show-all-dates nil)
                     (org-deadline-warning-days 0)
                     (org-agenda-entry-types '(:deadline))
                     (org-agenda-skip-deadline-if-done t)
                     (org-agenda-deadline-leaders
                      '("" "In %3d d.: " "%2d d. ago: "))
                     (org-agenda-include-diary t)))
            (tags-todo "-snooze-project+PRIORITY=\"A\"/+ACTIVE"
                       ((org-agenda-overriding-header "High priority")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "+project/-MAYBE"
                       ((org-agenda-overriding-header "Projects")))
            (tags-todo "-snooze/+MAYBE"
                       ((org-agenda-overriding-header "Maybes")))))
          ("E" "Emails"
           ((tags-todo "-snooze+email"
                       ((org-agenda-overriding-header "Unscheduled")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))))
            (agenda ""
                    ((org-agenda-overriding-header "Scheduled")
                     (org-agenda-start-day "-1w")
                     (org-agenda-span 21)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-scheduled-leaders '("" ""))
                     (org-agenda-skip-function ; Only works for explicit tags
                      '(org-agenda-skip-entry-if 'notregexp ":email:"))))))
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
            (consult-org-agenda match)
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
                (setq dependency (funcall 'kb/consult-org-depend nil match))))))))

    (consult-customize kb/consult-org-depend
                       :prompt "Select dependency for the heading at point: "
                       kb/consult-org-agenda-depend
                       :prompt "Select dependency for this agenda item: ")))

;;; org-agenda-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-general-rcp)
