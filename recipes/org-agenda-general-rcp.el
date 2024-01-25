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
  :elpaca nil
  ;; Call after `org' since some of the options below are from `org', not
  ;; `org-export', so they will be overwritten if this use-package loads before
  ;; `org' does
  :after org
  :hook ((org-agenda-finalize . (lambda () (goto-char (point-min))))
         (org-capture-before-finalize . kb/add-property-with-date-captured)
         (org-after-todo-state-change . kb/org-todo-project-prog))
  :general
  (kb/open-keys "a" 'org-agenda)
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

  ;; Inheritance
  (org-use-tag-inheritance t)
  (org-tags-exclude-from-inheritance '("project"))
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
  (org-agenda-sorting-strategy
   '((agenda time-up habit-down deadline-up priority-down todo-state-up scheduled-up category-keep)
     (todo todo-state-up priority-down category-keep)
     (tags todo-state-up priority-down category-keep)
     (search todo-state-up priority-down category-keep)))
  ;; See
  ;; https://emacs.stackexchange.com/questions/17302/is-there-a-way-to-make-org-mode-count-repetitive-tasks-done-certain-hours-past-m?rq=1
  (org-extend-today-until 3)
  (org-use-effective-time t)
  (org-agenda-block-separator ?—)
  (org-deadline-warning-days 0)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-breadcrumbs-separator " -> ")
  (org-agenda-skip-scheduled-delay-if-deadline nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-auto-exclude-function #'kb/org-agenda-auto-exclude)

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

  ;; Todos
  (org-fast-tag-selection-single-key 'expert)
  (org-todo-keywords
   '((sequence "PROG(p)" "ACTIVE(a)" "WAITING(w@/!)" "TODO(t)" "MAYBE(m)" "|" "DONE(d!/@)" "CANCELED(c@/!)")))
  (org-todo-keyword-faces
   '(("PROG" . (bold success))
     ("ACTIVE" . org-warning)
     ("TODO" . org-todo)
     ("WAITING" . (shadow error))
     ("MAYBE" . (shadow org-todo))
     ("DONE" . (bold org-done))
     ("CANCEL" . error)))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-highest-priority ?A)
  (org-lowest-priority ?E)
  (org-default-priority ?D)
  (org-priority-faces
   '((?A . (bold org-priority))
     (?B . org-priority)
     (?C . org-priority)
     (?D . (shadow org-priority))
     (?E . (shadow org-priority))))
  (org-stuck-projects
   ;; Stuck projects are those that...
   ;; 1. Have the "project" tag
   ;; 2. Do not have the DONE or CANCELED todo keywords
   ;; 3. Do not have todos that have the PROG or ACTIVE keywords
   ;; 4. Do not have tasks that match the `org-not-done-heading-regexp' followed
   ;; by the `org-scheduled-time-regexp' regexp
   `("+project/-DONE-CANCELED"
     ("PROG" "ACTIVE")
     nil
     ,(rx (regexp org-not-done-heading-regexp)
          (regexp org-scheduled-time-regexp))))
  :custom-face
  (org-mode-line-clock ((t (:inherit org-agenda-date))))
  :config
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

  ;; For `org-agenda'
  (defun kb/org-todo-project-prog ()
    "Project is set to \"PROG\" under certain conditions.
Side effects occur if the parent of the current headline has a
\"project\" tag. The parent headline's todo-keyword is changed to
\"PROG\" if the current headline's todo-keyword is \"PROG\"."
    (let* ((parent (org-element-property :parent (org-element-at-point)))
           (parent-tags (org-element-property :tags parent)))
      (when (member "project" parent-tags)
        (save-excursion
          (goto-char (org-element-property :begin parent))
          (when (string= org-state "PROG")
            (org-todo "PROG"))))))
  (defun kb/org-agenda-skip-if-not-stuck-project ()
    "tk"
    (let* ((top-level-p (org-edna-finder/parent 'todo-only))
           (siblings (org-edna-finder/rest-of-siblings 'from-top 'todo-only))
           todos)
      (when top-level-p                   ; Don't skip if top-level todo
        (setq todos (cl-loop
                     for sibling in siblings
                     collect (save-excursion
                               (save-window-excursion
                                 (org-goto-marker-or-bmk sibling)
                                 (org-element-property :todo-keyword (org-element-at-point))))))
        ;; Ignore if there is a PROG or ACTIVE todo keyword among siblings
        (when (or (member "PROG" todos) (member "ACTIVE" todos))
          (org-entry-end-position)))))

  (defun kb/org-agenda-auto-exclude (tag)
    "Set tags based on time. See (org) Filtering/limiting agenda items"
    (when (cond ((member tag '("@home"))
                 (let ((hr (nth 2 (decode-time))))
                   ;; After 10 or before 21
                   (or (> hr 10) (< hr 21)))))
      (concat "-" tag))))

;;; Org-super-agenda
(use-package org-super-agenda
  :demand
  :after org-agenda
  :custom
  (org-super-agenda-hide-empty-groups t)
  (org-super-agenda-keep-order t)
  :init
  (defun kb/org-agenda-breadcrumb (len)
    "Formatted breadcrumb for current `org-agenda' item."
    (org-with-point-at (org-get-at-bol 'org-marker)
      (let ((s (if (derived-mode-p 'org-mode)
                   (org-format-outline-path (org-get-outline-path)
                                            (1- (frame-width))
                                            nil org-agenda-breadcrumbs-separator)
                 ;; Not in Org buffer. This can happen, for example, in
                 ;; `org-agenda-add-time-grid-maybe' where time grid does not
                 ;; correspond to a particular heading.
                 "")))
        (if (equal "" s) ""
          (concat (truncate-string-to-width s len 0 nil (truncate-string-ellipsis)) org-agenda-breadcrumbs-separator)))))
  :config
  (org-super-agenda-mode)
  (setq org-agenda-custom-commands
        '(("n" "Now"
           ((alltodo ""
                     ((org-agenda-overriding-header "High priority but unscheduled")
                      (org-super-agenda-groups
                       '((:discard (:todo ("PROG" "ACTIVE" "WAITING")))
                         (:discard (:scheduled t))
                         (:discard (:tag "project"))
                         (:name ""
                                :priority>= "B")
                         (:discard (:anything t))))))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Unscheduled waiting")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'scheduled))))
            (alltodo ""
                     ((org-agenda-overriding-header "Expedited")
                      (org-agenda-prefix-format
                       '((todo . "  %-7e%-25(kb/org-agenda-breadcrumb 21)%?-10t")))
                      (org-super-agenda-groups
                       '((:discard (:scheduled t))
                         (:name ""
                                :and (:not (:scheduled today)
                                           :not (:tag "project")
                                           :todo ("PROG" "ACTIVE")))
                         (:discard (:anything t))))))
            (agenda ""
                    ((org-agenda-overriding-header "Tasks")
                     (org-agenda-show-inherited-tags t)
                     (org-agenda-sorting-strategy
                      '((agenda time-up habit-down priority-down deadline-up todo-state-up)))
                     (org-agenda-start-day "+0d")
                     (org-agenda-span 1)
                     (org-agenda-prefix-format
                      '((agenda . "  %-7e%-25(kb/org-agenda-breadcrumb 21)%-5s%?-10t")))
                     (org-agenda-scheduled-leaders '("" "%2dx:"))
                     (org-agenda-deadline-leaders '("" "In %3d d.:" "%2d d. ago:"))
                     (org-agenda-skip-deadline-prewarning-if-scheduled t)
                     (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
                     (org-habit-show-all-today nil)
                     (org-habit-show-habits-only-for-today nil)
                     (org-agenda-dim-blocked-tasks t)
                     (org-agenda-include-diary t)
                     (org-agenda-insert-diary-extract-time t)
                     (org-super-agenda-groups
                      '((:name "Projects in need of delegation"
                               :and (:tag "project"
                                          :scheduled t))
                        (:name "Projects due"
                               :and (:tag "project"
                                          :deadline t))
                        (:name "Orphans"
                               :anything t)))))))
          ("p" "Planning"
           ((tags-todo "+project"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-show-inherited-tags t)
                        (org-agenda-dim-blocked-tasks nil)
                        (org-agenda-prefix-format
                         '((tags . "%-25(kb/org-agenda-breadcrumb 21)%?s")))
                        (org-super-agenda-groups
                         '((:discard (:todo "PROG"))
                           (:discard (:not (:tag "project")))
                           (:name "Undated"
                                  :and (:not (:scheduled t)
                                             :not (:deadline t)))
                           (:name "Dated"
                                  :scheduled t
                                  :deadline t)
                           (:discard (:anything t))))))
            (agenda ""
                    ((org-agenda-overriding-header "Timeline")
                     (org-agenda-show-inherited-tags t)
                     (org-agenda-start-day "+0d")
                     (org-agenda-span 3)
                     (org-agenda-entry-types
                      '(:deadline :scheduled :timestamp :sexp))
                     (org-agenda-prefix-format
                      '((agenda . "  %-7e%-25(kb/org-agenda-breadcrumb 21)%-5s%?-10t")))
                     (org-agenda-scheduled-leaders '("" "%2dx: "))
                     (org-agenda-deadline-leaders '("" "In %3d d.: " "%2d d. ago: "))
                     (org-agenda-skip-deadline-prewarning-if-scheduled t)
                     (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
                     (org-agenda-skip-deadline-if-done t)
                     (org-habit-show-all-today nil)
                     (org-habit-show-habits-only-for-today nil)
                     (org-agenda-dim-blocked-tasks t)
                     (org-agenda-include-diary t)
                     (org-agenda-insert-diary-extract-time t)
                     (org-super-agenda-groups
                      '((:name "Projects"
                               :and (:tag "project"
                                          :scheduled t))
                        (:name "Orphans"
                               :anything t)))))
            (alltodo ""
                     ((org-agenda-overriding-header "Unscheduled")
                      (org-agenda-prefix-format
                       '((todo . "%-25(kb/org-agenda-breadcrumb 21)%-5s")))
                      (org-agenda-skip-function 'kb/org-agenda-skip-if-not-stuck-project)
                      (org-super-agenda-groups
                       '((:discard (:todo ("PROG" "ACTIVE" "WAITING" "MAYBE")))
                         (:discard (:not (:scheduled nil)))
                         ;; FIXME 2023-07-20: Would like to use :auto-priority,
                         ;; but this is erroring for me. Perhaps the issue is on
                         ;; the package side, e.g., unsupported version of org?
                         (:discard (:tag "project"))
                         (:name "" :anything t)))))
            (todo "MAYBE"
                  ((org-agenda-overriding-header "Maybes")
                   (org-agenda-dim-blocked-tasks 'invisible)
                   (org-super-agenda-groups
                    '((:discard (:scheduled t))
                      (:discard (:tag "project"))
                      (:name ""
                             :anything t)))))))
          ("u" "Upcoming due dates and ongoing projects"
           ((agenda ""
                    ((org-agenda-overriding-header "Upcoming dates")
                     (org-agenda-start-day "+0d")
                     (org-agenda-span 9)
                     (org-agenda-show-inherited-tags t)
                     (org-agenda-dim-blocked-tasks t)
                     (org-agenda-sorting-strategy
                      '((agenda time-up habit-down priority-down deadline-up todo-state-up)))
                     (org-agenda-prefix-format
                      '((agenda . "%2i %-14c%?-12t %-7s %-7e %b")))
                     (org-agenda-entry-types '(:deadline))))
            (tags-todo "+project"
                       ((org-agenda-overriding-header "Ongoing projects")
                        (org-agenda-dim-blocked-tasks t)
                        (org-agenda-use-tag-inheritance nil)))))
          ("d" "Done and cancelled"
           ((tags "TODO=\"DONE\"&LEVEL=1|TODO=\"CANCELED\"&LEVEL=1"))))))

;;; Org-clock
(use-package org-clock
  :demand
  :elpaca nil
  :after org-agenda
  :custom
  (org-clock-persist t)
  (org-clock-out-when-done t)
  (org-clock-history-length 10)
  (org-clock-in-resume nil)
  (org-clock-persist-query-resume t)
  (org-clock-into-drawer t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-show-notification-handler #'(lambda (str)
                                     (notifications-notify
                                      :title "Org-agenda task overrun!"
                                      :body str
                                      :app-name "GNU Emacs"
                                      :urgency 'normal)))
  ;; Mode line
  (org-clock-string-limit 0)
  (org-clock-heading-function 'kb/org-clock-get-heading-string)
  :config
  (org-clock-persistence-insinuate)
  ;; Mode line string
  (defun kb/org-clock-get-heading-string ()
    "Get truncated org heading string.

Same as default but truncates with `truncate-string-ellipsis'."
    (let ((heading (org-link-display-format
                    (org-no-properties (org-get-heading t t t t)))))
      (truncate-string-to-width heading 40 nil nil (truncate-string-ellipsis)))))

;;; Org-habit
(use-package org-habit
  :after org-agenda
  :elpaca nil
  :custom
  (org-habit-show-habits t)
  (org-habit-following-days 1)
  (org-habit-preceding-days 14)
  (org-habit-show-done-always-green t)
  (org-habit-show-habits-only-for-today t)
  (org-habit-graph-column 110)
  (org-habit-today-glyph ?◌)
  (org-habit-completed-glyph ?●)
  (org-habit-missed-glyph ?○)

  ;; Useful
  (org-todo-repeat-to-state "ACTIVE"))


;;; Work-timer
(use-package work-timer
  :demand
  :elpaca (:host github
                 :protocol ssh
                 :repo "krisbalintona/work-timer"
                 :depth nil
                 :files (:defaults "*.mp3"))
  :hook (kb/themes . kb/work-timer-set-faces)
  :general (kb/open-keys
             "w" work-timer-prefix-map)
  :custom
  (work-timer-debug t)
  (work-timer-time-format "%.2m:%.2s")
  (work-timer-default-work-duration 25)
  (work-timer-default-break-duration 5)
  (work-timer-work-duration-function 'work-timer-work-duration-fractional)
  (work-timer-break-duration-function 'work-timer-break-duration-fractional)
  ;; (work-timer-work-duration-function 'work-timer-work-duration-pomodoro)
  ;; (work-timer-break-duration-function 'work-timer-break-duration-pomodoro)
  ;; (work-timer-fractional-work-duration 0.5)
  ;; (work-timer-fractional-break-duration-fraction 1)
  (work-timer-fractional-work-duration 25)
  (work-timer-fractional-break-duration-fraction 0.15)
  :init
  ;; Save relevant current timer variables to resume timer across Emacs sessions
  (dolist (var '(work-timer-start-time
                 work-timer-duration
                 work-timer-type))
    (add-to-list 'savehist-additional-variables var))

  (defun kb/work-timer-set-faces ()
    "Set `work-timer-mode-line' according to dark or light theme."
    (let* ((dark-p
            (color-dark-p (color-name-to-rgb (face-attribute 'default :background))))
           (initial-color "DarkOrange")
           (foreground
            (if dark-p
                initial-color
              (color-darken-name initial-color 15))))
      (set-face-foreground 'work-timer-mode-line foreground)))

  :config
  (work-timer-with-org-clock-mode))

;;; org-agenda-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-general-rcp)
