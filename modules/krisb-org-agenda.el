;;; Calendar
(use-package calendar
  :ensure nil
  :bind ( :map krisb-open-keymap
          ("c" . calendar))
  :custom
  (calendar-time-display-form
   '( 24-hours ":" minutes (when time-zone (format "(%s)" time-zone))))
  (calendar-week-start-day 1)           ; Monday
  (calendar-time-zone-style 'symbolic)

  ;; Diary
  (calendar-mark-diary-entries-flag t)

  ;; Holidays
  (calendar-mark-holidays-flag t)
  :init
  (defun krisb-get-lat-lon ()
    "Fetch latitude and longitude via IP-based geolocation service."
    (let (lat lon (timeout 0))
      (url-retrieve "http://ip-api.com/json"
                    (lambda (_status)
                      (goto-char (point-min))
                      (re-search-forward "^$")
                      (let* ((json-object-type 'hash-table)
                             (json (json-read)))
                        (setq lat (gethash "lat" json)
                              lon (gethash "lon" json)))))
      ;; Wait until the data is retrieved or timeout.
      (while (and (not lat) (< timeout 50))
        (setq timeout (1+ timeout))
        (sit-for 0.1))
      (when (= timeout 50)
        (message "[krisb-get-lat-lon] Maximum timeout reached"))
      (if (and lat lon)
          (cons lat lon)
        (message "[krisb-get-lat-lon] Failed to fetch geolocation data")
        nil)))

  (defun krisb-get-location-name ()
    "Get the current location."
    (let (city region (timeout 0))
      (url-retrieve "http://ip-api.com/json"
                    (lambda (status)
                      (goto-char (point-min))
                      (re-search-forward "\n\n")  ;; Skip the headers
                      (let* ((json-object-type 'hash-table)
                             (json-key-type 'string)
                             (json-array-type 'list)
                             (data (json-read)))
                        (setq city (gethash "city" data)
                              region (gethash "region" data)))))
      ;; Wait until the data is retrieved or timeout.
      (while (and (not city) (not region) (< timeout 50))
        (setq timeout (1+ timeout))
        (sit-for 0.1))
      (when (= timeout 50)
        (message "[krisb-get-location-name] Maximum timeout reached"))
      (if (and city region)
          (format "%s, %s" city region)
        (message "[krisb-get-location-name] Failed to fetch geolocation data")
        nil)))
  :config
  ;; Solar
  (require 'solar)
  ;; FIXME 2024-10-23: Hangs on startup sometimes for some reason...
  ;; (krisb-evaluate-when-internet
  ;;   20
  ;;   (let ((coords (krisb-get-lat-lon)))
  ;;     (setopt calendar-latitude (car coords)
  ;;             calendar-longitude (cdr coords)))
  ;;   (setopt calendar-location-name (krisb-get-location-name)))
  )

;;; Org-agenda
(use-package org-agenda
  :ensure nil
  :hook (org-agenda-mode . hl-line-mode)
  :bind ( :map krisb-open-keymap
          ("a" . org-agenda)
          :map org-agenda-mode-map
          ("`" . krisb-org-agenda-process))
  :custom
  ;; Effort
  (org-agenda-sort-noeffort-is-high t)
  (org-effort-durations
   '(("m" . 1)
     ("h" . 60)
     ("d" . 1440)
     ("w" . 10080)
     ("mon" . 43200)
     ("y" . 525960.0)))

  ;; Tags
  (org-use-tag-inheritance t)
  (org-agenda-show-inherited-tags t)
  (org-use-fast-todo-selection 'expert)
  (org-tags-exclude-from-inheritance '("project" "inbox"))
  (org-use-property-inheritance '("CATEGORY" "ARCHIVE"))
  (org-agenda-show-inherited-tags t)
  (org-use-fast-todo-selection 'expert)
  (org-tag-faces
   '(("project" . outline-1)))

  ;; Dependencies
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies nil)
  (org-agenda-dim-blocked-tasks t)

  ;; Org agenda
  (org-agenda-file-regexp "\\`[^.].*\\.org\\'")
  (org-agenda-sticky t) ; Set to nil if frequently modifying `org-agenda-custom-commands'
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-tags-column 0)
  (org-agenda-start-on-weekday 1)
  (org-agenda-format-date #'krisb-org-agenda-format-date-aligned)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-scheduled nil)
  (org-agenda-remove-times-when-in-prefix t)
  (org-agenda-remove-tags 'prefix)
  (org-agenda-prefix-format
   '((agenda  . " %i %-8:c%?-12t% s%-5e%(krisb-org-agenda-breadcrumb 20)")
     (todo  . " %i %-8:c%-5e%(krisb-org-agenda-breadcrumb 20)")
     (tags  . " %i %-8:c%-5e%(krisb-org-agenda-breadcrumb 20)")
     (search . " %i %-8:c%-5e%(krisb-org-agenda-breadcrumb 20)")))
  ;; See `krisb-org-sort-agenda-by-created-time' for my user-defined sorter
  (org-agenda-sorting-strategy
   '((agenda habit-down urgency-down priority-down user-defined-up deadline-up todo-state-up category-up)
     (todo urgency-down priority-down user-defined-up todo-state-up category-up)
     (todo urgency-down priority-down user-defined-up todo-state-up category-up)
     (search todo-state-up priority-down category-keep)))
  ;; See
  ;; https://emacs.stackexchange.com/questions/17302/is-there-a-way-to-make-org-mode-count-repetitive-tasks-done-certain-hours-past-m?rq=1
  (org-extend-today-until 3)
  (org-use-effective-time t)
  (org-agenda-block-separator ?─)
  (org-deadline-warning-days 3)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  (org-agenda-breadcrumbs-separator " ⇛ ")
  (org-agenda-skip-scheduled-delay-if-deadline nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-auto-exclude-function #'krisb-org-agenda-auto-exclude-function)
  (org-agenda-compact-blocks nil)

  ;; Capture templates
  ;; See also `org-capture-templates-contexts'
  (org-capture-templates
   `(("t" "Todo" entry
      (file ,(expand-file-name "todo.org" krisb-org-agenda-directory))
      "* TODO %? :inbox:%^g\n"
      :empty-lines 1)
     ("T" "Todo (without processing)" entry
      (file ,(expand-file-name "todo.org" krisb-org-agenda-directory))
      "* TODO %? %^g\n"
      :empty-lines 1)
     ("i" "Idea" entry
      (file+olp+datetree ,(car (denote-directory-files "20221011T101254")))
      "* %?\n"
      :tree-type month
      :empty-lines 1
      :clock-in t
      :clock-resume t
      :kill-buffer t)
     ("I" "Idea with context" entry
      (file+olp+datetree ,(car (denote-directory-files "20221011T101254")))
      "* %?\n\n+ %^{Context string}: %a"
      :tree-type month
      :empty-lines 1
      :clock-in t
      :clock-resume t
      :kill-buffer t)
     ("j" "Journal" entry
      (file+olp+datetree ,(lambda ()
                            (org-roam-node-file
                             (org-roam-node-from-id "20241006T214800.000000"))))
      "* %<%c>\n\n%?"
      :tree-type month
      :jump-to-captured t
      :immediate-finish t
      :empty-lines 1
      :clock-in t
      :clock-resume t)))
  (org-capture-use-agenda-date t)       ; Use the time-at-point if any

  ;; Todos
  (org-fast-tag-selection-single-key 'expert)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "HOLD(h@/!)" "MAYBE(m)" "|" "DONE(d!/@)" "CANCELED(c@/!)")))
  (org-todo-keyword-faces
   '(("NEXT" . (bold success))
     ("TODO" . org-todo)
     ("HOLD" . (shadow error))
     ("MAYBE" . (shadow org-todo))
     ("DONE" . (bold org-done))
     ("CANCELED" . error)))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-highest-priority ?A)
  (org-default-priority ?E)
  (org-lowest-priority ?F)
  (org-priority-faces
   '((?A . (bold org-priority))
     (?B . (bold org-priority))
     (?C . org-priority)
     (?D . org-priority)
     (?E . (shadow org-priority))
     (?F . (shadow org-priority))))
  ;; FIXME 2024-10-02: Haven't found a way to get this to mesh well with my
  ;; workflow
  (org-stuck-projects
   `("+project/-DONE-CANCELED"
     ("NEXT" "TODO")
     nil
     nil
     ,(rx (regexp org-not-done-heading-regexp))))

  ;; Input
  (org-read-date-prefer-future 'time)
  :custom-face
  (org-drawer ((t (:height 0.9))))
  (org-mode-line-clock ((t (:inherit org-agenda-date))))
  :init
  (defun krisb-org-agenda-breadcrumb (len)
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
  ;; Populate `org-agenda-files'
  (dolist (f (directory-files-recursively krisb-org-agenda-directory
                                          (rx (or (literal ".org") (literal ".org_archive")) eol)
                                          nil
                                          (lambda (subdir)
                                            "Ignore \".st\" subdirectories.
These are directories created by SyncThing which may have org files I do not want to include."
                                            (not (string-match (rx (literal ".st")) subdir)))))
    (add-to-list 'org-agenda-files f))

  ;; Taken from
  ;; https://github.com/psamim/dotfiles/blob/master/doom/config.el#L213
  (defun krisb-org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the agenda or timeline.
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

  (defun krisb-org-agenda-auto-exclude-function (tag)
    "Set tags based on time of day.
See ((org) Filtering/limiting agenda items)."
    (when (cond ((member tag '("@home" "@hobbies"))
                 (let ((hr (nth 2 (decode-time))))
                   ;; After 10 or before 21
                   (or (> hr 10) (< hr 21)))))
      (concat "-" tag)))

  (defun krisb-org-agenda-process ()
    "(Bespoke) process org-agenda entry at point."
    (interactive)
    (org-agenda-priority)
    (org-agenda-todo)
    (org-agenda-set-tags)
    (org-agenda-set-effort)
    (org-review-insert-last-review)
    (org-review-insert-next-review)))

;;; Org-super-agenda
(use-package org-super-agenda
  :vc ( :url "https://github.com/Alexander-Miller/org-super-agenda.git"
        :rev :newest)
  :custom
  (org-super-agenda-hide-empty-groups t)
  ;; FIXME 2024-10-06: When trying to set `org-super-agenda-keep-order' to
  ;; non-nil, it causes an error when using :auto-* selectors. This doesn't seem
  ;; to occur in an emacs -Q instances, but I have no clue what is causing the
  ;; error in my config... Although the following PR might fix the issue:
  ;; https://github.com/alphapapa/org-super-agenda/pull/242. Also see the NOTE
  ;; above, near the :vc keword
  (org-super-agenda-keep-order t)
  (org-agenda-cmp-user-defined #'krisb-org-sort-agenda-by-created-time)
  :config
  (org-super-agenda-mode 1)

  (defun krisb-org-get-created-time (entry)
    "Return the CREATED time of ENTRY, or an empty string if it doesn't exist."
    (let ((marker (get-text-property 0 'marker entry)))
      (if marker
          (org-entry-get marker "CREATED")
        "")))

  (defun krisb-org-sort-agenda-by-created-time (a b)
    "Compare two agenda items, A and B, by their CREATED property."
    (let* ((time-a (krisb-org-get-created-time a))
           (time-b (krisb-org-get-created-time b)))
      (cond
       ((string= time-a "") +1)         ; A has no CREATED property, put it last
       ((string= time-b "") -1)         ; B has no CREATED property, put it last
       (t
        (if (time-less-p (date-to-time time-a) (date-to-time time-b))
            -1 +1))))))

;;; `org-agenda-custom-commands'
(with-eval-after-load 'org-agenda
  ;; Relevant variables to set locally in `org-agenda-custom-commands'
  ;; - `org-agenda-overriding-header'
  ;; - `org-agenda-show-inherited-tags'
  ;; - `org-agenda-sorting-strategy'
  ;; - `org-agenda-start-day'
  ;; - `org-agenda-span'
  ;; - `org-agenda-prefix-format'
  ;; - `org-agenda-scheduled-leaders'
  ;; - `org-agenda-deadline-leaders'
  ;; - `org-agenda-skip-deadline-prewarning-if-scheduled'
  ;; - `org-agenda-skip-scheduled-if-deadline-is-shown'
  ;; - `org-habit-show-all-today'
  ;; - `org-habit-show-habits-only-for-today'
  ;; - `org-agenda-dim-blocked-tasks'
  ;; - `org-agenda-include-diary'
  ;; - `org-agenda-insert-diary-extract-time'
  ;; - `org-agenda-skip-function'
  ;; - `org-agenda-entry-types'
  ;; - `org-deadline-warning-days'
  ;; - `org-scheduled-delay-days'
  (setopt org-agenda-custom-commands
          '(("f" "FYP"
             ((agenda ""
                      ((org-agenda-overriding-header "Time-bound tasks")
                       (org-agenda-show-inherited-tags t)
                       (org-agenda-start-day "+0d")
                       (org-agenda-span 'day)
                       (org-habit-show-habits-only-for-today t)
                       (org-agenda-dim-blocked-tasks t)
                       (org-agenda-include-diary t)
                       (org-agenda-insert-diary-extract-time t)
                       (org-super-agenda-groups
                        '((:name "Overdue" :scheduled past :deadline past)
                          (:auto-category t)))))
              (tags-todo "+TODO=\"NEXT\"-project-inbox|+TODO=\"TODO\"-project-inbox"
                         ((org-agenda-overriding-header "Non-time-bound tasks")
                          (org-agenda-use-tag-inheritance '(todo))
                          (org-agenda-show-inherited-tags t)
                          (org-agenda-dim-blocked-tasks 'invisible)
                          (org-agenda-skip-function
                           '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
            ("i" "Inbox: process entries"
             ((tags-todo "+inbox|+TODO=\"MAYBE\""
                         ((org-agenda-overriding-header "Review")
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-skip-function 'org-review-agenda-skip)))
              (tags-todo "+inbox"
                         ((org-agenda-overriding-header "Non-review inbox")
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-skip-function
                           'krisb-org-review-has-review-property-p)))
              (todo "MAYBE"
                    ((org-agenda-overriding-header "Non-review maybes")
                     (org-agenda-skip-function
                      'krisb-org-review-has-review-property-p)))))
            ("p" "Projects"
             ((tags-todo "project"
                         ((org-agenda-overriding-header "")
                          ;; This lets project sub-tasks be discoverable by a tags
                          ;; search. One might think :auto-parent makes this
                          ;; redundant, but this handles cases where I have a
                          ;; sub-task but its parent is not a project -- I do this
                          ;; sometimes for simple dependencies between todos
                          ;; FIXME 2024-10-07: This shows the project tag for all the
                          ;; sub-tasks, which can be visually noisy. I'm not sure if
                          ;; there is a workaround
                          (org-tags-exclude-from-inheritance
                           (remove "project" org-tags-exclude-from-inheritance))
                          (org-agenda-prefix-format
                           ;; FIXME 2024-10-07: Not sure if this is a tags- or
                           ;; todo-type view
                           '((tags  . " %i %-8:c%-5e%?-12t% s")))
                          (org-super-agenda-groups
                           '(( :auto-parent t
                               :order 2)
                             ( :name "All projects"
                               :anything t
                               :order 1))))))))))

;;; Org-clock
(use-package org-clock
  :ensure nil
  :custom
  (org-clock-persist t)
  (org-clock-out-when-done t)
  (org-clock-history-length 10)
  (org-clock-in-resume t)
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
  (org-clock-heading-function 'krisb-org-clock-get-heading-string)
  :config
  (org-clock-persistence-insinuate)

  ;; Mode line string
  (defun krisb-org-clock-get-heading-string ()
    "Get truncated org heading string.

Same as default but truncates with `truncate-string-ellipsis'."
    (let ((heading (org-link-display-format
                    (org-no-properties (org-get-heading t t t t)))))
      (truncate-string-to-width heading 40 nil nil (truncate-string-ellipsis)))))

;;; Org-habit
(use-package org-habit
  :after org-agenda
  :ensure nil
  :custom
  (org-habit-show-habits t)
  (org-habit-following-days 1)
  (org-habit-preceding-days 14)
  (org-habit-show-done-always-green t)
  (org-habit-show-habits-only-for-today t)
  (org-habit-graph-column 110)
  (org-habit-today-glyph ?◌)
  (org-habit-completed-glyph ?●)
  (org-habit-missed-glyph ?○))

;;; Org-expiry
(use-package org-expiry
  :requires org-contrib
  :ensure nil
  :hook (org-capture-before-finalize . org-expiry-insert-created)
  :custom
  (org-expiry-inactive-timestamps t))

;;; Org-depend
;; Add blocking and triggering actions when an org-todo state is changed.
(use-package org-depend
  :requires org-contrib
  :ensure nil
  :after org-agenda)

;;; Org-edna
;; Also look at `org-edna' with `org-linker-edna'
;; (https://github.com/toshism/org-linker-edna) (the second of which requires
;; `org-linker': https://github.com/toshism/org-linker). `org-super-links' can
;; be added to see which tasks are being blocked by the current task. See
;; https://karl-voit.at/2021/01/23/org-linker-edna/ for sample workflow
(use-package org-edna
  :diminish
  :demand t
  :after org-agenda
  :bind ( :map org-mode-map
          ("C-c d" . krisb-consult-org-depend)
          :map org-agenda-mode-map
          ("C-c d". krisb-consult-org-agenda-depend))
  :config
  (org-edna-mode 1)

  (with-eval-after-load 'consult
    (defun krisb-consult-org-depend--add-id (new-id)
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

    (defun krisb-consult-org-depend (&optional match)
      "Create a dependency for the `org-todo’ at point.
  A dependency is defined by `org-depend’s `BLOCKER’ property. IDs
  are created in the todo dependency with `org-id-get-create’.
  MATCH is a query sent to `org-map-entries’."
      (interactive)
      (save-window-excursion
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
            (krisb-consult-org-depend--add-id new-id)
            (message "‘%s’ added as a dependency to this todo"
                     (substring-no-properties dependency))))))

    (defun krisb-consult-org-agenda-depend (&optional match)
      "Create a dependency for the `org-agenda’ item at point.
  See `krisb-consult-org-depend’."
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
                (setq dependency (funcall 'krisb-consult-org-depend match))))))))

    (consult-customize krisb-consult-org-depend
                       :prompt "Select dependency for the heading at point: "
                       krisb-consult-org-agenda-depend
                       :prompt "Select dependency for this agenda item: ")))

;;; Org-review
(use-package org-review
  :bind ( :map org-mode-map
          ("C-c r s" . org-review-insert-next-review)
          ("C-c r l" . org-review-insert-last-review)
          ("C-c r u" . krisb-org-review-unreview)
          :map org-agenda-mode-map
          ("C-c r s" . org-review-insert-next-review)
          ("C-c r l" . org-review-insert-last-review)
          ("C-c r u" . krisb-org-review-unreview))
  :custom
  (org-review-delay "+8d")
  (org-review-last-timestamp-format 'inactive)
  (org-review-next-timestamp-format 'inactive)
  (org-review-sets-next-date nil)
  :config
  ;; Agenda helpers
  (defun krisb-org-review-has-review-property-p ()
    "Skip the current todo if it has an org-review property.
Returns non-nil if the current todo has a property by the name of the
value of `org-review-next-property-name' or
`org-review-last-property-name'."
    (and (or (org-entry-get (point) org-review-next-property-name)
             (org-entry-get (point) org-review-last-property-name))
         (org-with-wide-buffer (or (outline-next-heading) (point-max)))))

  ;; Commands
  (defun krisb-org-review-unreview ()
    "Un-review the current heading.
Removes the properties denoted by `org-review-next-property-name' and
`org-review-last-property-name'."
    (interactive)
    (when (org-entry-get (point) org-review-next-property-name)
      (org-delete-property org-review-next-property-name))
    (when (org-entry-get (point) org-review-last-property-name)
      (org-delete-property org-review-last-property-name)))

  (defun krisb-org-review-scheduled-to-review ()
    "Turn the scheduled date of an agenda entry to a review date.
Sets the value of `org-review-next-property-name' to the scheduled
date.  Deletes the scheduled date afterward.

This command was initially used to help me transition from a
non-org-review workflow (a combination of an INBOX tag and scheduling)
to an org-review workflow."
    (interactive)
    (org-agenda-with-point-at-orig-entry nil
      (let ((date (org-entry-get (point) "SCHEDULED")))
        (if date
            (progn
              (org-set-property org-review-next-property-name
                                (concat "["
                                        (substring date 1 -1)
                                        "]"))
              (org-schedule '(4))
              (message "No scheduled date found for this item.")))))))

;;; Provide
(provide 'krisb-org-agenda)
