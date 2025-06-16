;; -*- lexical-binding: t; -*-

;;; Calendar
(use-package calendar
  :ensure nil
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
  ;; Ensure diary file exists
  (unless (file-exists-p diary-file)
    (make-empty-file diary-file t))

  ;; Solar
  (require 'solar)
  ;; FIXME 2024-10-23: Hangs on startup sometimes for some reason...
  ;; 2025-04-22: A potential solution might be to change
  ;; `krisb-evaluate-when-internet' to use `while-no-input'.
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
  :custom
  (org-agenda-prefix-format
   ;; See https://whhone.com/posts/org-agenda-repeated-tasks/ for an explanation
   ;; of `krisb-org-agenda-repeater' usage here
   '((agenda  . " %i %-8:c%?-12t% s%(krisb-org-agenda-repeater)%-5e%(krisb-org-agenda-breadcrumb 20)")
     (todo  . " %i %-8:c%-5e%(krisb-org-agenda-breadcrumb 20)")
     (tags  . " %i %-8:c%-5e%(krisb-org-agenda-breadcrumb 20)")
     (search . " %i %-8:c%-5e%(krisb-org-agenda-breadcrumb 20)")))
  ;; See `krisb-org-sort-agenda-by-created-time' for my user-defined sorter
  (org-agenda-sorting-strategy
   '((agenda habit-down urgency-down priority-down user-defined-up deadline-up todo-state-up category-up)
     (todo urgency-down priority-down user-defined-up todo-state-up category-up)
     (todo urgency-down priority-down user-defined-up todo-state-up category-up)
     (search todo-state-up priority-down category-keep)))
  (org-agenda-block-separator ?─)
  (org-deadline-warning-days 3)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  (org-agenda-breadcrumbs-separator " ⇛ ")
  ;; Shorten the leaders to reserve spaces for the repeater.  Taken from
  ;; https://whhone.com/posts/org-agenda-repeated-tasks/
  (org-agenda-scheduled-leaders '("Sched" "S.%2dx"))
  (org-agenda-deadline-leaders '("Deadl" "In%2dd" "D.%2dx"))
  (org-agenda-skip-scheduled-delay-if-deadline nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-auto-exclude-function #'krisb-org-agenda-auto-exclude-function)
  (org-agenda-compact-blocks nil)

  ;; Todos
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
  (defun krisb-org-agenda-repeater ()
    "The repeater shown in org-agenda-prefix for agenda.

Taken from https://whhone.com/posts/org-agenda-repeated-tasks/."
    (if (org-before-first-heading-p)
        "┄┄┄┄┄┄┄"                       ; Fill the time grid
      (format "%5s: " (or (org-get-repeat) ""))))
  :config
  ;; REVIEW 2024-11-11: Not sure if this is needed if we set the value of
  ;; `org-durations-units' via :custom.
  ;; (org-duration-set-regexps)

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
      (concat "-" tag))))

;;; Org-clock
(use-package org-clock
  :ensure nil
  :custom
  (org-clock-persist t)
  (org-clock-history-length 10)
  (org-clock-persist-query-resume t)
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

  ;; Bespoke mode line string
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
          ("C-c d" . krisb-consult-org-edna-block)
          :map org-agenda-mode-map
          ("C-c d". krisb-consult-org-agenda-edna-block))
  :config
  (org-edna-mode 1)

  ;; Bespoke `consult' integration.  This functionality depends on org-enda's
  ;; :BLOCKER: (which supports lists of IDs, rather than a single ID, like
  ;; `org-depend').
  (defun krisb-consult-org-edna--add-id (new-id)
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

  (defun krisb-consult-org-edna-block (&optional match)
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
            (require 'consult)
            (consult-org-agenda (or match "/-DONE-CANCELED"))
            (setq dependency (org-get-heading))
            (when (equal current-heading dependency)
              (error "Cannot depend on the same `org-todo’!"))
            (setq new-id (org-id-get-create)))
          ;; Modify the BLOCKER property of the current todo
          (krisb-consult-org-edna--add-id new-id)
          (message "‘%s’ added as a dependency to this todo"
                   (substring-no-properties dependency))))))

  (defun krisb-consult-org-agenda-edna-block (&optional match)
    "Create a dependency for the `org-agenda’ item at point.
  See `krisb-consult-org-edna-block’."
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
              (setq dependency (funcall 'krisb-consult-org-edna-block match))))))))

  (with-eval-after-load 'consult
    (consult-customize krisb-consult-org-edna-block
                       :prompt "Select dependency for the heading at point: "
                       krisb-consult-org-agenda-edna-block
                       :prompt "Select dependency for this agenda item: ")))

;;; Provide
(provide 'krisb-org-agenda)
