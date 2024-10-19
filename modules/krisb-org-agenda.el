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
  (krisb-evaluate-when-internet
    20
    (let ((coords (krisb-get-lat-lon)))
      (setopt calendar-latitude (car coords)
              calendar-longitude (cdr coords)))
    (setopt calendar-location-name (krisb-get-location-name))))

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
      (file+olp+datetree ,(car (denote-directory-files "20241006T214811")))
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
                                          (rx (or (literal ".org") (literal ".org_archive")) eol)))
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
    (org-agenda-set-tags)
    (org-agenda-next-item 1)))

;;; Provide
(provide 'krisb-org-agenda)
