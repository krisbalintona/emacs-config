;;; org-agenda-general-rcp.el --- Todos with org-agenda  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Settings related to org-agenda itself.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'org-general-rcp)

;;;; Org-agenda
(use-package org-agenda
  :ensure nil
  ;; Call after `org' since some of the options below are from `org', not
  ;; `org-export', so they will be overwritten if this use-package loads before
  ;; `org' does
  :after org
  :hook ((org-agenda-finalize . (lambda () (goto-char (point-min))))
         (org-after-todo-state-change . kb/org-todo-project-prog))
  :bind
  ( :map kb/open-keys
    ("a" . org-agenda)
    :map org-agenda-mode-map
    ("C-c ." . kb/org-set-agenda-show-property))
  :custom
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
  (org-agenda-show-inherited-tags t)
  (org-use-fast-todo-selection 'expert)
  (org-tags-exclude-from-inheritance '("project"))
  (org-use-property-inheritance '("CATEGORY" "ARCHIVE"))

  ;; Dependencies
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies nil)
  (org-agenda-dim-blocked-tasks t)

  ;; Org agenda
  (org-agenda-file-regexp "\\`[^.].*\\.org\\'")
  (org-agenda-sticky t) ; Set to nil if frequently modifying `org-agenda-custom-commands'
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
  (org-agenda-compact-blocks t)

  ;; Capture templates
  ;; See also `org-capture-templates-contexts'
  (org-capture-templates
   `(("t" "Todo" entry
      (file ,(expand-file-name "todo.org" kb/agenda-dir))
      ;; "* TODO %? %^g\n"
      "* TODO %? %^{CATEGORY}p%^g\n"
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
      :kill-buffer t)))

  ;; Todos
  (org-fast-tag-selection-single-key 'expert)
  (org-todo-keywords
   '((sequence "NEXT(n)" "TODO(t)" "WAITING(w@/!)" "MAYBE(m)" "|" "DONE(d!/@)" "CANCELED(c@/!)")))
  (org-todo-repeat-to-state "TODO")
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
  (dolist (f (directory-files-recursively kb/agenda-dir (rx (literal ".org") eol)))
    (add-to-list 'org-agenda-files f))

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
    (when (cond ((member tag '("@home" "@hobbies"))
                 (let ((hr (nth 2 (decode-time))))
                   ;; After 10 or before 21
                   (or (> hr 10) (< hr 21)))))
      (concat "-" tag)))


  ;; Define bespoke means of adding active timestamps to headings so that they
  ;; show in agenda buffers
  (defun kb/org-set-agenda-show-property ()
    "Set the \"AGENDA_SHOW\" property for the current heading.
Also works in agenda buffers. Definition modeled after
`org-agenda-set-property'."
    (interactive)
    (if (not (eq major-mode 'org-agenda-mode))
        (org-set-property "AGENDA_SHOW" (concat "<" (org-read-date) ">"))
      (org-agenda-check-no-diary)
      (org-agenda-maybe-loop
       #'org-agenda-set-property nil nil nil
       (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                            (org-agenda-error)))
              (buffer (marker-buffer hdmarker))
              (pos (marker-position hdmarker))
              (inhibit-read-only t)
              ) ;; newhead
         (org-with-remote-undo buffer
           (with-current-buffer buffer
             (widen)
             (goto-char pos)
             (org-fold-show-context 'agenda)
             (org-set-property "AGENDA_SHOW" (concat "<" (org-read-date) ">")))))))))

;;;; Org-super-agenda
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
  (org-super-agenda-mode 1)
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
  (setopt org-agenda-custom-commands
          '(("f" "FYP"
             ((agenda ""
                      ((org-agenda-overriding-header "Time-bound tasks")
                       (org-agenda-show-inherited-tags t)
                       (org-agenda-sorting-strategy
                        ;; NOTE 2024-10-01: Testing out `urgency-down' instead
                        ;; of `priority-down'
                        '((agenda habit-down time-up urgency-down deadline-up todo-state-up category-up)))
                       (org-agenda-start-day "+0d")
                       (org-agenda-span 3)
                       (org-agenda-skip-deadline-prewarning-if-scheduled nil)
                       (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
                       (org-habit-show-all-today nil)
                       (org-habit-show-habits-only-for-today t)
                       (org-agenda-dim-blocked-tasks t)
                       (org-agenda-include-diary t)
                       (org-agenda-insert-diary-extract-time t)))
              (alltodo ""
                       ((org-agenda-overriding-header "Non-timed Todos")
                        (org-agenda-show-inherited-tags t)
                        (org-agenda-dim-blocked-tasks 'invisible)
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'timestamp))
                        (org-agenda-sorting-strategy
                         '((todo todo-state-up priority-down category-up)))))))
            ("n" "Now"
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
                        '((agenda . "  %-7e%-25(kb/org-agenda-breadcrumb 21)%-5s%?-12t")))
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
                        '((agenda . "  %-7e%-25(kb/org-agenda-breadcrumb 21)%-5s%?-12t")))
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

;;;; Org-clock
(use-package org-clock
  :demand
  :ensure nil
  :after org-agenda
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

;;;; Org-habit
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

(provide 'org-agenda-general-rcp)
;;; org-agenda-general-rcp.el ends here
