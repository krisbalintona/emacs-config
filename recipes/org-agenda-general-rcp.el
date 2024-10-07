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
  :bind
  ( :map kb/open-keys
    ("a" . org-agenda)
    :map org-agenda-mode-map
    ("`" . kb/org-agenda-process))
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
  (org-agenda-tags-column 'auto)
  (org-agenda-start-on-weekday 1)
  (org-agenda-format-date 'kb/org-agenda-format-date-aligned)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-scheduled nil)
  (org-agenda-remove-times-when-in-prefix t)
  (org-agenda-remove-tags 'prefix)
  (org-agenda-prefix-format
   '((agenda  . " %i %-8:c%?-12t% s %(kb/org-agenda-breadcrumb 20)")
     (todo  . " %i %-8:c %(kb/org-agenda-breadcrumb 20)")
     (tags  . " %i %-8:c %(kb/org-agenda-breadcrumb 20)")
     (search . " %i %-8:c %(kb/org-agenda-breadcrumb 20)")))
  (org-agenda-sorting-strategy
   '((agenda habit-down user-defined-up urgency-down deadline-up todo-state-up category-up)
     (todo user-defined-up urgency-down todo-state-up category-up)
     (todo user-defined-up urgency-down todo-state-up category-up)
     (search todo-state-up priority-down category-keep)))
  ;; See
  ;; https://emacs.stackexchange.com/questions/17302/is-there-a-way-to-make-org-mode-count-repetitive-tasks-done-certain-hours-past-m?rq=1
  (org-extend-today-until 3)
  (org-use-effective-time t)
  (org-agenda-block-separator ?—)
  (org-deadline-warning-days 3)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-breadcrumbs-separator " ⇛ ")
  (org-agenda-skip-scheduled-delay-if-deadline nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-auto-exclude-function #'kb/org-agenda-auto-exclude)
  (org-agenda-compact-blocks nil)

  ;; Capture templates
  ;; See also `org-capture-templates-contexts'
  (org-capture-templates
   `(("t" "Todo (without processing)" entry
      (file ,(expand-file-name "todo.org" kb/agenda-dir))
      "* TODO %? :inbox:%^g\n"
      :empty-lines 1)
     ("T" "Todo" entry
      (file ,(expand-file-name "todo.org" kb/agenda-dir))
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
      "* %<%F>\n\n%?"
      :tree-type week
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
  (org-mode-line-clock ((t (:inherit org-agenda-date))))
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

  ;; TODO 2024-10-02: Perhaps turn this into a transient menu which lets me
  ;; choose on-the-fly among a list of pre-defined options (similar to
  ;; `notmuch-tag-jump')
  (defun kb/org-agenda-auto-exclude (tag)
    "Set tags based on time. See (org) Filtering/limiting agenda items"
    (when (cond ((member tag '("@home" "@hobbies"))
                 (let ((hr (nth 2 (decode-time))))
                   ;; After 10 or before 21
                   (or (> hr 10) (< hr 21)))))
      (concat "-" tag)))

  (defun kb/org-agenda-process ()
    "(Bespoke) process org-agenda entry at point."
    (interactive)
    (org-agenda-priority)
    (org-agenda-set-tags)
    (org-agenda-next-item 1)))

;;;; Org-super-agenda
(use-package org-super-agenda
  :demand
  :after org-agenda
  :custom
  (org-super-agenda-hide-empty-groups t)
  ;; FIXME 2024-10-06: When trying to set `org-super-agenda-keep-order' to
  ;; non-nil, it causes an error when using :auto-* selectors. This doesn't seem
  ;; to occur in an emacs -Q instances, but I have no clue what is causing the
  ;; error in my config... Although the following PR might fix the issue:
  ;; https://github.com/alphapapa/org-super-agenda/pull/242
  ;; NOTE 2024-10-06: I have currently checked out and installed a version of
  ;; org-super-agenda that applies the patch from PR#242
  ;; (/home/krisbalintona/emacs-repos/packages/org-super-agenda-PR#242/)
  (org-super-agenda-keep-order t)
  (org-agenda-cmp-user-defined #'kb/org-sort-agenda-by-created-time)
  :init
  (defun kb/org-get-created-time (entry)
    "Return the CREATED time of ENTRY, or an empty string if it doesn't exist."
    (let ((marker (get-text-property 0 'marker entry)))
      (if marker
          (org-entry-get marker "CREATED")
        "")))

  (defun kb/org-sort-agenda-by-created-time (a b)
    "Compare two agenda items, A and B, by their CREATED property."
    (let* ((time-a (kb/org-get-created-time a))
           (time-b (kb/org-get-created-time b)))
      (cond
       ((string= time-a "") +1)         ; A has no CREATED property, put it last
       ((string= time-b "") -1)         ; B has no CREATED property, put it last
       (t
        (if (time-less-p (date-to-time time-a) (date-to-time time-b))
            -1 +1)))))
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
                        '((:discard (:tag "inbox"))
                          (:auto-category t)))))
              (tags-todo "+TODO=\"NEXT\"-project-inbox"
                         ((org-agenda-overriding-header "Next")
                          (org-agenda-use-tag-inheritance '(todo))
                          (org-agenda-show-inherited-tags t)
                          (org-agenda-dim-blocked-tasks 'invisible)
                          (org-agenda-skip-function
                           '(org-agenda-skip-entry-if 'scheduled 'deadline))))
              (tags-todo "+TODO=\"TODO\"-project-inbox"
                         ((org-agenda-overriding-header "Standard")
                          (org-agenda-use-tag-inheritance '(todo))
                          (org-agenda-show-inherited-tags t)
                          (org-agenda-dim-blocked-tasks 'invisible)
                          (org-agenda-skip-function
                           '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
            ("i" "Inbox: process entries"
             ((agenda ""
                      ((org-agenda-overriding-header "Time-bound inbox")
                       (org-agenda-start-day "+0d")
                       (org-agenda-span 5)
                       (org-habit-show-habits nil)
                       (org-agenda-entry-types
                        '(:deadline :scheduled))
                       (org-super-agenda-groups
                        '((:tag "inbox")
                          (:todo "MAYBE")
                          (:discard (:anything t))))))
              (tags-todo "+inbox"
                         ((org-agenda-overriding-header "Regular inbox")
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-skip-function
                           '(org-agenda-skip-entry-if 'scheduled 'deadline))))
              (todo "MAYBE"
                    ((org-agenda-overriding-header "Regular maybes")
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
            ("p" "Projects"
             ((tags-todo "project"
                         ((org-agenda-overriding-header "")
                          (org-agenda-dim-blocked-tasks nil)
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
                           '((tags  . " %i %-8:c")))
                          (org-super-agenda-groups
                           '(( :auto-parent t
                               :order 2)
                             ( :name "All projects"
                               :anything t
                               :order 1))))))))))
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
