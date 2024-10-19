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

;;;; Org-super-agenda
(use-package org-super-agenda
  ;; NOTE 2024-10-06: I can currently check out and install a version of
  ;; org-super-agenda that applies the patch from PR#242
  :vc (:url "https://github.com/Alexander-Miller/org-super-agenda.git"
            :rev "f524347474d7535aab7ae3e6651cf2dd1fb68c72")
  :demand
  :after org-agenda
  :custom
  (org-super-agenda-hide-empty-groups t)
  ;; FIXME 2024-10-06: When trying to set `org-super-agenda-keep-order' to
  ;; non-nil, it causes an error when using :auto-* selectors. This doesn't seem
  ;; to occur in an emacs -Q instances, but I have no clue what is causing the
  ;; error in my config... Although the following PR might fix the issue:
  ;; https://github.com/alphapapa/org-super-agenda/pull/242. Also see the NOTE
  ;; above, near the :vc keword
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
                        '((:discard (:and (:tag "inbox" :not (:deadline t)))) ; We want to see deadlines even if they have the inbox tag
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
