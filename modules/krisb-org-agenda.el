;; -*- lexical-binding: t; -*-

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
  :hook (org-agenda-mode . hl-line-mode)
  :bind ( :map krisb-open-keymap
          ("a" . org-agenda))
  :custom
  (org-agenda-files (krisb-org-agenda-directory-files))
  (org-agenda-inhibit-startup t)

  ;; Effort
  (org-agenda-sort-noeffort-is-high nil)
  (org-duration-units
   `(("m" . 1)
     ("h" . 60)
     ("d" . ,(* 60 8))
     ("w" . ,(* 60 8 5))
     ("mon" . ,(* 60 8 5 4))
     ("y" . ,(* 60 8 5 4 10))))

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

  ;; Capture templates
  ;; See also `org-capture-templates-contexts'
  (org-capture-templates
   `(("t" "Todo" entry
      (file ,krisb-org-agenda-main-file)
      "* TODO %? :inbox:%^g\n"
      :empty-lines 1)
     ("T" "Todo (without processing)" entry
      (file ,krisb-org-agenda-main-file)
      "* TODO %? %^g\n"
      :empty-lines 1)
     ("j" "Journal" entry
      (function (lambda ()
                  (let* ((candidate (krisb-org-capture--org-node-select-by-tags '("^__journal$")))
                         (node (gethash candidate org-node--candidate<>node)))
                    (krisb-org-capture--org-node-insert-datetree node))))
      "* %<%c>\n"
      :tree-type (year quarter month)
      :jump-to-captured t
      :immediate-finish t
      :empty-lines 1
      :clock-in t
      :clock-resume t)
     ("w" "Just write" entry
      (function ,(lambda () (krisb-org-capture--org-node-insert-datetree (org-node-by-id "20241006T214800.000000"))))
      "* %<%c>\n\n*P:* %(car (krisb-oblique-strategies--random))\n\n"
      :tree-type (year quarter month)
      :jump-to-captured t
      :immediate-finish t
      :empty-lines 1
      :clock-in t
      :clock-resume t)
     ("l" "Log" item
      (function (lambda ()
                  (let* ((candidate (krisb-org-capture--org-node-select-by-tags '("^__log$")))
                         (node (gethash candidate org-node--candidate<>node)))
                    (krisb-org-capture--org-node-insert-datetree node))))
      "%U %?"
      :tree-type (quarter week)
      :clock-in t
      :clock-resume t)
     ("m" "Work meeting notes" entry
      (function ,(lambda () (krisb-org-capture--org-node-insert-datetree (org-node-by-id "20241114T091749.707997"))))
      "* (%<%c>)%?\n\n"
      :tree-type (year quarter month)
      :jump-to-captured t
      :immediate-finish t)
     ("r" "Reference" entry
      (function (lambda () (krisb-org-capture--org-node-insert-datetree (org-node-by-id "20250422T171216.767702"))))
      "* %?\n"
      :tree-type (year month)
      :jump-to-captured t
      :immediate-finish t
      :empty-lines 1
      :hook org-id-get-create
      :before-finalize (org-node-add-refs
                        (lambda () (org-set-property "ROAM_BOX" "references"))))
     ("b" "Blog post" plain
      (function (lambda ()
                  (let ((org-node-ask-directory krisb-blog-directory))
                    (org-node-capture-target))))
      "#+filetags: :__blog_draft:
#+hugo_bundle:
#+export_file_name: index
#+hugo_tags:
#+hugo_categories:
#+hugo_publishdate:
#+hugo_lastmod:
#+hugo_custom_front_matter: :TableOfContents true
#+hugo_draft: true
#+hugo_paired_shortcodes:\n\n%?"
      :jump-to-captured t
      :immediate-finish t)))
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

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-reschedule 'time)
  (org-log-redeadline 'time)

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

  (defvar krisb-org-capture--temp-olp nil
    "Used as cache for some bespoke org-capture templates.
Should be a list of strings representing the outline path (olp) of an
org heading.")
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
      (concat "-" tag)))

  ;; Bespoke functions for datetrees in org-node nodes
  (defun krisb-org-capture--org-node-insert-datetree (node)
    "Creates datetree at org-node NODE.
Creates a datetree at NODE, and leaves point where a new entry should
be.

This is a helper function for functions used in `org-capture-templates',
but can also be called interactively to prompt for NODE."
    (interactive (list (org-node-read)))
    (require 'org-node)
    (require 'org-datetree)
    (let* ((file (org-node-get-file node))
           (olp (when (< 0 (org-node-get-level node))
                  (org-node-get-olp-with-self node)))
           (pt (org-node-get-pos node))
           (date (calendar-gregorian-from-absolute
                  (time-to-days
                   (org-capture-get :default-time)))) ; Respect C-1 and :time-prompt
           (buffer (org-capture-target-buffer file))
           (tree-type (org-capture-get :tree-type))) ; Respect :tree-type
      ;; See `org-capture-set-target-location' for an explanation of the next
      ;; few lines
      (set-buffer buffer)
      (org-capture-put-target-region-and-position)
      (widen)
      (goto-char pt)
      ;; Create datetree.  See the implementation of
      ;; `org-capture-set-target-location' for an explanation of the lines
      ;; below; it handles all the cases org-capture does
      (funcall
       (pcase tree-type
         (`week #'org-datetree-find-iso-week-create)
         (`month #'org-datetree-find-month-create)
         (`day #'org-datetree-find-date-create)
         ((pred not) #'org-datetree-find-date-create)
         ((pred functionp)
          (lambda (d keep-restriction)
            (org-datetree-find-create-hierarchy
             (funcall tree-type d) keep-restriction)))
         ((pred listp)
          (lambda (d keep-restriction)
            (funcall #'org-datetree-find-create-entry tree-type
                     d keep-restriction)))
         (_ (error "Unrecognized :tree-type")))
       date
       (when olp 'subtree-at-point))))

  (defun krisb-org-capture--org-node-select-by-tags (tags)
    "Interactively prompt for an org-node candidate matching TAGS.
TAGS is a list of regexps that match org-node tags.

This function will use `completing-read' whose candidates are the
org-node nodes that match all of TAGS.  It will return a candidate (see
`org-node--candidate<>node')."
    (completing-read "Select node: "
                     #'org-node-collection
                     (lambda (_title node)
                       (cl-every (lambda (re)
                                   (cl-some (lambda (str)
                                              (string-match-p re str))
                                            (org-node-get-tags node)))
                                 tags))
                     t nil 'org-node-hist)))

;;; Org-super-agenda
(use-package org-super-agenda
  :custom
  (org-super-agenda-hide-empty-groups t)
  (org-super-agenda-keep-order t)
  (org-agenda-cmp-user-defined #'krisb-org-sort-agenda-by-created-time)
  :config
  (org-super-agenda-mode 1)

  ;; Custom user-defined sorting (comparison) function for
  ;; `org-agenda-cmp-user-defined'
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
            -1 +1)))))

  ;; NOTE 2025-03-14: I manually apply the changes described in the un-pulled
  ;; merge request: https://github.com/alphapapa/org-super-agenda/pull/242.  I
  ;; will keep this until the problem it resolves is fixed in master.
  (el-patch-defun org-super-agenda--group-items (all-items)
    "Divide ALL-ITEMS into groups based on `org-super-agenda-groups'."
    (if (bound-and-true-p org-super-agenda-groups)
        ;; Transform groups
        (let ((org-super-agenda-groups (org-super-agenda--transform-groups org-super-agenda-groups)))
          ;; Collect and insert groups
          (cl-loop with section-name
                   for filter in org-super-agenda-groups
                   for custom-section-name = (plist-get filter :name)
                   for order = (or (plist-get filter :order) 0)  ; Lowest number first, 0 by default
                   for (auto-section-name non-matching matching) = (org-super-agenda--group-dispatch all-items filter)

                   do (when org-super-agenda-keep-order
                        (el-patch-swap
                          (setf matching (sort matching #'org-entries-lessp))
                          (setf matching (krisb-org-super-agenda--sort-matches-for-original-order matching))))

                   ;; Transformer
                   for transformer = (plist-get filter :transformer)
                   when transformer
                   do (setq matching (-map (pcase transformer
                                             (`(function ,transformer) transformer)
                                             ((pred symbolp) transformer)
                                             (_ `(lambda (it) ,transformer)))
                                           matching))

                   ;; Face
                   for face = (plist-get filter :face)
                   when face
                   do (let ((append (plist-get face :append)))
                        (when append (cl-remf face :append))
                        (--each matching
                          (add-face-text-property 0 (length it) face append it)))

                   ;; Auto category/group
                   if (cl-member auto-section-name org-super-agenda-auto-selector-keywords)
                   do (setq section-name (or custom-section-name "Auto category/group"))
                   and append (cl-loop for group in matching
                                       collect (list :name (plist-get group :name)
                                                     :items (plist-get group :items)
                                                     :order order))
                   into sections
                   and do (setq all-items non-matching)

                   ;; Manual groups
                   else
                   do (setq section-name (or custom-section-name auto-section-name))
                   and collect (list :name section-name :items matching :order order) into sections
                   and do (setq all-items non-matching)

                   ;; Sort sections by :order then :name
                   finally do (setq non-matching (list :name org-super-agenda-unmatched-name
                                                       :items non-matching
                                                       :order org-super-agenda-unmatched-order))
                   finally do (setq sections (--sort (let ((o-it (plist-get it :order))
                                                           (o-other (plist-get other :order)))
                                                       (cond ((and
                                                               ;; FIXME: This is now quite ugly.  I'm not sure that all of these tests
                                                               ;; are necessary, but at the moment it works, so I'm leaving it alone.
                                                               (equal o-it o-other)
                                                               (not (equal o-it 0))
                                                               (stringp (plist-get it :name))
                                                               (stringp (plist-get other :name)))
                                                              ;; Sort by string only for items with a set order
                                                              (string< (plist-get it :name)
                                                                       (plist-get other :name)))
                                                             ((and (numberp o-it)
                                                                   (numberp o-other))
                                                              (< o-it o-other))
                                                             (t nil)))
                                                     (push non-matching sections)))
                   ;; Insert sections
                   finally return (cl-loop for (_ name _ items) in sections
                                           when items
                                           collect (org-super-agenda--make-agenda-header name)
                                           and append items)))
      ;; No super-filters; return list unmodified
      all-items))

  (defun krisb-org-super-agenda--sort-matches-for-original-order (matching)
    "Sort MATCHING items back into their original ordering based on `org-entries-lessp'.
Only used when `org-super-agenda-keep-order' is non-nil."
    (--sort
     ;; Sorting has a shallow element of recursion because not all of the given items
     ;; are matched org headlines that can just be sorted using `org-entries-lessp'.
     ;; Some super-agenda matchers, like `:auto-category', will introduce sublists
     ;; whose contents need sorting of their own. In that case the lists' `:items'
     ;; properties need to be sorted instead.
     (let ((first-is-list (listp it))
           (second-is-list (listp other)))
       (when first-is-list
         (plist-put
          it :items
          (sort (plist-get it :items) #'org-entries-lessp)))
       (when second-is-list
         (plist-put
          other :items
          (sort (plist-get other :items) #'org-entries-lessp)))
       (cond
        (second-is-list t)
        (first-is-list nil)
        (t (org-entries-lessp it other))))
     matching)))

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
              (tags-todo "-inbox"
                         ((org-agenda-overriding-header "Projects and tasks to review")
                          (org-agenda-use-tag-inheritance '(todo))
                          (org-agenda-show-inherited-tags t)
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-skip-function 'org-review-agenda-skip)
                          (org-agenda-cmp-user-defined 'org-review-compare)
                          (org-agenda-sorting-strategy '(user-defined-down))))
              (tags-todo "-project-inbox"
                         ((org-agenda-overriding-header "Non-time-bound tasks")
                          (org-agenda-use-tag-inheritance '(todo))
                          (org-agenda-show-inherited-tags t)
                          (org-agenda-dim-blocked-tasks 'invisible)
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-entry-if 'scheduled)
                                  ;; Rather than something like:
                                  ;;   (not (org-review-agenda-skip))
                                  ;; We manually invert the definition of
                                  ;; `org-review-agenda-skip' because skipping
                                  ;; functions, if successful (i.e. reporting a
                                  ;; skip), must return the point which
                                  ;; org-agenda should continue from.
                                  (and (org-review-toreview-p)
                                       (org-with-wide-buffer (or (outline-next-heading) (point-max)))))))))))
            ("i" "Inbox: process entries"
             ((tags-todo "+inbox"
                         ((org-agenda-overriding-header "Review")
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-skip-function 'org-review-agenda-skip)
                          (org-agenda-cmp-user-defined 'org-review-compare)
                          (org-agenda-sorting-strategy '(user-defined-down))))
              (tags-todo "+inbox"
                         ((org-agenda-overriding-header "Non-review inbox")
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-skip-function
                           'krisb-org-review-has-review-property-p)))
              (todo "HOLD"
                    ((org-agenda-overriding-header "Non-review holds")
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
          ("C-c d" . krisb-consult-org-edna-block)
          :map org-agenda-mode-map
          ("C-c d". krisb-consult-org-agenda-edna-block))
  :config
  (org-edna-mode 1)

  ;; Bespoke `consult' integration.  This functionality depends on org-enda's
  ;; :BLOCKER: (which supports lists of IDs, rather than a single ID, like
  ;; `org-depend').
  (with-eval-after-load 'consult
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
              (consult-org-agenda (or match "/-DONE-CANCELED"))
              (setq dependency (org-get-heading))
              (when (equal current-heading dependency)
                (error "Cannot depend on the same `org-todo’!"))
              (setq new-id (org-id-get-create)))
            ;; Modify the BLOCKER property of the current todo
            (krisb-consult-org-edna--add-id new-id)
            (message "‘%s’ added as a dependency to this todo"
                     (substring-no-properties dependency))))))

    (defun krisb-consult-org-agenda-edna (&optional match)
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

    (consult-customize krisb-consult-org-edna-block
                       :prompt "Select dependency for the heading at point: "
                       krisb-consult-org-agenda-edna
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
  (org-review-sets-next-date t)
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
              (message "No scheduled date found for this item."))))))

  ;; "Scatter" org-review function; intended to be used as an org-agenda-bulk
  ;; action/function
  (defun krisb-org-review--select-day ()
    "Prompt for a number of days and return as an integer."
    (let ((days 0)
          (prompt "Scatter tasks across how many days? "))
      (while (<= days 0)
        (setq days (read-number prompt 7)
              prompt "Scatter tasks across how many days? Must be greater than 0: "))
      days))
  (defun krisb-org-review-randomize (days)
    "Randomly set the next review date for entry within the next DAYS days.
DAYS should be a positive integer.  Calls `org-review-insert-date' onto
a random date within the next DAYS days."
    (interactive (list (krisb-org-review--select-day)))
    (let* ((random-day (1+ (random days)))
           (ts (format-time-string (car org-time-stamp-formats)
                                   (time-add (current-time) (days-to-time random-day)))))
      ;; We don't also call `org-review-insert-last-review' because I sometimes
      ;; I do not want that.  In the cases when I'd like that function called as
      ;; well, I persist the org-agenda marks and call that function before or
      ;; after this one
      (org-review-insert-date org-review-next-property-name
                              org-review-next-timestamp-format
                              ts)))
  (with-eval-after-load 'org-agenda
    (add-to-list 'org-agenda-bulk-custom-functions
                 '(?R krisb-org-review-randomize
                      ;; Must return a list (of arguments)
                      (lambda () (list (krisb-org-review--select-day)))))))

;;; Provide
(provide 'krisb-org-agenda)
