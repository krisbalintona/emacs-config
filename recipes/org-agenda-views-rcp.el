;;; org-agenda-views-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Org-agenda views
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Org-ql
;; More powerful searching and selecting of todo headlines
(use-package org-ql
  :straight (org-ql :type git :host github :repo "alphapapa/org-ql")
  :after org-roam ; Necessary for one or more of the functions
  :config
  (setq org-ql-views
        `(("Current projects"
           :buffers-files org-agenda-files
           :query (and (parent (todo))
                       (todo)
                       (not (done)))
           :super-groups ((:auto-parent t))
           :title "Current projects"
           :sort (deadline scheduled todo priority)
           )
          ("In-progress tasks not in a project"
           :buffers-files org-agenda-files
           :query (and (not (done))
                       (not (parent (todo)))
                       (not (children (todo)))
                       (todo "PROG"))
           :super-groups ((:discard (:file-path "roam"))
                          (:name none
                                 :anything t))
           :title "In-progress tasks"
           :sort (priority deadline scheduled)
           )
          ("In-progress and upcoming Zettelkasten notes"
           :buffers-files ,(org-roam--list-all-files)
           :query (and (not (done))
                       (todo "PROG" "NEXT"))
           :super-groups ((:name none
                                 :anything t))
           :title "Zettelkasten notes I'm working on"
           :sort (todo deadline scheduled priority)
           )
          ))
  ;; (setq org-ql-view-buffer nil) ; No clue how to set this variable

  (kb/general-keys
    "oq" '(org-ql-view :which-key "Org-ql views") ; Currently can't find a way to close all org-agenda bufers after opening the org-ql-view
    )
  )

;;;; Org-super-agenda
;; Autoload the package and start with no views
(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)

  ;; Remove Evil bindings on group headings
  (define-key org-agenda-mode-map (kbd "h") nil)
  (define-key org-super-agenda-header-map (kbd "h") nil)
  (define-key org-agenda-mode-map (kbd "j") nil)
  (define-key org-super-agenda-header-map (kbd "j") nil)
  (define-key org-agenda-mode-map (kbd "k") nil)
  (define-key org-super-agenda-header-map (kbd "k") nil)
  (define-key org-agenda-mode-map (kbd "l") nil)
  (define-key org-super-agenda-header-map (kbd "l") nil)

  (setq org-agenda-custom-commands nil) ; Start from scratch
  )

;;;; Org-agenda-views
;;;;; Active tasks
(add-to-list 'org-agenda-custom-commands
             '("c" "Schoolwork"
               ((alltodo ""
                         ((org-agenda-overriding-header "")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "school.org"))) 
                             (:auto-planning t)
                             (:discard (:anything t))
                             ))
                          ))
                (org-ql-block '(and (path "school.org")
                                    (parent (tags "PROJECT"))
                                    (todo)
                                    (not (done)))
                              ((org-ql-block-header "")
                               (org-agenda-files kb/all-agenda-dir-files-minus-inbox)
                               (org-super-agenda-groups
                                '((:auto-outline-path t)
                                  ))
                               ))
                (alltodo ""
                         ((org-agenda-overriding-header "")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "school.org"))) 
                             (:discard (:not (:todo "PROG")))
                             (:discard (:tag "PROJECT"))
                             (:tag "URGENT")
                             (:tag "TOP")
                             (:tag "TRANSITORY")
                             (:tag "DEMANDING")
                             (:tag "BACKGROUND")
                             (:name "Uncategorized" :anything t)
                             (:discard (:anything t))
                             ))
                          ))
                (alltodo ""
                         ((org-agenda-overriding-header "")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "school.org"))) 
                             (:discard (:not (:todo "NEXT")))
                             (:discard (:tag "PROJECT"))
                             (:tag "URGENT")
                             (:tag "TOP")
                             (:tag "TRANSITORY")
                             (:tag "DEMANDING")
                             (:tag "BACKGROUND")
                             (:name "Uncategorized" :anything t)
                             ))
                          ))
                (org-ql-block '(and (path "school.org")
                                    (not (parent (todo)))
                                    (not (todo "PROG" "NEXT"))
                                    (not (tags "PROJECT"))
                                    (todo)
                                    (not (done)))
                              ((org-ql-block-header "")
                               (org-agenda-files kb/all-agenda-dir-files-minus-inbox)
                               (org-super-agenda-groups
                                '((:anything t)
                                  ))
                               ))
                (org-ql-block '(and (path "school.org")
                                    (tags-local "PROJECT")
                                    (not (children))
                                    (todo)
                                    (not (done)))
                              ((org-ql-block-header "Projects without any tasks")
                               (org-agenda-files kb/all-agenda-dir-files-minus-inbox)
                               (org-super-agenda-groups
                                '((:auto-category t)
                                  ))
                               ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("p" "Personal tasks"
               ((alltodo ""
                         ((org-agenda-overriding-header "")
                          (org-super-agenda-groups
                           '((:discard (:file-path "school.org"))
                             (:discard (:file-path "habits.org"))
                             (:discard (:not (:todo "PROG")))
                             (:discard (:tag "PROJECT"))
                             (:tag "URGENT")
                             (:tag "TOP")
                             (:tag "TRANSITORY")
                             (:tag "DEMANDING")
                             (:tag "BACKGROUND")
                             (:name "Uncategorized" :anything t)
                             ))
                          ))
                (alltodo ""
                         ((org-agenda-overriding-header "")
                          (org-super-agenda-groups
                           '((:discard (:file-path "school.org"))
                             (:discard (:file-path "habits.org"))
                             (:auto-planning t)
                             (:discard (:anything t))
                             ))
                          ))
                (org-ql-block '(and (not (path "school.org"))
                                    (not (path "habits.org"))
                                    (parent (tags "PROJECT"))
                                    (todo)
                                    (not (done)))
                              ((org-ql-block-header "")
                               (org-agenda-files kb/all-agenda-dir-files-minus-inbox)
                               (org-super-agenda-groups
                                '((:auto-outline-path t)
                                  ))
                               ))
                (org-ql-block '(and (not (path "school.org"))
                                    (not (path "habits.org"))
                                    (tags-local "PROJECT")
                                    (not (children))
                                    (todo)
                                    (not (done)))
                              ((org-ql-block-header "Projects without any tasks")
                               (org-agenda-files kb/all-agenda-dir-files-minus-inbox)
                               (org-super-agenda-groups
                                '((:auto-category t)
                                  ))
                               ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("n" "What's next?"
               ((org-ql-block '(and (not (parent (todo)))
                                    (not (children (todo)))
                                    (todo "NEXT")
                                    (not (done)))
                              ((org-ql-block-header "Next Non-project Tasks")
                               (org-super-agenda-groups
                                '((:name "No effort or effort less than 5 minutes"
                                         :effort< "5")
                                  (:name "10 minutes or less"
                                         :effort< "11")
                                  (:name "30 minutes or less"
                                         :effort< "31")
                                  (:name "1 hour or less"
                                         :effort< "61")
                                  (:name "More than an hour but less than 3"
                                         :effort< "180")
                                  (:name "3 hours or more"
                                         :effort> "179")
                                  (:name "Next tasks without an effort rating"
                                         :anything t)
                                  ))
                               ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("T" "Tickles"
               ((agenda ""
                        ((org-agenda-overriding-header "My habit calendar")
                         (org-agenda-span 'week)
                         (org-agenda-start-day "+2") ; Start the agenda view with yesterday
                         (org-agenda-span 'fortnight)
                         (org-super-agenda-groups
                          '((:discard (:not (:todo "TICKLE")))
                            (:anything t)
                            ))
                         ))
                (alltodo ""
                         ((org-agenda-overriding-header "Unscheduled or missed tickles")
                          (org-super-agenda-groups
                           '((:discard (:not (:todo "TICKLE")))
                             (:name "Unscheduled" :and (:deadline nil :scheduled nil))
                             (:name "Missed" :and (:deadline past :scheduled past))
                             (:name "Future?" :and (:deadline future :scheduled future))
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("z" "Current Zettelkasten notes"
               ((alltodo ""
                         ((org-agenda-overriding-header "Current and upcoming Zettelkasten notes")
                          (org-agenda-files (org-roam--list-all-files))
                          (org-super-agenda-groups
                           '((:discard (:not (:todo ("PROG" "NEXT"))))
                             (:name none
                                    :auto-parent t)
                             ))
                          ))
                ))
             t)

;;;;; Zettelkasten
(add-to-list 'org-agenda-custom-commands
             '("Zz" "Fresh Zettelkasten notes"
               ((alltodo ""
                         ((org-agenda-overriding-header "Zettelkasten maintanence overview")
                          (org-agenda-files (org-roam--list-all-files))
                          (org-super-agenda-groups
                           '((:name "Tags that are done but not marked as done"
                                    :tag ("MATURE" "COMPLETE"))
                             (:name "Fermenting notes"
                                    :tag "ephemeral")
                             (:name "Unprocessed ephemeral notes" ; Remove once I've finished processing all the notes with the ephemeral tag. I use my seedbox for this now
                                    :tag "ephemeral")
                             (:name "Fresh notes"
                                    :tag ("WAITING" "NASCENT"))
                             (:name "Intermediate notes"
                                    :tag ("PROGRESS" "GROWING"))
                             (:name "Irregular notes"
                                    :anything t)
                             ))
                          ))
                (alltodo ""
                         ((org-agenda-overriding-header "By category")
                          (org-agenda-files (org-roam--list-all-files))
                          (org-super-agenda-groups
                           '((:name "Tags that are done but not marked as done"
                                    :tag ("MATURE" "COMPLETE"))
                             (:name none
                                    :auto-category t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("Zg" "Zettelkasten growth and done"
               ((alltodo ""
                         ((org-agenda-overriding-header "All notes organized by note-type")
                          (org-agenda-files (org-roam--list-all-files))
                          (org-super-agenda-groups
                           '((:name none
                                    :auto-category t)
                             ))
                          ))
                (todo "DONE|CANCELLED"
                      ((org-agenda-overriding-header "Finished notes")
                       (org-agenda-files (org-roam--list-all-files))
                       (org-super-agenda-groups
                        '((:discard (:not (:category ("lit" "bib_notes" "quote" "zett" "ephemeral"))))
                          (:name none
                                 :auto-ts t)
                          (:name "Irregular notes"
                                 :anything t)
                          ))
                       ))
                ))
             t)

;;;;; Maintainence
(add-to-list 'org-agenda-custom-commands
             '("xu" "Projects potentially in limbo (via stuck projects)"
               ((stuck ""
                       ((org-agenda-overriding-header "School")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "school.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Computer stuff")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "computers.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Inputs")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "inputs.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Miscellaneous")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "misc.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Habits")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "habits.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Entertainment")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "media.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("xb" "Stuff in the backburner"
               ((alltodo ""
                         ((org-agenda-overriding-header "Did I forget about these?")
                          (org-super-agenda-groups
                           '((:discard (:not (:todo ("SOMEDAY" "HOLD" "INACTIVE"))))
                             (:name none
                                    :auto-category t)
                             (:name "You shouldn't be here..."
                                    :anything t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("xd" "Todos in a DONE state"
               ((todo "DONE|CANCELLED"
                      ((org-agenda-overriding-header "Regular candidates for archival")
                       (org-agenda-files (directory-files-recursively kb/agenda-dir "[^hive].org$"))))
                (todo "DONE|CANCELLED"
                      ((org-agenda-overriding-header "Done Zettelkasten notes")
                       (org-agenda-files (org-roam--list-all-files))))
                )
               )
             t)

(add-to-list 'org-agenda-custom-commands
             '("xr" "All trivial and to-refile tasks"
               ((alltodo ""
                         ((org-agenda-overriding-header "Tasks to refile")
                          (org-super-agenda-groups
                           '((:discard (:not (:tag "REFILE")))
                             (:auto-tags t)
                             (:discard (:anything t))
                             ))
                          ))
                (alltodo ""
                         ((org-agenda-overriding-header "High-priority items without next todo keyword")
                          (org-super-agenda-groups
                           '((:discard (:todo ("NEXT" "PROG")))
                             (:discard (:todo ("WAITING" "TICKLE")))
                             (:discard (:file-path "habits.org"))
                             (:name none :and (:priority>= "B" :not (:todo ("NEXT" "PROG"))))
                             (:discard (:anything t))
                             ))
                          ))
                (alltodo ""
                         ((org-agenda-overriding-header "Trivial Tasks")
                          (org-super-agenda-groups
                           '((:name none
                                    :and (:priority<= "E" :not (:todo ("HOLD" "SOMEDAY" "INACTIVE" "CANCELLED"))))
                             (:discard (:anything t))
                             ))
                          ))
                ))
             t)

;;;;; Habits
(add-to-list 'org-agenda-custom-commands
             '("h" "Habits"
               ((agenda ""
                        ((org-agenda-overriding-header "My habit calendar")
                         (org-agenda-span 'week)
                         (org-agenda-start-day "+0") ; Start the agenda view with yestersy
                         (org-agenda-span 3)
                         (org-super-agenda-groups
                          '((:discard (:not (:file-path "habits.org")))
                            (:anything t)
                            ))
                         ))
                ))
             t)

;;;;; File-specific
(add-to-list 'org-agenda-custom-commands
             '("fw" "Schoolwork"
               ((agenda ""
                        ((org-agenda-overriding-header "My school calendar")
                         (org-agenda-span 'week)
                         (org-agenda-start-day "-1") ; Start the agenda view with yestersy
                         (org-agenda-span 7)
                         (org-super-agenda-groups
                          '((:discard (:not (:file-path "school.org")))
                            (:name "Due"
                                   :time-grid t
                                   :deadline today)
                            (:name "Planned"
                                   :time-grid t
                                   :scheduled today)
                            (:name "Due in the future"
                                   :time-grid t
                                   :deadline future)
                            (:name "Planned in the future"
                                   :time-grid t
                                   :scheduled future)
                            (:name "Missed Items!"
                                   :scheduled past
                                   :deadline past)
                            (:name "Uncategorized"
                                   :anything t)
                            ))
                         ))
                (alltodo ""
                         ((org-agenda-overriding-header "Assignments on my plate...")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "school.org")))
                             (:name "Related to coursework"
                                    :tag ("ASSIGNMENT" "EMAIL"))
                             (:name "Me involved with the community"
                                    :tag ("CLUB" "EVENT" "SOCIAL" "ORGANIZATION"))
                             (:name "Consumption"
                                    :tag ("LEARN" "PARSE"))
                             (:name "Finances"
                                    :tag ("PAYING"))
                             (:name "Overflow (uncategorized)"
                                    :anything t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("fe" "Entertainment time?"
               ((alltodo ""
                         ((org-agenda-overriding-header "What's on my \"to-comsume\" list?")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "media.org")))
                             (:name "Movies" :tag "MOVIE")
                             (:name "Shows" :tag "SHOW")
                             (:name "Books" :tag "BOOK")
                             (:name "Videos" :tag "VIDEOS")
                             (:name "Manga" :tag "MANGA")
                             (:name "Overflow (uncategorized)" :anything t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("fc" "Computer-related tasks"
               ((alltodo ""
                         ((org-agenda-overriding-header "Computer stuff I have to get to")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "computers.org")))
                             (:name "Projects"
                                    :tag ("PROJECT"))
                             (:name "Things that involve thinking"
                                    :tag ("DWELL" "WORKFLOW"))
                             (:name "Going through information"
                                    :tag ("LEARN" "PACKAGE" "DOCS"))
                             (:name "Actions for the better"
                                    :tag ("CONFIG" "TROUBLESHOOTING"))
                             (:name "Actions for QoL"
                                    :tag ("RICE"))
                             (:name "Overflow (uncategorized)"
                                    :anything t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("fi" "My input tasks"
               ((alltodo ""
                         ((org-agenda-overriding-header "All my inputs")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "inputs.org")))
                             (:name none
                                    :auto-tags t)
                             ))
                          ))
                ))
             t)

;;;; Appearance of views
;;;;; Agenda view faces
(if (not (featurep 'elegant-agenda-mode)) ; Only load if elegant-agenda-mode is disabled
    (with-eval-after-load 'org-super-agenda
      (set-face-attribute 'org-super-agenda-header nil :height 148 :font kb/variable-pitch-font :foreground "DarkGoldenrod2" :underline nil)
      (set-face-attribute 'org-agenda-date nil :height 157 :font kb/variable-pitch-font :foreground "dodger blue" :underline nil)
      (set-face-attribute 'org-agenda-structure nil :height 180 :font kb/variable-pitch-font :bold t :italic t :foreground "DarkOliveGreen3" :underline t)
      ))

;;;;; Elegant-agenda-mode
;; Pre-configured org-agenda to be beautiful
(use-package elegant-agenda-mode
  :straight (elegant-agenda-mode :type git :host github :repo "justinbarclay/elegant-agenda-mode")
  :hook (org-agenda-mode . elegant-agenda-mode)
  :custom
  (elegant-agenda-font "Yanone Kaffeesatz") ; Other fonts may not support the weights used
  (elegant-agenda-face-remappings ; Default but with smaller font sizes
   '((default
       (:family elegant-agenda-font :height 130 :weight thin))
     (header-line
      (:family elegant-agenda-font :height 154 :weight thin :underline nil :overline nil :box nil))
     (org-agenda-date-today
      (:weight regular))
     (org-agenda-structure
      (:weight regular))
     (bold
      (:height 149 :weight thin)))
   )
  )

;;; org-agenda-views-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-views-rcp)
