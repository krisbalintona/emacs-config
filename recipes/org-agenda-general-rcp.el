;;; org-agenda-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Settings related to org-agenda itself.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Directories and variables
(require 'org-agenda)

(setq org-agenda-files kb/all-org-dir-files)

(setq org-agenda-start-with-log-mode t ; Show progression of done and clocked tasks in grid view
      org-agenda-log-mode-items '(closed clock) ; Things which should be added to grid view in log mode (turned on above)
      org-log-done 'time ; When done add CLOSED line with inactive timestamp
      org-log-into-drawer t) ; But everything into a drawer as opposed to appending it

;;;; Refiling and archiving
(setq org-refile-targets
      '(;; (nil :maxlevel . 9) ; Consider headlines in the current buffer
        (kb/all-agenda-dir-files-minus-inbox :maxlevel . 2)
        (kb/all-agenda-dir-files-minus-inbox :tag . "PROJECT")
        ))
(setq org-refile-use-outline-path 'file) ; Show file name while refiling
(setq org-outline-path-complete-in-steps nil) ; Don't have consequtive prompts for paths and headings
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Exclude DONE state tasks from refile targets
(defun kb/verify-refile-target ()
  "Remove todos in done states from possible refile target"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'kb/verify-refile-target)

;; Custom org-reverse-datetree refile function - Now outdated in favor of unpackaged/org-refile-to-datetree-using-ts-in-entry
(defun kb/org-refile-to-reverse-datetree-archive ()
  "Refile a todo into my archive file, with today as the day"
  (interactive "P")
  (org-reverse-datetree-refile-to-file
   kb/agenda-dir "archive.org" (current-time)))
;; :ask-always arg :prefer '("CREATED_TIME" "CREATED_AT" "CLOSED")))

;;;;;; Unpackaged.el
;; Useful `unpackaged.el' packages taken from
;; https://github.com/alphapapa/unpackaged.el#refile-to-datetree-file-using-earliestlatest-timestamp-in-entry
;; as well as my own-defined functions
(defun unpackaged/org-refile-to-datetree-using-ts-in-entry (which-ts file &optional subtree-p)
  "Refile current entry to datetree in FILE using timestamp found in entry.
        WHICH should be `earliest' or `latest'. If SUBTREE-P is non-nil,
        search whole subtree."
  (interactive (list (intern (completing-read "Which timestamp? " '(earliest latest)))
                     (read-file-name "File: " (concat org-directory "/") nil 'mustmatch nil
                                     (lambda (filename)
                                       (string-suffix-p ".org" filename)))
                     current-prefix-arg))
  (require 'ts)
  (let* ((sorter (pcase which-ts
                   ('earliest #'ts<)
                   ('latest #'ts>)))
         (tss (unpackaged/org-timestamps-in-entry subtree-p))
         (ts (car (sort tss sorter)))
         (date (list (ts-month ts) (ts-day ts) (ts-year ts))))
    (unpackaged/org-refile-to-datetree file :date date)))

        ;;;###autoload
(defun unpackaged/org-timestamps-in-entry (&optional subtree-p)
  "Return timestamp objects for all Org timestamps in entry.
         If SUBTREE-P is non-nil (interactively, with prefix), search
         whole subtree."
  (interactive (list current-prefix-arg))
  (save-excursion
    (let* ((beg (org-entry-beginning-position))
           (end (if subtree-p
                    (org-end-of-subtree)
                  (org-entry-end-position))))
      (goto-char beg)
      (cl-loop while (re-search-forward org-tsr-regexp-both end t)
               collect (ts-parse-org (match-string 0))))))

        ;;;###autoload
(cl-defun unpackaged/org-refile-to-datetree (file &key (date (calendar-current-date)) entry)
  "Refile ENTRY or current node to entry for DATE in datetree in FILE.
        DATE should be a list of (MONTH DAY YEAR) integers, e.g. as
        returned by `calendar-current-date'."
  (interactive (list (read-file-name "File: " (concat org-directory "/") nil 'mustmatch nil
                                     (lambda (filename)
                                       (string-suffix-p ".org" filename)))))
  ;; If org-datetree isn't loaded, it will cut the tree but not file
  ;; it anywhere, losing data. I don't know why
  ;; org-datetree-file-entry-under is in a separate package, not
  ;; loaded with the rest of org-mode.
  (require 'org-datetree)
  (unless entry
    (org-cut-subtree))
  ;; Using a condition-case to be extra careful. In case the refile
  ;; fails in any way, put cut subtree back.
  (condition-case err
      (with-current-buffer (or (org-find-base-buffer-visiting file)
                               (find-file-noselect file))
        (org-datetree-file-entry-under (or entry (car kill-ring)) date)
        (save-buffer))
    (error (unless entry
             (org-paste-subtree))
           (message "Unable to refile! %s" err))))

(defun kb/refile-to-datetree-using-ts ()
  "Uses unpackaged/org-refile-to-datetree-using-ts-in-entry with the target file being archive.org"
  (interactive)
  (funcall-interactively
   (unpackaged/org-refile-to-datetree-using-ts-in-entry 'earliest (expand-file-name "archive.org" kb/agenda-dir) t))
  )

;;;; Org todo settings
;; Causes freezing on refile and changing priority for parent todos
(setq org-enforce-todo-dependencies nil)
(setq org-enforce-todo-checkbox-dependencies nil)
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-todo-ignore-time-comparison-use-seconds t) ; Compare seconds, not days

                                        ; Classic org-agenda archiving
(setq org-archive-location (concat kb/agenda-dir "archive.org::"))

;; Change todo states with S-left and S-right skipping all of the
;; normal processing when entering or leaving a todo state. This
;; cycles through the todo states but skips setting timestamps and
;; entering notes which is very convenient when all you want to do is
;; fix up the status of an entry.
(setq org-use-fast-todo-selection 'auto)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w!)" "NEXT(n)" "PROG(p)" "|" "DONE(d!/@)")
        (sequence "TICKLE(T)" "HOLD(h!)" "SOMEDAY(s)" "INACTIVE(i@)" "|" "CANCELLED(c@/!)")
        ))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "orange" :underline t)
        ("NEXT" :foreground "orchid" :weight bold)
        ("TICKLE" :foreground "bisque3" :weight bold)
        ("WAITING" :foreground "khaki1" :weight bold)
        ("PROG" :foreground "turquoise" :underline t)
        ("DONE" :foreground "chartreuse" :weight normal)
        ("HOLD" :foreground "LightGoldenrod1" :weight normal :underline t)
        ("SOMEDAY" :foreground "aquamarine" :weight normal)
        ("CANCELLED" :foreground "deep pink" :weight normal)
        ("INACTIVE" :foreground "light slate blue" :weight normal)
        ))

;; Priorities
;; (setq org-priority-faces ; Faces set by org-fancy-priorities
;;       '((65 :foreground "#e45649")
;;         (66 :foreground "#da8548")
;;         (67 :foreground "#0098dd")))
(setq org-priority-highest ?A
      org-priority-lowest ?F
      org-priority-default ?D) ; This needs to be defined due to a bug which uses the old variable names (these) instead of the new ones (the following)
(setq org-highest-priority ?A
      org-lowest-priority ?F
      org-default-priority ?D)

                                        ; Add or remove tags as you change the checkbox state
;; (setq org-todo-state-tags-triggers
;;       '(("TODO" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("NEXT" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("DONE" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("HOLD" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("CANCELLED" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("INACTIVE" ("NEXT") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))))

;; ; Automatically change todo keyword to DONE when all children are complete
;; (defun kb/org-summary-todo (n-done n-not-done)
;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;   (let (org-log-done org-log-states)   ; turn off logging
;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
;; (add-hook 'org-after-todo-statistics-hook 'kb/org-summary-todo)

;; Remove empty LOGBOOK drawers on clock out
(defun kb/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'kb/remove-empty-drawer-on-clock-out 'append)

;;;; Agenda view and stuck tasks
;; Window setup
(setq org-agenda-window-setup 'current-window) ; Open agenda in current window
(setq org-indirect-buffer-display 'current-window) ; Put indirect buffers right on top of the current window
(setq org-agenda-include-diary t) ; Show diary in calendar

(add-hook 'org-agenda-finalize-hook 'evil-goto-first-line) ; Start at first line in org-agenda

;; Custom bulk mark functions
(setq org-agenda-bulk-custom-functions
      '((?P org-agenda-priority)
        (?R kb/org-agenda-process-inbox-item)
        ))

;; Columns
(setq org-tags-column -77)
;; (setq org-agenda-tags-column -208)
;; (add-hook 'after-focus-change-function
;;           (lambda () (progn
;;                        (setq org-tags-column -80)
;;                        (org-align-tags t))))

;; Schedule and item format settings
(setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      ;; org-agenda-include-deadlines t
      org-agenda-block-separator 61
      org-agenda-tags-column 'auto
      org-cycle-separator-lines 0) ; Remove blank lines when folding

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo . " %i %-13:c   %-6e %?s %t |%l")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")
        ))
;; org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%a, %b %e - %H:%M>") ; For org-agenda timestamps, default is '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")

;; Stuck projects
(setq org-stuck-projects
      '("*/!-HOLD-SOMEDAY" ; Tags/todo/property matcher that identifies which tasks are projects
        ("PROG" "NEXT") ; Todo keywords for non-stuck projects
        nil ; Tags for non-stuck projects
        "" ; Any regexp for non-stuck projects
        ))

;; Function to quicky set effort, priority, tags, and refile.
;; From https://blog.jethro.dev/posts/processing_inbox/
(defun kb/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (interactive)
  (org-with-wide-buffer
   ;; (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-todo)
   ;; (call-interactively 'jethro/my-org-agenda-set-effort)
   ;; (org-agenda-set-effort)
   (org-agenda-refile nil nil t)
   ))

;;;; Make sure org-agenda doesn't keep files open
(require 'dash)

(defun my-org-keep-quiet (orig-fun &rest args)
  (let ((buffers-pre (-filter #'get-file-buffer (org-agenda-files))))
    (apply orig-fun args)
    (let* ((buffers-post (-filter #'get-file-buffer (org-agenda-files)))
           (buffers-new  (-difference buffers-post buffers-pre)))
      (mapcar (lambda (file) (kill-buffer (get-file-buffer file))) buffers-new))))

(advice-add 'org-agenda-list :around #'my-org-keep-quiet)
(advice-add 'org-search-view :around #'my-org-keep-quiet)
(advice-add 'org-tags-view   :around #'my-org-keep-quiet)

;;;; Habits
(add-to-list 'org-modules 'org-habit)
(customize-set-variable 'org-habit-show-habits-only-for-today nil) ; I want to see when habits are rescheduled for
(customize-set-variable 'org-habit-preceding-days 8)
(customize-set-variable 'org-habit-following-days 4)
(customize-set-variable 'org-extend-today-until 4) ; The day ends at 3AM!
(customize-set-variable 'org-use-effective-time t) ; Set timestamps respecting org-extend-today-until
(customize-set-variable 'org-habit-graph-column 90)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t))) ; Force showing of habits in agenda every day at 6AM

;;;; Keybindings
(kb/leader-keys
  "oa" '(org-agenda :which-key "Org-agenda")

  "mn" '(org-capture-goto-last-stored :which-key "Goto last note captured")
  )

(general-define-key ; Eyebrowse keybindings overwrite this so I reset it
 :keymaps 'org-capture-mode-map
 "C-c C-w" 'org-capture-refile
 )

(general-define-key
 :keymaps 'org-agenda-mode-map
 [remap org-agenda-archive] 'kb/refile-to-datetree-using-ts ; Archive
 "r" 'kb/org-agenda-process-inbox-item ; Process task
 )

(add-hook 'org-agenda-mode-hook ; Don't rebuild agenda buffer after "g"
          (lambda ()
            (general-define-key
             :keymaps 'local
             "g" nil)
            ))
(add-hook 'org-super-agenda-mode-hook ; Don't rebuild agenda buffer after "g"
          (lambda ()
            (general-define-key
             :keymaps 'local
             "g" nil)
            ))

;;;; Org-capture-templates
(setq org-default-notes-file (concat kb/agenda-dir "inbox.org"))
(setq org-capture-templates ; Used for org-agenda task management
      '(("i" "New input")
        ("iv" "Video" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Watch %(org-cliplink-capture)\n%U\n"
         :immediate-finish t)
        ("ia" "Article" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Read %(org-cliplink-capture)\n%U\n"
         :immediate-finish t)
        ("ip" "Podcast" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Listen to %(org-cliplink-capture)\n%U\n")
        ("iw" "Profound quote" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p%?\nby \n%U\n\n")
        ("ib" "Book" entry (file org-default-notes-file)
         "* TODO [#%^{Priority?|A|B|C|D|E|F}] Read /%?/\nby \n%U\n")
        ("il" "Lecture" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Watch and study %(org-cliplink-capture)\n%U\n"
         :immediate-finish t)
        ("ij" "Academic paper" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Read and analyze %(org-cliplink-capture)\n%U\n"
         :immediate-finish t)

        ("m" "New entertainment to gobble" entry (file org-default-notes-file)
         "* TODO Consume %? %^{What type of entertainment?|MOVIE|BOOK|SHOW|VIDEO}\n%U\n")

        ("e" "Email" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Revisit %:fromname ( %:fromaddress ) -- /%:subject/ [/] :EMAIL:\n- RECEIVED :: %:date-timestamp-inactive\nSCHEDULED: %^t\nDEADLINE: %^T\n%A\n%U")
        ("a" "General todo" entry (file org-default-notes-file)
         "* TODO %? [/] %^G\n%U")
        ("f" "This is an idea I should ferment" entry (file+headline "~/Documents/org-database/roam/seedbox.org" "Fermenting Seeds")
         "* %? \n%U")
        ))

;;; org-agenda-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-general-rcp)
