;;; org-agenda-other-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Other org-agenda packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;; Org-depend
;; Add blocking and triggering actions when an org-todo state is changed.
(use-package org-depend
  :elpaca nil
  :after org-contrib
  :demand
  :after org-agenda
  :commands kb/consult-org-id-get-create)

;;; Org-edna
;; Also look at `org-edna' with `org-linker-edna'
;; (https://github.com/toshism/org-linker-edna) (the second of which requires
;; `org-linker': https://github.com/toshism/org-linker). `org-super-links' can
;; be added to see which tasks are being blocked by the current task. See
;; https://karl-voit.at/2021/01/23/org-linker-edna/ for sample workflow
(use-package org-edna
  :after org
  :diminish
  :general
  (:keymaps 'org-mode-map
            "C-c d" 'kb/consult-org-depend)
  (:keymaps 'org-agenda-mode-map
            "C-c d" 'kb/consult-org-agenda-depend)
  :init
  (org-edna-mode)
  :config
  (with-eval-after-load 'consult
    (defun kb/consult-org-depend--add-id (new-id)
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

    (defun kb/consult-org-depend (&optional match)
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
            (kb/consult-org-depend--add-id new-id)
            (message "‘%s’ added as a dependency to this todo"
                     (substring-no-properties dependency))))))

    (defun kb/consult-org-agenda-depend (&optional match)
      "Create a dependency for the `org-agenda’ item at point.
See `kb/consult-org-depend’."
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
                (setq dependency (funcall 'kb/consult-org-depend match))))))))

    (consult-customize kb/consult-org-depend
                       :prompt "Select dependency for the heading at point: "
                       kb/consult-org-agenda-depend
                       :prompt "Select dependency for this agenda item: ")))

;;; Org-gcal
(use-package plstore                    ; Dependency
  :elpaca nil
  :config
  ;; Don't get prompted for password so much. Has to be with `setq' since it
  ;; isn't a customizable variable. See https://github.com/kidd/org-gcal.el#note
  (setq plstore-cache-passphrase-for-symmetric-encryption t))
(use-package org-gcal
  :custom
  ;; NOTE 2023-01-22: If syncing broke for some reason, try
  ;; `org-gcal-sync-tokens-clear'
  (org-gcal-client-id "477180658883-q2ok2j39ko4bfp88e2tqd9qi6c1r6ebm.apps.googleusercontent.com")
  (org-gcal-client-secret "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")
  (oauth2-auto-plstore (expand-file-name "oauth2-auto.plist" org-gcal-dir)) ; File used by `plstore'
  (org-generic-id-locations-file        ; File for org IDs
   (convert-standard-filename (expand-file-name ".org-generic-id-locations" org-gcal-dir)))
  (org-gcal-fetch-file-alist
   `(;; University
     ("v7tpr3s152ao11tlf93lu3don4@group.calendar.google.com" . ,(expand-file-name "gcal/brown.org" kb/agenda-dir))
     ("brown.edu_d61gju3thc3a7e58k84qbn1nc8@group.calendar.google.com" . ,(expand-file-name "gcal/crc_events.org" kb/agenda-dir))
     ("c_8ri64bp98ab1oj28704npqhig4@group.calendar.google.com" . ,(expand-file-name "gcal/assignments.org" kb/agenda-dir))
     ("c_g2s8uc0cufru3ruq7g7tbd1a7k@group.calendar.google.com" . ,(expand-file-name "gcal/bui.org" kb/agenda-dir))
     ("c_pr2pb1gdf5dkogh3h13kvs8uf0@group.calendar.google.com" . ,(expand-file-name "gcal/office_hours.org" kb/agenda-dir))
     ("independent_study@brown.edu" . ,(expand-file-name "gcal/independent_study.org" kb/agenda-dir))
     ("kristoffer_balintona@brown.edu" . ,(expand-file-name "gcal/kristoffer_balintona_events.org" kb/agenda-dir))
     ;; Personal
     ("ic4ecccdo60mub7raqhear02vg@group.calendar.google.com" . ,(expand-file-name "gcal/birthdays.org" kb/agenda-dir))))
  (org-gcal-up-days 31)
  (org-gcal-down-days 62)
  (org-gcal-recurring-events-mode 'top-level)
  (org-gcal-notify-p t)
  (org-gcal-update-cancelled-events-with-todo t)
  (org-gcal-remove-api-cancelled-events t)
  ;; Time zone
  ;; (org-gcal-local-timezone "America/Chicago")
  (org-gcal-local-timezone "America/New_York")
  :init
  (require 'plstore))

;;; Org-heatmap
;; Heatmap in agenda for tracked habits. Also highlighted calendar dates
(use-package org-heatmap
  :disabled  ; REVIEW 2023-07-10: Doesn't work for me right now. Return to later
  :after org
  :elpaca (:type git :host github :repo "Elilif/org-heatmap")
  :custom
  (org-heatmap-db-location (no-littering-expand-var-file-name "org-heatmap/"))
  (org-heatmap-enable-habit-statics t)
  :init
  (org-heatmap-mode))

;;; Org-timeblock
(use-package org-timeblock
  :elpaca (:type git :host github :repo "ichernyshovvv/org-timeblock"))

;;; Custom org-tags-view org-link type
(with-eval-after-load 'org
  (defun kb/org-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view (null current-prefix-arg) tag))

  (org-add-link-type "tag" 'kb/org-tag-link))

;;; org-agenda-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-other-rcp)
