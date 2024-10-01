;;; org-agenda-other-rcp.el --- Other org-agenda configs  -*- lexical-binding: t; -*-

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

;; Other org-agenda packages.

;;; Code:

;;;; Solar
(use-package solar
  :ensure nil
  :custom
  ;; Geolocation
  ;; TODO 2024-10-01: Make sure below isn't blocking when no internet is available.
  (calendar-latitude (car (kb/get-lat-lon)))
  (calendar-longitude (cdr (kb/get-lat-lon)))
  (calendar-location-name (kb/get-location-name))
  :init
  (defun kb/get-lat-lon ()
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
        (message "[kb/get-lat-lon] Waiting...")
        (sit-for 0.1))
      (if (and lat lon)
          (cons lat lon)
        (message "Failed to fetch geolocation data")
        nil)))

  (defun kb/get-location-name ()
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
        (message "[kb/get-location-name] Waiting...")
        (sit-for 0.1))
      (if (and city region)
          (format "%s, %s" city region)
        (message "Failed to fetch geolocation data")
        nil))))

;;;; Calendar
(use-package calendar
  :ensure nil
  :custom
  (calendar-time-display-form
   '( 24-hours ":" minutes (when time-zone (format "(%s)" time-zone))))
  (calendar-week-start-day 1)           ; Monday
  (calendar-time-zone-style 'symbolic)

  ;; Diary
  (calendar-mark-diary-entries-flag t)

  ;; Holidays
  (calendar-mark-holidays-flag t))

;;;; Org-expiry
(use-package org-expiry
  :ensure nil
  :hook (org-capture-before-finalize . org-expiry-insert-created)
  :custom
  (org-expiry-inactive-timestamps t))

;;;; Org-depend
;; Add blocking and triggering actions when an org-todo state is changed.
(use-package org-depend
  :ensure nil
  :after org-contrib
  :demand
  :after org-agenda
  :commands kb/consult-org-id-get-create)

;;;; Org-edna
;; Also look at `org-edna' with `org-linker-edna'
;; (https://github.com/toshism/org-linker-edna) (the second of which requires
;; `org-linker': https://github.com/toshism/org-linker). `org-super-links' can
;; be added to see which tasks are being blocked by the current task. See
;; https://karl-voit.at/2021/01/23/org-linker-edna/ for sample workflow
(use-package org-edna
  :after org-agenda
  :diminish
  :bind
  ( :map org-mode-map
    ("C-c d" . kb/consult-org-depend)
    :map org-agenda-mode-map
    ("C-c d". kb/consult-org-agenda-depend))
  :config
  (org-edna-mode 1)

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

;;;; Custom org-tags-view org-link type
(with-eval-after-load 'org
  (defun kb/org-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
  With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view (null current-prefix-arg) tag))

  (org-add-link-type "tag" 'kb/org-tag-link))

;;;; Ical2orgpy script
;; NOTE 2024-01-24: Make sure ical2orgpy is installed via `pipx install
;; ical2orgpy'
(with-eval-after-load 'org-agenda
  (unless (system-packages-package-installed-p "ical2orgpy")
    (system-packages-ensure "pipx install --force ical2orgpy"))

  (defun kb/org-gcal (&optional arg)
    "Run my emacs-gcal script.
  If called with ARG, then show output buffer. Else, keep output
  buffer hidden."
    (interactive "P")
    (let* ((buf-name "*emacs-gcal*")
           (buf (get-buffer-create buf-name))
           (script (expand-file-name "emacs-gcal.sh" "~/Scripts/"))
           (display-buffer-alist (if arg
                                     display-buffer-alist
                                   `((,buf-name display-buffer-no-window)))))
      (unless (get-buffer-process buf)
        ;; OPTIMIZE 2024-01-24: Consider using `start-process' instead of
        ;; `async-shell-command'
        (async-shell-command script buf))))

  ;; Timer every 30 min
  (run-with-timer (* 60 30) (* 60 30) 'kb/org-gcal))

(provide 'org-agenda-other-rcp)
;;; org-agenda-other-rcp.el ends here
