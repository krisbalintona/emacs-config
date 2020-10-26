;;; org-gcal-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-gcal" "org-gcal.el" (0 0 0 0))
;;; Generated autoloads from org-gcal.el

(autoload 'org-gcal-sync "org-gcal" "\
Import events from calendars.
Export the ones to the calendar if unless
SKIP-EXPORT.  Set SILENT to non-nil to inhibit notifications.

\(fn &optional SKIP-EXPORT SILENT)" t nil)

(autoload 'org-gcal-fetch "org-gcal" "\
Fetch event data from google calendar." t nil)

(autoload 'org-gcal-sync-buffer "org-gcal" "\
Sync entries containing Calendar events in the currently-visible portion of the
buffer.

Updates events on the server unless SKIP-EXPORT is set. In this case, events
modified on the server will overwrite entries in the buffer.
Set SILENT to non-nil to inhibit notifications.

\(fn &optional SKIP-EXPORT SILENT)" t nil)

(autoload 'org-gcal-fetch-buffer "org-gcal" "\
Fetch changes to events in the currently-visible portion of the buffer, not
writing any changes to Calendar.

\(fn &optional SKIP-EXPORT SILENT)" t nil)

(autoload 'org-gcal-post-at-point "org-gcal" "\
Post entry at point to current calendar. This overwrites the event on the
server with the data from the entry, except if the ‘org-gcal-etag-property’ is
present and is out of sync with the server, in which case the entry is
overwritten with data from the server instead.

If SKIP-IMPORT is not nil, don’t overwrite the entry with data from the server.
If SKIP-EXPORT is not nil, don’t overwrite the event on the server.

\(fn &optional SKIP-IMPORT SKIP-EXPORT)" t nil)

(autoload 'org-gcal-delete-at-point "org-gcal" "\
Delete entry at point to current calendar." t nil)

(autoload 'org-gcal-sync-tokens-clear "org-gcal" "\
Clear all Calendar API sync tokens.

Use this to force retrieving all events in ‘org-gcal-sync’ or
‘org-gcal-fetch’." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-gcal" '("org-gcal-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-gcal-autoloads.el ends here
