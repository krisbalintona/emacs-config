;; -*- lexical-binding: t; -*-

;;; krisb-org-ext.el --- Org-mode extensions         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: tools

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

;; Bespoke extensions to org-mode.

;;; Code:
(require 'org)

;;; Blank lines between headings and their contents
;; Ensure that there are blank lines before and after org heading. Use with
;; `universal-argument' to apply to whole buffer. Taken from
;; https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#ensure-blank-lines-between-headings-and-before-contents
;;;###autoload
(defun krisb-org-ext-add-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and their contents.
With PREFIX, operate on whole buffer.  Ensures that blank lines exist
after each heading's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))

;;; Automate creation of CUSTOM_ID
;;;###autoload
(defun krisb-org-ext-create-custom-id ()
  "Get the CUSTOM_ID of the current entry.
If the entry already has a CUSTOM_ID, return it as-is, else create a new
one.

This function is a copy of `denote-link-ol-get-id'."
  (interactive nil org-mode)
  (let* ((pos (point))
         (id (org-entry-get pos "CUSTOM_ID")))
    (if (and (stringp id) (string-match-p "\\S-" id))
        id
      (setq id (org-id-new "h"))
      (org-entry-put pos "CUSTOM_ID" id)
      id)))

;;; Eldoc backend for footnote content
;;;###autoload
(defun krisb-org-ext-eldoc-footnote (callback &rest _rest)
  "Show formatted footnote content for footnote reference at point.
Read `eldoc-documentation-functions' for an explanation of CALLBACK and
_REST."
  (when-let*
      ((reference (org-footnote-at-reference-p))
       (label (nth 0 (org-footnote-at-reference-p)))
       (definition (org-footnote-get-definition label))
       (footnote-content (buffer-substring (nth 1 definition)
                                           (nth 2 definition))))
    (funcall callback footnote-content)))

;;; Provide
(provide 'krisb-org-ext)
;;; krisb-org-ext.el ends here
