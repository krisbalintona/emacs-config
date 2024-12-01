;;; krisb-org-roam-ext.el --- My bespoke org-roam extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: tools, convenience

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

;; Org-roam stuff for my own Zettelkasten org-roam workflow.

;;; Code:
(require 'org-roam-node)

;;; Custom org-roam-node accessors
;;;; Normal values
;;;###autoload
(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  "Return the hierarchy of NODE.
The hierarchy is the node title prepended with the file and outline path
if it is a headline.  These parts are separated by \" > \"."
  (when-let* ((level (org-roam-node-level node)))
    (concat
     (org-roam-node-title node)
     (when (> level 1)
       (concat
        (propertize " (" 'face 'shadow)
        (propertize (string-join (nconc (list (org-roam-node-file-title node)) (org-roam-node-olp node))
                                 " > ")
                    'face 'shadow)
        (propertize ")" 'face 'shadow))))))

;;;###autoload
(cl-defmethod org-roam-node-directories ((node org-roam-node))
  "Access the address of NODE.
The address of a node is the value of the \"ROAM_PLACE\" property."
  (when-let* ((dirs (file-name-directory
                     (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (car (split-string dirs "/"))))

;;;###autoload
(cl-defmethod org-roam-node-address ((node org-roam-node))
  "Access the address of NODE.
The address of a node is the value of the \"ROAM_PLACE\" property."
  (let ((address (cdr (assoc "ROAM_PLACE" (org-roam-node-properties node) #'string-equal))))
    (when (and address (not (string-empty-p address)))
      (string-trim address))))

;;;###autoload
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Access the type of NODE.
The address of a node is the value of the \"ROAM_TYPE\" property."
  (let ((index-number (cdr (assoc "ROAM_TYPE" (org-roam-node-properties node) #'string-equal))))
    (when (and index-number (not (string-empty-p index-number)))
      (string-trim index-number))))

;;;###autoload
(cl-defmethod org-roam-node-person ((node org-roam-node))
  "Access the person associated with NODE.
The address of a node is the value of the \"ROAM_PERSON\" property."
  (let ((person (cdr (assoc "ROAM_PERSON" (org-roam-node-properties node) #'string-equal))))
    (when (and person (not (string-empty-p person)))
      (string-trim person))))

;;;; Values formatted values for display templates
;;;###autoload
(cl-defmethod org-roam-node-directories-display-template ((node org-roam-node))
  "Returned formatted directory path of NODE.
Formats the value of `org-roam-node-directories' on NODE."
  (when-let* ((path (org-roam-node-directories node)))
    (propertize (concat "/" path " ") 'face 'shadow)))

;;;###autoload
(cl-defmethod org-roam-node-person-display-template ((node org-roam-node))
  "Returned formatted person of NODE.
Formats the value of `org-roam-node-person' on NODE."
  (when-let* ((person (org-roam-node-person node)))
    (propertize (concat "@" person " ") 'face 'font-lock-keyword-face)))

;;;###autoload
(cl-defmethod org-roam-node-type-display-template ((node org-roam-node))
  "Returned formatted type of NODE.
Formats the value of `org-roam-node-type' on NODE."
  (when-let* ((type (org-roam-node-type node)))
    (propertize (concat "&" type " ") 'face 'font-lock-doc-face)))

;;;###autoload
(cl-defmethod org-roam-node-address-display-template ((node org-roam-node))
  "Returned formatted address of NODE.
Formats the value of `org-roam-node-address' on NODE."
  (when-let* ((address (org-roam-node-address node)))
    (propertize (concat address " ") 'face 'shadow)))

;;; Custom org-roam node formatter function
;; We use this function as the value of `org-roam-node-formatter' instead of a
;; display template string because we want our inserted description to be
;; dependent on the existence or non-existence of certain values, e.g., the node
;; address
(cl-defmethod krisb-org-roam-node-formatter ((node org-roam-node))
  "Formatted output of an org-roam NODE.
This is meant to be used as the value of `org-roam-node-formatter',
which modifies the description of org-roam nodes from
`org-roam-node-insert'."
  (let* ((address (org-roam-node-address node))
         (type (org-roam-node-type node))
         (hierarchy (org-roam-node-hierarchy node)))
    (concat (when address (format "(%s) " address))
            (when type (format "{%s} " type))
            hierarchy)))

;;; Update link descriptions
(defun krisb-org-roam-ext-lint-descriptions (dir)
  "Correct org-roam link descriptions in DIR.
If called interactively, DIR defaults to `org-roam-directory'.  When
called with a prefix argument, prompt the user for the directory in
which link swill be corrected.

This function does the following operations in this order:
1. List all org-roam nodes.
2. Act (steps 3-6) on each node in turn.
3. Retrieve backlinks and correctly formatted node text
   (`org-roam-node-formatted').
4. For each backlink to node, in reverse-point order, update their link
   description if it does not match the correctly formatted node text.
5. Kill any newly opened buffers."
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Correct descriptions in directory: "
                                              (expand-file-name org-roam-directory))
                       org-roam-directory)))
  (let ((save-silently t)
        (node-list (seq-filter
                    (lambda (node)
                      (file-in-directory-p (org-roam-node-file node) dir))
                    (org-roam-node-list)))
        (corrected-counter 0))
    (message "Correcting all links in %s... (this may take a while)" dir)
    ;; Before iterating on node-list, must first ensure that the database file
    ;; correctly reflects buffer contents.  This is because the location data of
    ;; backlinks (their points) must be accurate, otherwise new link
    ;; descriptions might not change the correct region of the buffer.  To do
    ;; this, we first save all files that are visiting a node in node-list and
    ;; update the org-roam database
    (save-some-buffers nil (lambda () (member (buffer-file-name)
                                              (mapcar #'org-roam-node-file node-list))))
    (org-roam-db-sync)
    (dolist (node node-list)
      (let* ((node-formatted (org-roam-node-formatted node))
             (backlinks (org-roam-backlinks-get node :unique t))
             (initial-buffer-list (buffer-list)))
        (message "Correcting links to %s" node-formatted)
        ;; We must iterate through backlinks in the reverse order of their
        ;; points (i.e. later links come first) so if any of those links are
        ;; changed the backlink points in any earlier links aren't disrupted
        (dolist (backlink (reverse (sort backlinks :key #'org-roam-backlink-point)))
          (let ((path (org-roam-node-file
                       (org-roam-backlink-source-node backlink))))
            (with-current-buffer (find-file-noselect path)
              (save-excursion
                (let ((link-pt (org-roam-backlink-point backlink)))
                  (goto-char link-pt)
                  (let ((element (org-element-context)))
                    (if (eq (org-element-type element) 'link)
                        (let ((begin (org-element-property :contents-begin element))
                              (end (org-element-property :contents-end element)))
                          (when (and begin end)
                            (unless (string= (buffer-substring-no-properties begin end)
                                             node-formatted)
                              (goto-char begin)
                              (delete-region begin end)
                              (insert node-formatted)
                              (setq corrected-counter (1+ corrected-counter))
                              ;; It is important to save the buffer and ensure
                              ;; it is updated in the org-roam database such
                              ;; that later nodes in node-list that have
                              ;; backlinks in buffers with modified links
                              ;; correctly report the location (point) of those
                              ;; links
                              ;; TODO 2024-11-30: Right now we save the file and
                              ;; update the database for every change in a link.
                              ;; This command could be much more performant if
                              ;; we only do this once after we update the links
                              ;; in that file.  This would, however, require
                              ;; iterating per-file rather than per-node, which
                              ;; (I think) would make the logic of this function
                              ;; less intuitive.
                              (save-buffer)
                              (unless org-roam-db-autosync-mode
                                (org-roam-db-update-file path)))))
                      (message "[krisb-org-roam-ext-lint-descriptions] Element at %s in %s not a link!"
                               link-pt path))))))))
        ;; Kill newly opened buffers
        (mapc #'kill-buffer (seq-difference (buffer-list) initial-buffer-list))))
    (message "Finished correcting %s links in %s!" corrected-counter dir)))

;;; Provide
(provide 'krisb-org-roam-ext)
;;; krisb-org-roam-ext.el ends here
