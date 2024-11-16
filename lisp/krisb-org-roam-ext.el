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
     (when (> level 0)
       (concat (org-roam-node-file-title node) " > "))
     (when (> level 1)
       (concat (string-join (org-roam-node-olp node) " > ")
               " > "))
     (if (> level 1)
         (propertize (org-roam-node-title node) 'face 'org-roam-title)
       (org-roam-node-title node)))))

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

;;; Provdie
(provide 'krisb-org-roam-ext)
;;; krisb-org-roam-ext.el ends here
