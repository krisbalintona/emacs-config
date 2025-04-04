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
(require 'org-roam-folgezettel)
(require 'transient)
(require 'org-expiry)

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
         (title (org-roam-node-title node))
         (file-title (org-roam-node-file-title node)))
    (concat (when address (format "(%s) " address))
            (when type (format "{%s} " type))
            title
            (unless (string= title file-title)
              (propertize (concat " (" file-title ")") 'face 'shadow)))))

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
              (save-restriction
                (widen)
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
                                 link-pt path)))))))))
        ;; Kill newly opened buffers
        (mapc #'kill-buffer (seq-difference (buffer-list) initial-buffer-list))))
    (message "Finished correcting %s links in %s!" corrected-counter dir)))

;;; Index numbering conversion
;; TODO 2024-12-03: Rename the functions below more appropriately.
;; TODO 2024-12-02: This function takes in a list rather than a single
;; letter/number.  Rename appropriately.
(defun krisb-org-roam-ext--convert-index-num-or-letter (num-or-letter)
  "Turn a number into a letter and a letter into a number.
NUM-OR-LETTER is either a number or a letter.  If it is a number, return
the letter in the alphabet at that place in the alphabet.  For instance,
\"2\" turns into \"b\".  If it is a letter, then return the place it is
in the alphabet as a string.  For instance, \"d\" returns \"4\"."
  (mapcar (lambda (item)
            (cond
             ;; If item is a number string, convert it to a letter.
             ((string-match-p "^[0-9]+$" item)
              (let ((num (string-to-number item)))
                (if (and (>= num 1) (<= num 26))
                    (char-to-string (+ ?a (- num 1)))
                  (error "Number out of range: %s" item))))
             ;; If item is a single letter, convert it to a number.
             ((string-match-p "^[a-zA-Z]$" item)
              (let ((char (string-to-char (downcase item))))
                (number-to-string (- char ?a -1))))
             ;; Otherwise, raise an error for invalid input.
             (t (error "Invalid input: %s" item))))
          num-or-letter))

(defun krisb-org-roam-ext-convert-index--next-num-or-letter (input)
  "Return the next number or letter of the alphabet as a string for INPUT.
- If INPUT is a number string (e.g., \"3\"), return the next
  number (\"4\").
- If INPUT is a single letter (e.g., \"a\"), return the next
  letter (\"b\").  Wrap from \"z\" to \"a\".
- Raise an error for invalid input."
  (cond
   ;; Case 1: Input is a number
   ((string-match-p "^[0-9]+$" input)
    (number-to-string (1+ (string-to-number input))))
   ;; Case 2: Input is a single letter
   ((string-match-p "^[a-zA-Z]$" input)
    (let* ((char (string-to-char input))
           (next-char (if (char-equal char ?z) ?a
                        (if (char-equal char ?Z) ?A
                          (1+ char)))))
      (char-to-string next-char)))
   ;; Case 3: Invalid input
   (t (error "Invalid input: %s" input))))

(defun krisb-org-roam-ext-convert-index-numbering (root-index note-index prefix &optional unique)
  "Convert NOTE-INDEX to a child index of ROOT-INDEX by modifying PREFIX.

This function modifies the NOTE-INDEX to ensure it represents a valid
child of ROOT-INDEX in a hierarchical numbering system (e.g.,
\"12.3\").

- PREFIX is the common prefix that both ROOT-INDEX and NOTE-INDEX
  share.  The function ensures that NOTE-INDEX starts with PREFIX and
  processes the remaining parts (referred to as suffix-parts).
- If the final part of ROOT-INDEX (referred to as prefix-parts) is a
  letter and the first part of the NOTE-INDEX suffix is also a letter,
  or if both are numbers, the first part of the suffix-parts is
  converted into the alternate type (letter or number) using
  `krisb-org-roam-ext--convert-index-num-or-letter'.
- If the first part of the suffix-parts is converted, the rest of the
  suffix-parts are also processed to maintain consistency.

If the optional argument UNIQUE is non-nil:
- The function ensures the modified NOTE-INDEX does not collide with any
  existing index numbers.  Existing indices are retrieved via
  `org-roam-ql' using the UNIQUE argument as a box predicate.
- If a collision is detected, the first part of the suffix-parts is
  incremented using
  `krisb-org-roam-ext-convert-index--next-num-or-letter' until a unique
  index is generated.

Steps:
1. Extract the suffix-parts of NOTE-INDEX by removing PREFIX.
2. Extract the prefix-parts of PARENT-INDEX.
3. If conversion is needed (based on matching types for the final prefix-parts
   and first suffix-parts), process suffix-parts using
   `krisb-org-roam-ext--convert-index-num-or-letter'.
4. Ensure uniqueness of the modified index if UNIQUE is provided.
5. Combine root-index and `string-join'd final-suffix-parts into a
   single string.

Arguments:
- PARENT-INDEX: The hierarchical index to which NOTE-INDEX will be a child.
- NOTE-INDEX: The original index to be modified.
- PREFIX: The common prefix shared between ROOT-INDEX and NOTE-INDEX.
- UNIQUE: If non-nil, ensures the generated index is unique by checking against
  existing indices.

Returns:
A string representing the new index, combining ROOT-INDEX and the converted
suffix of NOTE-INDEX.

Examples:
  ;; Convert NOTE-INDEX to a child of ROOT-INDEX without uniqueness
  (krisb-org-roam-ext-convert-index-numbering \"12\" \"12.3\" \"12.\")
  => \"12.3\"

  ;; Convert NOTE-INDEX to a child of ROOT-INDEX, ensuring uniqueness
  ;; (Assume \"12.3\" already exists in UNIQUE box)
  (krisb-org-roam-ext-convert-index-numbering \"12\" \"12.3\" \"12.\" \"unique-box\")
  => \"12.4\""
  ;; Ensure NOTE-INDEX starts with PREFIX
  (unless (string-prefix-p prefix note-index)
    (error "NOTE-INDEX does not start with PREFIX"))
  ;; Step 1: Get parts of NOTE-INDEX after removing PREFIX
  (let* ((suffix-parts (org-roam-folgezettel--index-split
                        (string-remove-prefix prefix note-index)))
         ;; Step 2: Get parts of ROOT-INDEX
         (prefix-parts (org-roam-folgezettel--index-split root-index))
         ;; Step 3: Check and convert first part of SUFFIX-PARTS if necessary
         (head-original (car suffix-parts))
         (head-converted head-original)
         (needs-conversion nil))
    ;; Determine if conversion is needed
    (setq head-converted
          (if (or (and (string-match-p "^[a-zA-Z]$" (car (last prefix-parts)))
                       (string-match-p "^[a-zA-Z]$" head-original))
                  (and (string-match-p "^[0-9]+$" (car (last prefix-parts)))
                       (string-match-p "^[0-9]+$" head-original)))
              (progn
                (setq needs-conversion t)
                (car (krisb-org-roam-ext--convert-index-num-or-letter (list head-original))))
            head-original))
    ;; Step 4: Convert the rest of SUFFIX-PARTS if necessary
    (let* ((converted-suffix-parts
            (if needs-conversion
                (krisb-org-roam-ext--convert-index-num-or-letter suffix-parts)
              suffix-parts))
           ;; Combine prefix-parts and converted suffix-parts
           (final-suffix-parts (cons head-converted (cdr converted-suffix-parts)))
           (new-index nil))
      ;; Step 5: Ensure uniqueness if requested
      (when unique
        (setq new-index
              (concat root-index (string-join final-suffix-parts)))
        (while (member new-index
                       (mapcar #'org-roam-folgezettel-list--retrieve-index
                               (org-roam-ql-nodes `(box ,unique))))
          (setq head-converted
                (krisb-org-roam-ext-convert-index--next-num-or-letter head-converted))
          (setq final-suffix-parts
                (cons head-converted (cdr converted-suffix-parts)))
          (setq new-index
                (concat root-index (string-join final-suffix-parts)))))
      ;; Step 6: Combine prefix-parts and final-suffix-parts into a single string
      (concat root-index (string-join final-suffix-parts)))))

;;; Bespoke transient menu for creating and managing org-roam heading node properties
(defun krisb-org-roam-ext-set-roam-place ()
  "Set the ROAM_PLACE property in the current heading."
  (interactive)
  (let ((place (org-read-property-value "ROAM_PLACE")))
    (org-set-property "ROAM_PLACE" place)
    (message "ROAM_PLACE set to: %s" place)))

(defun krisb-org-roam-ext-set-roam-type ()
  "Set the ROAM_TYPE property in the current heading."
  (interactive)
  (let ((place (org-read-property-value "ROAM_TYPE")))
    (org-set-property "ROAM_TYPE" place)
    (message "ROAM_TYPE set to: %s" place)))

(defun krisb-org-roam-ext-toggle-roam-exclude ()
  "Toggle the ROAM_EXCLUDE property in the current heading."
  (interactive)
  (org-entry-put nil "ROAM_EXCLUDE"
                 (pcase (org-entry-get nil "ROAM_EXCLUDE" nil t)
                   ("t" "nil")
                   ("nil" "t"))))

(defun krisb-org-roam-ext-set-roam-box ()
  "Set the ROAM_BOX property in the current heading."
  (interactive)
  (let ((box (org-read-property-value "ROAM_BOX")))
    (org-set-property "ROAM_BOX" box)
    (message "ROAM_BOX set to: %s" box)))

(defun krisb-org-roam-ext-set-roam-person ()
  "Set the ROAM_PERSON property in the current heading."
  (interactive)
  (let ((person (org-read-property-value "ROAM_PERSON")))
    (org-set-property "ROAM_PERSON" person)
    (message "ROAM_PERSON set to: %s" person)))

(defun krisb-org-roam-ext-set-roam-source ()
  "Set the ROAM_SOURCE property in the current heading."
  (interactive)
  (let ((source (org-read-property-value "ROAM_SOURCE")))
    (org-set-property "ROAM_SOURCE" source)
    (message "ROAM_SOURCE set to: %s" source)))

(defun krisb-org-roam-ext-set-roam-context ()
  "Set the ROAM_PROPERTY property in the current heading."
  (interactive)
  (let ((context (org-read-property-value "ROAM_CONTEXT")))
    (org-set-property "ROAM_CONTEXT" context)
    (message "ROAM_CONTEXT set to: %s" context)))

(defun krisb-org-roam-ext-toggle-properties-visibility ()
  "Toggle the visibility of the PROPERTIES drawer of the Org heading at point."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((drawer (save-excursion
                    (when (re-search-forward org-property-drawer-re (save-excursion (outline-next-heading)) t)
                      (match-beginning 0)))))
      (if drawer
          (progn
            (goto-char drawer)
            (org-fold-hide-drawer-toggle (not (org-at-drawer-p))))
        (message "No PROPERTIES drawer found at this heading.")))))

(defun krisb-org-roam-ext-toggle-heading-content-visibility ()
  "Toggle the visibility of a heading's contents."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (and (org-at-heading-p)
             (outline-invisible-p (line-end-position))))
      (org-fold-show-entry)
    (org-fold-hide-entry)))

(defmacro krisb-org-roam-ext-transient--dyn-roam-property-description (desc prop-name &optional desc-alt)
  "Macro of dynamic descriptions for setting org-roam-specific properties.
DESC is the description and PROP-NAME is the property name being checked
for.  If there does not exist a value corresponding to the property
named PROP-NAME, then the description of the transient command will be
DESC.  If there is a value, then return DESC with the value of that
property propertized and appended within parentheses.

If DESC-ALT is provided, use that string instead as the base string when
there exists a value for the property named PROP-NAME."
  `(lambda ()
     (let ((prop (org-entry-get (point) ,prop-name nil t)))
       (concat (if (and prop ,desc-alt)
                   ,desc-alt
                 ,desc)
               (when prop
                 (concat " ("
                         (propertize prop 'face 'transient-value)
                         ")"))))))

(transient-define-prefix krisb-org-roam-ext-properties-transient ()
  "Transient menu for setting org-roam properties."
  ["Generic properties"
   (org-id-get-create
    :key "a"
    :transient t
    :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Add ID" "ID" "Modify ID"))
   (org-expiry-insert-created
    :key "C"
    :transient t
    :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Add CREATED" "CREATED"))]
  ["Roam-specific properties"
   ["All nodes"
    (krisb-org-roam-ext-toggle-roam-exclude
     :key "e"
     :transient t
     :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Toggle ROAM_EXCLUDE" "ROAM_EXCLUDE"))
    (krisb-org-roam-ext-set-roam-box
     :key "b"
     :transient t
     :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_BOX" "ROAM_BOX"))]
   ["Main nodes"
    (krisb-org-roam-ext-set-roam-type
     :key "t"
     :transient t
     :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_TYPE" "ROAM_TYPE"))
    (krisb-org-roam-ext-set-roam-source
     :key "s"
     :transient t
     :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_SOURCE" "ROAM_SOURCE"))
    (krisb-org-roam-ext-set-roam-context
     :key "c"
     :transient t
     :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_CONTEXT" "ROAM_CONTEXT"))
    (krisb-org-roam-ext-set-roam-person
     :key "r"
     :transient t
     :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_PERSON" "ROAM_PERSON"))
    (krisb-org-roam-ext-set-roam-place
     :key "p"
     :transient t
     :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_PLACE" "ROAM_PLACE"))]]
  [["Navigation"
    ("C-u" "Up heading" org-up-heading :transient t)
    ("C-p" "Next heading" org-previous-visible-heading :transient t)
    ("C-n" "Next heading" org-next-visible-heading :transient t)
    ("C-f" "Forward heading same level" org-forward-heading-same-level :transient t)
    ("C-b" "Backward heading same level" org-backward-heading-same-level :transient t)]
   ["Visibility"
    ("M-t" "Toggle visibility of heading contents" krisb-org-roam-ext-toggle-heading-content-visibility :transient t)
    ("M-T" "Toggle visibility of properties drawer" krisb-org-roam-ext-toggle-properties-visibility :transient t)]])

;;; Provide
(provide 'krisb-org-roam-ext)
;;; krisb-org-roam-ext.el ends here
