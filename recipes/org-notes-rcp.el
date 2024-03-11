;;; org-notes-rcp.el --- Taking notes in org-mode    -*- lexical-binding: t; -*-

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

;; Everything directly used for my note-taking needs.

;;; Code:
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Denote
;;;;; This
(use-package denote
  :vc (:url "https://github.com/protesilaos/denote.git"
            :rev :newest)
  :autoload 'denote-directory-files
  :hook ((dired-mode . denote-dired-mode)
         (denote-dired-mode . toggle-truncate-lines)
         (before-save . kb/denote-insert-identifier-maybe)
         (after-save . kb/denote-auto-rename)
         (kb/themes . kb/themes-setup-denote-faces))
  :general (kb/note-keys
             "f" 'denote-open-or-create
             "i" 'denote-insert-link
             "ta" 'denote-keywords-add
             "tr" 'denote-keywords-remove)
  :custom
  (denote-directory kb/notes-dir)
  (denote-known-keywords nil)
  (denote-prompts '(subdirectory title keywords signature template))
  (denote-org-front-matter "#+title: %s
#+date: %s
#+filetags: %s
#+identifier: %s
")
  (denote-templates
   '((default . "\n")
     (paper . "#+latex_class: mla
#+cite_export: biblatex mla-new
#+professor:
#+course:
#+export_file_name:

* Potential titles

* 1 Draft                                                     :export:ignore:

* Works Cited                                                 :ignore:export:

#+begin_export LaTeX
\\newpage
\center
#+end_export

#+print_bibliography:")
     (buoy . "* Responses

* Biographical information

+ Buoy nominations :: tk
+ Instagram handle :: tk

* Potential titles

1.

* 1 Draft                                                     :ignore:export:
")))
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  (denote-rename-buffer-format "%s %t")
  :init
  ;; Rename denote note. Meant to be added to `after-save-hook'
  (defun kb/denote-auto-rename ()
    "Auto rename denote file."
    (when-let ((f (buffer-file-name)))
      (when (and (file-in-directory-p f denote-directory)
                 (denote-filename-is-note-p f))
        (denote-rename-file-using-front-matter f :auto-confirm))))

  (require 's)
  ;; Camel cased keywords
  (defun kb/denote-sluggify-keyword (str)
    "Sluggify STR while joining separate words.
My version camelCases keywords."
    (s-lower-camel-case
     (denote--slug-hyphenate (denote--slug-no-punct str))))
  (setq denote-file-name-slug-functions
        '((title . denote-sluggify-title)
          (signature . denote-sluggify-signature)
          (keyword . kb/denote-sluggify-keyword)))
  :config
  (denote-rename-buffer-mode)
  (denote-menu-bar-mode -1)

  ;; HACK 2024-03-03: Temporary fix for org-capture creating CUSTOM_ID
  ;; properties. See related issue:
  ;; https://github.com/protesilaos/denote/issues/267
  (advice-add 'org-capture :around
              (lambda (orig-fun &rest args)
                (let ((denote-org-store-link-to-heading nil))
                  (apply orig-fun args))))

  ;; Set face parameters
  (defun kb/themes-setup-denote-faces ()
    "Set up denote faces."
    (modus-themes-with-colors
      (set-face-attribute 'denote-faces-signature nil :weight 'bold)
      (set-face-attribute 'denote-faces-title nil :weight 'semibold :foreground cyan-cooler)
      (set-face-attribute 'denote-faces-keywords nil :foreground keyword :slant 'italic)
      (set-face-attribute 'denote-faces-date nil :foreground 'unspecified :inherit 'shadow)))
  (kb/themes-setup-denote-faces))       ; Immediately call

;;;;; Standardizing note front-matter
(with-eval-after-load 'denote
  (defun kb/org-set-keyword (keyword value)
    ;; Got lazy and copied `org-roam-set-keyword'
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward (concat "^#\\+" keyword ":\\(.*\\)") (point-max) t)
            (if (string-blank-p value)
                (kill-whole-line)
              (replace-match (concat " " value) 'fixedcase nil nil 1))
          ;; Don't think this is necessary, and it'd be too much code
          ;; to copy if it were
          ;; (org-roam-end-of-meta-data 'drawers)
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line))
          (insert "#+" keyword ": " value "\n")))))

  (defun kb/note-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
        (denote-trim-whitespace
         (buffer-substring-no-properties
          (match-beginning 1)
          (match-end 1))))))

  (defun kb/denote-insert-identifier-maybe ()
    (when (and (buffer-file-name) (denote-file-is-note-p (buffer-file-name)))
      (cond
       ;; ID doesn't exist
       ((not (kb/note-buffer-prop-get "identifier"))
        (save-excursion
          (beginning-of-buffer)
          ;; Move cursor until after the first of following
          ;; properties exists: filetags, date, or title
          (while (and (not (eobp))
                      (cond
                       ((kb/note-buffer-prop-get "filetags")
                        (re-search-forward (rx bol "#+"
                                               (or "F" "f")
                                               (or "I" "i")
                                               (or "L" "l")
                                               (or "E" "e")
                                               (or "T" "t")
                                               (or "A" "a")
                                               (or "G" "g")
                                               (or "S" "s")
                                               ":")
                                           (point-max) t))
                       ((kb/note-buffer-prop-get "date")
                        (re-search-forward (rx bol "#+"
                                               (or "D" "d")
                                               (or "A" "a")
                                               (or "T" "t")
                                               (or "E" "e")
                                               ":")
                                           (point-max) t))
                       ((kb/note-buffer-prop-get "title")
                        (re-search-forward (rx bol "#+"
                                               (or "T" "t")
                                               (or "I" "i")
                                               (or "T" "t")
                                               (or "L" "l")
                                               (or "E" "e")
                                               ":")
                                           (point-max) t))))
            (cond
             ((save-excursion (end-of-line) (eobp))
              (end-of-line)
              (insert "\n"))
             (t
              (forward-line)
              (beginning-of-line))))
          (insert "#+identifier: " (denote-retrieve-filename-identifier (buffer-file-name)) "\n")))
       ;; When file name ID and identifier property value differ
       ((not (string= (denote-retrieve-filename-identifier (buffer-file-name))
                      (kb/note-buffer-prop-get "identifier")))
        (kb/org-set-keyword "identifier" (denote-retrieve-filename-identifier (buffer-file-name)))))))
  (defun kb/denote-rearrange-keywords-maybe ()
    (let* ((f (buffer-file-name))
           (file-type (denote-filetype-heuristics f))
           (cur-keywords (seq-uniq (denote-retrieve-keywords-value f file-type)))
           (sorted-keywords (denote-keywords-sort (copy-list cur-keywords))))
      (denote--rewrite-keywords f sorted-keywords file-type)
      ;; Add empty filetags property if one isn't already present
      (unless (kb/note-buffer-prop-get "filetags")
        (beginning-of-buffer)
        (while (and (not (eobp))
                    (cond
                     ((kb/note-buffer-prop-get "date")
                      (re-search-forward (rx bol "#+"
                                             (or "D" "d")
                                             (or "A" "a")
                                             (or "T" "t")
                                             (or "E" "e")
                                             ":")
                                         (point-max) t))
                     ((kb/note-buffer-prop-get "title")
                      (re-search-forward (rx bol "#+"
                                             (or "T" "t")
                                             (or "I" "i")
                                             (or "T" "t")
                                             (or "L" "l")
                                             (or "E" "e")
                                             ":")
                                         (point-max) t))))
          (cond
           ((save-excursion (end-of-line) (eobp))
            (end-of-line)
            (insert "\n"))
           (t
            (forward-line)
            (beginning-of-line))))
        (insert "#+filetags:\n"))))
  (defun kb/denote-ensure-title-space ()
    (save-excursion
      (beginning-of-buffer)
      (if-let ((end-of-title-keyword
                (re-search-forward (rx bol "#+"
                                       (or "T" "t")
                                       (or "I" "i")
                                       (or "T" "t")
                                       (or "L" "l")
                                       (or "E" "e")
                                       ":")
                                   (point-max) t)))
          (progn
            (goto-char end-of-title-keyword)
            (just-one-space))
        (error "No title in %s!" (buffer-file-name)))))
  (defun kb/denote-standardize-front-matter ()
    (interactive)
    (require 'denote)
    (save-mark-and-excursion
      (dolist (file (denote-directory-files-matching-regexp (rx (literal ".org") eol)))
        ;; Export all the files
        (with-current-buffer (find-file-noselect file)
          (read-only-mode -1)
          (save-restriction
            (widen)
            (kb/denote-insert-identifier-maybe)
            (kb/denote-rearrange-keywords-maybe)
            (kb/denote-ensure-title-space)
            (kb/format-buffer-indentation))
          (denote-rename-file-using-front-matter file t)

          (unless (member (get-buffer (buffer-name)) (buffer-list)) ; Kill buffer unless it already exists
            (kill-buffer)))))))

;;;; Denote-explore
;;;;; This
;; Useful Denote utilities
(use-package denote-explore
  :after denote
  ;; NOTE 2024-02-28: Don't forget to install the required dependencies required
  ;; for my chosen `denote-explore-network-format'
  :ensure-system-package ((dot . graphviz)
                          (R . r))
  :custom
  (denote-explore-network-directory     ; Have to end path in slash
   (no-littering-expand-var-file-name "denote-explore/"))
  (denote-explore-network-format 'd3.js)
  (denote-explore-network-keywords-ignore '("archive")))

;;;;; Update link descriptions
(with-eval-after-load 'org
  (defun kb/denote-explore-update-link-descriptions (confirmp)
    "Recreate denote link descriptions in the current buffer.
If called with CONFIMP, then prompt user to confirm a replacement. When
interactively called, CONFIRMP is non-nil by default, flipping the value
with prefix-arg."
    (interactive (list (not current-prefix-arg)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (denote-org-extras--get-link-type-regexp 'denote) nil :no-error)
        (condition-case err
            (save-match-data
              (let* ((link-beg (match-beginning 0))
                     (link-end (match-end 0))
                     (s (match-string-no-properties 0))
                     (link (with-temp-buffer
                             (let ((org-inhibit-startup nil))
                               (insert s)
                               (org-mode)
                               (goto-char (point-min))
                               (org-element-link-parser))))
                     (type (org-element-property :type link))
                     (path (org-element-property :path link))
                     (file (denote-get-path-by-id (car (string-split path "::"))))
                     (heading-custom-id (cadr (string-split path "::")))
                     (new-link-text
                      ;; TODO 2024-03-04: This is a brittle way to create the
                      ;; link. Changes to Denote might break this. Avoid that.
                      (if (and denote-org-store-link-to-heading heading-custom-id)
                          (format "[[denote:%s::#%s][%s]]"
                                  (denote-retrieve-filename-identifier file)
                                  (string-remove-prefix "#" heading-custom-id)
                                  (concat (denote--link-get-description file)
                                          "::"
                                          (save-excursion
                                            (with-current-buffer (find-file-noselect file)
                                              (org-link-search heading-custom-id)
                                              (org-link-display-format
                                               (denote-link-ol-get-heading))))))
                        (format "[[denote:%s][%s]]"
                                (denote-retrieve-filename-identifier file)
                                (denote--link-get-description file))))
                     (current-link-text (buffer-substring link-beg link-end)))
                (when (and (not (string= (substring-no-properties current-link-text) new-link-text))
                           (or (not confirmp)
                               (yes-or-no-p (concat "Replace this link? " current-link-text))))
                  (goto-char link-beg)
                  (delete-region link-beg link-end)
                  (insert new-link-text))))
          (error (message "[kb/denote-explore-update-link-descriptions] Error encountered:  %s"
                          (error-message-string err))))))
    (message "Corrected links in %s"
             (propertize (denote-retrieve-front-matter-title-value
                          (buffer-file-name)
                          (denote-filetype-heuristics (buffer-file-name)))
                         'face 'denote-faces-title))))

;;;; Denote-menu
;;;;; Itself
(use-package denote-menu
  :general
  (kb/note-keys
    "m" 'denote-menu-list-notes
    "r" 'kb/denote-menu-set-signature-interactively)
  (:keymaps 'denote-menu-mode-map
            "|" 'denote-menu-clear-filters
            "/r" 'denote-menu-filter
            "//" 'denote-menu-filter
            "/k" 'denote-menu-filter-by-keyword
            "/e" 'kb/denote-menu-edit-filter-presets
            "/E" 'kb/denote-menu-edit-filter
            "E" 'denote-menu-export-to-dired
            "RET" 'kb/denote-menu-goto-note
            "o" 'kb/denote-menu-goto-note-other-window
            "C-o" 'kb/denote-menu-display-note-other-window
            "r" 'kb/denote-menu-set-signature-interactively
            "R" 'kb/denote-menu-set-signature)
  :custom
  (denote-menu-show-file-signature t)
  (denote-menu-show-file-type nil)
  (denote-menu-signature-column-width
   (+ 6 (cl-loop for file in (denote-directory-files)
                 maximize (length (denote-retrieve-filename-signature file)))))
  (denote-menu-title-column-width 120)
  (denote-menu-initial-regex (first kb/denote-menu-filter-presets))
  :preface
  (defvar kb/denote-menu-filter-presets
    '("zettels/[^z-a]*" "bib/[^z-a]*")
    "The common filters I use."))

;;;;; Custom denote-menu functions and commands
(with-eval-after-load 'denote-menu
  (defun kb/denote-menu-edit-filter ()
    "Edit the currently existing filter."
    (interactive)
    (setq denote-menu-current-regex
          (read-from-minibuffer "Filter regex: " denote-menu-current-regex))
    (revert-buffer))

  (defun kb/denote-menu-edit-filter-presets ()
    "Edit the currently existing filter."
    (interactive)
    (setq denote-menu-current-regex
          (completing-read "Filter preset: "
                           (remove denote-menu-current-regex kb/denote-menu-filter-presets)))
    (revert-buffer))

  (defun kb/denote-menu--get-path-at-point ()
    "Get the file path of the note at point."
    (let* ((tab-id (tabulated-list-get-id))
           (denote-id (first (string-split tab-id "-")))
           (path (denote-get-path-by-id denote-id)))
      path))

  (defun kb/denote-menu-goto-note ()
    "Jump to the note corresponding to the entry at point."
    (interactive)
    (find-file (kb/denote-menu--get-path-at-point)))

  (defun kb/denote-menu-goto-note-other-window ()
    "Open in another window the note corresponding to the entry at point."
    (interactive)
    (find-file-other-window (kb/denote-menu--get-path-at-point)))

  (defun kb/denote-menu-display-note-other-window ()
    "Just display the current note in another window."
    (interactive)
    (display-buffer (find-file-noselect (kb/denote-menu--get-path-at-point)) t))

  (defun kb/denote-menu-set-signature (path new-sig)
    "Set the note at point's signature."
    (interactive (list (kb/denote-menu--get-path-at-point) nil))
    (let* ((path (or path (kb/denote-menu--get-path-at-point)))
           (file-type (denote-filetype-heuristics path))
           (title (denote-retrieve-title-value path file-type))
           (initial-sig (denote-retrieve-filename-signature path))
           (new-sig (or new-sig
                        (denote-signature-prompt
                         (unless (string= initial-sig "000") initial-sig) ; 000 is the "unsorted" signature for me
                         "Choose new signature")))
           (keywords
            (denote-retrieve-front-matter-keywords-value path file-type))
           (denote-rename-no-confirm t)) ; Want it automatic
      (denote-rename-file path title keywords new-sig))))

;;;;; Support for bespoke numbering system
(with-eval-after-load 'denote-menu
  (defun kb/denote-menu--signature-elements-head-tail (group)
    "Take a signature GROUP and return a cons.
The car of this cons will be the \"front\" portion of the signature,
while the cdr of this cons will be the remaining portion of the
signature.

Right now, a \"signature portion\" is delimited by:
- The \"=\" character.
- A change from a number to letter.
- A change from a letter to number."
    (let (head tail)
      (save-match-data
        ;; HACK 2024-03-04: I hardcode "=" as an additional delimiter of
        ;; signature portions
        (if (string-match (if (s-numeric-p (substring group 0 1))
                              (rx (any alpha)) ; Numbered index head
                            (rx (any digit)))  ; Alphabet index head
                          group)
            (setq head (substring group 0 (match-beginning 0))
                  tail (string-remove-prefix "=" (substring group (match-beginning 0))))
          (setq head group
                tail nil)))
      (cons head tail)))

  (defun kb/denote-menu--signature-decompose-into-groups (sig)
    "Decompose SIG into groups."
    (when sig
      (if (string-empty-p sig)
          nil
        (string-split sig "="))))

  (defun kb/denote-menu--signature-decompose-elements-from-group (group)
    "Take a GROUP and decompose it into its elements.
Uses `kb/denote-menu--signature-elements-head-tail'."
    (let* ((parts (kb/denote-menu--signature-elements-head-tail group))
           (head (car parts))
           (tail (cdr parts)))
      (if tail
          (flatten-list
           (list head
                 (kb/denote-menu--signature-decompose-elements-from-group tail)))
        group)))

  ;; REVIEW 2024-03-04: Consider changing to take in files rather than
  ;; signatures? If so, make sure I alter my advice for
  ;; `denote-sort-signature-lessp'.
  (defun kb/denote-menu--signature-group-lessp (group1 group2)
    "Compare the ordering of two groups.
Returns t if GROUP1 should be sorted before GROUP2, nil otherwise."
    (when (and group1
               group2
               (or (s-contains-p "=" group1) (s-contains-p "=" group2)))
      (error "[kb/denote-menu--signature-group-lessp] Does not accept strings with \"=\""))
    (if (and group1 group2)
        (let* ((elements1 (kb/denote-menu--signature-elements-head-tail group1))
               (elements2 (kb/denote-menu--signature-elements-head-tail group2))
               (head1 (car elements1))
               (head2 (car elements2))
               (tail1 (cdr elements1))
               (tail2 (cdr elements2))
               ;; HACK 2024-03-03: Right now, this treats uppercase and
               ;; lowercase as the same, as well as ignores the difference
               ;; between, e.g., "a" and "aa"
               (index1 (string-to-number head1 16))
               (index2 (string-to-number head2 16)))
          (cond
           ;; Group1 is earlier in order than group2
           ((< index1 index2) t)
           ;; Group2 is later than group2
           ((> index1 index2) nil)
           ;; Group1 has no tail while group2 has a tail, so it's later than
           ;; group2
           ((and (not tail1) tail2) t)
           ;; Group1 has a tail while group2 has no tail, so it's earlier than
           ;; group2
           ((and tail1 (not tail2)) nil)
           ;; Neither group2 nor group2 have a tail, and their indexes must be
           ;; equal, so they must have identical signatures. So do something
           ;; with it now. (Returning nil seems to put the oldest earlier, so we
           ;; do that.)
           ((and (not tail1) (not tail2)) nil)
           ;; Their indices are equal, and they still have a tail, so process
           ;; those tails next
           ((= index1 index2)
            (kb/denote-menu--signature-group-lessp tail1 tail2))))
      ;; When one or both of group1 and group2 are not supplied
      (cond
       ;; If neither are supplied, then use `string-collate-lessp'
       ((not (or group1 group2))
        (string-collate-lessp group1 group2))
       ;; If group1 is present but not group2, then return true so that group1
       ;; can be earlier in the list
       ((and group1 (not group2))
        t)
       ;; If group2 is present but not group1, then return nil to put group2
       ;; earlier
       ((and (not group1) group2)
        nil))))

  (defun kb/denote-menu--signature-lessp (sig1 sig2)
    "Compare two strings based on my signature sorting rules.
Returns t if SIG1 should be sorted before SIG2, nil otherwise.

This function splits SIG1 and SIG2 into indexical groups with
`kb/denote-menu--signature-decompose-into-groups' and compares the first
group of each. If SIG1 is not definitively before SIG2, then recursively
call this function on the remaining portion of the signature."
    (let ((groups1 (kb/denote-menu--signature-decompose-into-groups sig1))
          (groups2 (kb/denote-menu--signature-decompose-into-groups sig2)))
      (cond
       ;; Return t when: if sig1's groups have so far been after sig2's, but
       ;; sig2 has more groups while sig1 does not, then this means sig2
       ;; ultimately goes after sig1
       ((and (not sig1) sig2) t)
       ;; Return nil when: if all of sig1's groups go after sig2's groups, then
       ;; sig2 is after sig1
       ((not (and sig1 sig2)) nil)
       ;; When the car of groups1 and groups2 are the same, then recursively
       ;; call this function on the remaining portion of the signature
       ((string= (car groups1) (car groups2))
        (let ((remaining-groups1 (string-join (cdr groups1) "="))
              (remaining-groups2 (string-join (cdr groups2) "=")))
          (kb/denote-menu--signature-lessp (unless (string-empty-p remaining-groups1) remaining-groups1)
                                           (unless (string-empty-p remaining-groups2) remaining-groups2))))
       (t
        (kb/denote-menu--signature-group-lessp (pop groups1) (pop groups2))))))
  (advice-add 'denote-sort-signature-lessp
              :override (lambda (f1 f2)
                          (kb/denote-menu--signature-lessp (denote-retrieve-filename-signature f1)
                                                           (denote-retrieve-filename-signature f2))))

  (defun kb/denote-menu--signature-sorter (a b)
    "Tabulated-list sorter for signatures A and B.
Note: this function needs to be performant, otherwise `denote-menu'
loading time suffer greatly."
    (let* ((sig1 (aref (cadr a) 0))
           (sig2 (aref (cadr b) 0)))
      ;; FIXME 2024-03-04: I have to replace "."s with "=" because in
      ;; `kb/denote-menu--path-to-entry' I do the reverse. This is quite
      ;; fragile, so try to find a more robust alternative
      (setq sig1 (replace-regexp-in-string "\\." "=" sig1)
            sig2 (replace-regexp-in-string "\\." "=" sig2))
      (kb/denote-menu--signature-lessp sig1 sig2)))

  (defun kb/denote-menu--next-signature (sig)
    "Return the signature following SIG.
The following signature for \"a\" is \"b\", for \"9\" is \"10\", for
\"z\" is \"A\", and for \"Z\" \"aa\"."
    (let* ((groups (kb/denote-menu--signature-decompose-into-groups sig))
           (parts (kb/denote-menu--signature-elements-head-tail (car (last groups))))
           tail char next)
      (while (cdr parts)                  ; Get final portion of signature
        (setq parts (kb/denote-menu--signature-elements-head-tail (cdr parts))))
      (setq tail (car parts)
            char (string-to-char tail)
            next (cond ((s-numeric-p tail) ; A number
                        (number-to-string
                         (1+ (string-to-number tail))))
                       ((and (>= char 97) (< char 122)) ; Between "a" and "z"
                        (char-to-string (1+ char)))
                       ((and (>= char 65) (< char 90)) ; Between "A" and "Z"
                        (char-to-string (1+ char)))
                       ((= 122 char) "A") ; Is "z"
                       ;; REVIEW 2024-03-03: Presently, we choose to go into
                       ;; double-letters when we go above Z
                       ((= 90 char) "aa"))) ; Is "Z"
      (concat (string-remove-suffix tail sig) next)))

  (defvar kb/denote-menu--relations
    '("Sibling" "Child" "Top-level")
    "List of possible note relations for
`kb/denote-menu--determine-new-signature'. See its docstring for more
information.")

  (defun kb/denote-menu--determine-new-signature (sig &optional relation dir)
    "Return the next available signature relative to SIG.

The new signature depends on RELATION, a string in
`kb/denote-menu--relations'. If RELATION is \"child\", then return the
next signature available for a new child note. If it is \"sibling\",
then the new note will be the next available signature at the same
hierarchical level as SIG. If it is \"top-level\", then the next
available top-level signature will be returned. If RELATION is nil, then
it defaults to a value of \"child\".

If DIR is provided, check for the existence of signatures in that
directory rather than the entirety of `denote-directory'. DIR can also
be a file. If it is, the parent directory of that file will be used as
the directory."
    (let* ((relation (or (downcase relation) "child"))
           (dir (when dir
                  (file-name-directory (file-relative-name dir denote-directory))))
           (next-sig (pcase relation
                       ("child"
                        (concat sig
                                (if (s-numeric-p (substring sig (1- (length sig))))
                                    "a" "1")))
                       ("sibling"
                        (kb/denote-menu--next-signature sig))
                       ("top-level"
                        (concat (number-to-string
                                 (1+ (cl-loop for f in (denote-directory-files dir)
                                              maximize (string-to-number
                                                        (car (kb/denote-menu--signature-decompose-into-groups
                                                              (or (denote-retrieve-filename-signature f)
                                                                  "000")))))))
                                "=1")))))
      (while (member next-sig
                     (cl-loop for f in (denote-directory-files dir)
                              collect (denote-retrieve-filename-signature f)))
        (setq next-sig (kb/denote-menu--next-signature next-sig)))
      next-sig))

  (defun kb/denote-menu--add-group-text-property (text sig)
    "Add the `denote-menu-sig' text property to TEXT.
Its value will be SIG.

Call this function for its side effects."
    (add-text-properties 0
                         (length text)
                         (list 'denote-menu-sig (if sig
                                                    (car (kb/denote-menu--signature-decompose-into-groups sig))
                                                  "No signature"))
                         text))

  (defun kb/denote-menu-set-signature-interactively (files)
    "Set the note at point's signature by selecting another note.
Select another note and choose whether to be its the sibling or child.

Also accepts FILES, which are the list of file paths which are
considered.

If nil or called interactively, then defaults `denote-directory-files'
constrained to notes with signatures (i.e. \"==\") and are in the
current subdirectory (this is my bespoke desired behavior), as well as
the :omit-current non-nil. Otherwise,when called interactively in
`denote-menu', it will be the value of `denote-menu--entries-to-paths'."
    (interactive (list (if (eq major-mode 'denote-menu-mode)
                           (denote-menu--entries-to-paths)
                         (denote-directory-files
                          (rx (literal (car (last (butlast (file-name-split (buffer-file-name)) 1))))
                              "/" (* alnum) "==")
                          :omit-current))))
    (let* ((file-at-point (cond ((derived-mode-p 'denote-menu-mode)
                                 (kb/denote-menu--get-path-at-point))
                                ((denote-file-is-note-p (buffer-file-name))
                                 (buffer-file-name))
                                (t
                                 (user-error "Must use in `denote-menu' or a Denote note!"))))
           (files (remove file-at-point
                          (or files (denote-directory-files
                                     (rx (literal (car (last (butlast (file-name-split (buffer-file-name)) 1))))
                                         "/" (* alnum) "==")
                                     :omit-current))))
           (files
            (cl-loop for file in files collect
                     (let ((sig (denote-retrieve-filename-signature file)))
                       (kb/denote-menu--add-group-text-property file sig)
                       file)))
           (largest-sig-length
            (cl-loop for file in files
                     maximize (length (denote-retrieve-filename-signature file))))
           (display-sort-function
            (lambda (completions)
              (cl-sort completions
                       'kb/denote-menu--signature-lessp
                       :key (lambda (c) (denote-retrieve-filename-signature c)))))
           (group-function
            (lambda (title transform)
              (if transform
                  title
                (get-text-property 0 'denote-menu-sig title))))
           (affixation-function
            (lambda (cands)
              (cl-loop for cand in cands collect
                       (let* ((title (denote-retrieve-front-matter-title-value
                                      cand (denote-filetype-heuristics cand)))
                              (propertized-title (propertize title 'face 'denote-faces-title))
                              (sig (denote-retrieve-filename-signature cand))
                              (propertized-sig
                               (replace-regexp-in-string "=" (propertize "." 'face 'shadow)
                                                         (kb/denote-menu--signature-propertize sig))))
                         (kb/denote-menu--add-group-text-property propertized-title sig)
                         (list propertized-title
                               (string-pad propertized-sig (+ largest-sig-length 3))
                               nil)))))
           (selection
            (completing-read "Choose a note: "
                             (lambda (str pred action)
                               (if (eq action 'metadata)
                                   `(metadata
                                     (display-sort-function . ,display-sort-function)
                                     (group-function . ,group-function)
                                     (affixation-function . ,affixation-function))
                                 (complete-with-action action files str pred)))
                             nil t))
           (file-type (denote-filetype-heuristics selection))
           (current-sig (denote-retrieve-filename-signature selection))
           (note-relation
            (downcase (completing-read "Choose relation: " kb/denote-menu--relations)))
           (new-sig (kb/denote-menu--determine-new-signature
                     (denote-retrieve-filename-signature selection)
                     note-relation
                     selection))
           (denote-rename-no-confirm t))
      (kb/denote-menu-set-signature file-at-point new-sig))))

;;;;; Override for `denote-menu-mode' and friends
(with-eval-after-load 'denote-menu
  (defun kb/denote-menu--path-to-entry (path)
    "Convert PATH to an entry matching the form of `tabulated-list-entries'."
    (if denote-menu-show-file-signature
        `(,(denote-menu--path-to-unique-identifier path)
          [,(denote-menu-signature path)
           ,(denote-menu-title path)
           ,(concat (propertize "(" 'face 'shadow)
                    (string-join
                     (mapcar (lambda (s) (propertize s 'face 'denote-faces-keywords))
                             (denote-extract-keywords-from-path path))
                     (propertize ", " 'face 'shadow))
                    (propertize ")" 'face 'shadow))])

      `(,(denote-menu--path-to-unique-identifier path)
        [(,(denote-menu-date path) . (action ,(lambda (button) (funcall denote-menu-action path))))
         ,(denote-menu-title path)
         ,(concat (propertize "(" 'face 'shadow)
                  (string-join
                   (mapcar (lambda (s) (propertize s 'face 'denote-faces-keywords))
                           (denote-extract-keywords-from-path path))
                   (propertize ", " 'face 'shadow))
                  (propertize ")" 'face 'shadow))])))
  (advice-add 'denote-menu--path-to-entry :override #'kb/denote-menu--path-to-entry)

  (defvar kb/denote-menu--signature-propertize-cache nil
    "Signature cache for `kb/denote-menu--signature-propertize'.")

  (defun kb/denote-menu--signature-propertize-element (sig level)
    "Return SIG with its first element (head) propertized.
The head is propertized with the stipulation that its nesting level is
LEVEL."
    (unless (or (not sig) (string-empty-p sig))
      (let* ((groups (kb/denote-menu--signature-decompose-into-groups sig))
             (head (car (kb/denote-menu--signature-decompose-elements-from-group (car groups)))))
        (concat
         (propertize head
                     'face (intern (format "outline-%s" (+ 1 (% (1- level) 8)))))
         (string-remove-prefix head sig)))))

  (defun kb/denote-menu--signature-propertize (sig)
    "Return propertized SIG for hierarchical visibility."
    (or (and (not sig) "")
        (cdr (assoc-string sig kb/denote-menu--signature-propertize-cache))
        (let* ((groups (kb/denote-menu--signature-decompose-into-groups sig))
               (decomposed
                (flatten-list
                 (cl-loop for group in groups
                          collect (kb/denote-menu--signature-decompose-elements-from-group group))))
               (level (1- (length decomposed)))
               (face (if (string= sig "000") 'shadow ; 000 is my "unsorted" signature
                       (intern (format "outline-%s" (+ 1 (% (1- level) 8))))))
               (propertized-sig (propertize sig 'face face)))
          (add-to-list 'kb/denote-menu--signature-propertize-cache
                       (cons sig propertized-sig))
          propertized-sig)))

  (defun kb/denote-menu-signature (path)
    "Return file signature from denote PATH identifier."
    (let ((sig (denote-retrieve-filename-signature path)))
      (replace-regexp-in-string "=" (propertize "." 'face 'shadow)
                                (kb/denote-menu--signature-propertize sig))))
  (advice-add 'denote-menu-signature :override #'kb/denote-menu-signature)

  (defun kb/denote-menu-title (path)
    "Return title of PATH.
If the denote file PATH has no title, return the string \"(No
Title)\".  Otherwise return PATH's title.

Determine whether a denote file has a title based on the
following rule derived from the file naming scheme:

1. If the path does not have a \"--\", it has no title."

    (let* ((title (if (or (not (string-match-p "--" path)))
                      (propertize "(No Title)" 'face 'font-lock-comment-face)
                    (propertize (denote-retrieve-front-matter-title-value path (denote-filetype-heuristics path))
                                'face 'denote-faces-title)))
           (file-type (propertize (concat "." (denote-menu-type path)) 'face 'font-lock-keyword-face)))
      (if denote-menu-show-file-type
          (concat title " " file-type)
        title)))
  (advice-add 'denote-menu-title :override #'kb/denote-menu-title)

  (define-derived-mode denote-menu-mode tabulated-list-mode "Denote Menu"
    "Major mode for browsing a list of Denote files."
    :interactive nil
    (if denote-menu-show-file-signature
        (setq tabulated-list-format `[("Signature" ,denote-menu-signature-column-width kb/denote-menu--signature-sorter)
                                      ("Title" ,denote-menu-title-column-width t)
                                      ("Keywords" ,denote-menu-keywords-column-width nil)])

      (setq tabulated-list-format `[("Date" ,denote-menu-date-column-width t)
                                    ("Title" ,denote-menu-title-column-width t)
                                    ("Keywords" ,denote-menu-keywords-column-width nil)]))

    (denote-menu-update-entries)
    (setq tabulated-list-sort-key (if denote-menu-show-file-signature
                                      '("Signature" . nil)
                                    '("Date" . t)))
    (tabulated-list-init-header)
    (tabulated-list-print)))

;;;; Consult-notes
(use-package consult-notes
  :disabled                             ; Trying without
  :ensure (consult-notes :type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; In case using `org-roam'
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :general (kb/note-keys "f" 'consult-notes)
  :custom
  ;; File paths must have ending slashing. See
  ;; https://github.com/mclear-tools/consult-notes/issues/26#issuecomment-1356038580
  (consult-notes-file-dir-sources nil)
  ;; Denote
  (consult-notes-denote-display-id nil)
  (consult-notes-denote-dir t)
  :custom-face
  (org-link ((t (:slant italic))))
  (denote-faces-link ((t (:foreground "goldenrod3" :underline nil :slant italic))))
  :config
  (when (locate-library "org-roam")
    (consult-notes-denote-mode))

  ;; Custom `consult--multi' sections
  (defconst consult-notes-denote--source
    (list :name     "Notes"
          :narrow   ?n
          :category 'consult-notes-category
          :annotate #'consult-notes-denote--annotate
          :items    (lambda ()
                      (let* ((max-width 0)
                             (cands (mapcar (lambda (f)
                                              (let* ((id (denote-retrieve-filename-identifier f))
                                                     (title (org-fontify-like-in-org-mode
                                                             (if consult-notes-denote-display-id
                                                                 (concat id " " (denote-retrieve-title-value f (denote-filetype-heuristics f)))
                                                               (denote-retrieve-title-value f (denote-filetype-heuristics f)))))
                                                     (dir (file-relative-name (file-name-directory f) denote-directory))
                                                     (keywords (denote-extract-keywords-from-path f)))
                                                (let ((current-width (string-width title)))
                                                  (when (> current-width max-width)
                                                    (setq max-width (+ 24 current-width))))
                                                (propertize title 'denote-path f 'denote-keywords keywords)))
                                            (remove-if-not (lambda (f) (equal (file-name-extension f) "org"))
                                                           (cl-set-difference
                                                            (denote-directory-files) ; See `denote-file-is-note-p'
                                                            (denote-directory-files "papers/")))))) ; Exclude papers directory
                        (mapcar (lambda (c)
                                  (let* ((keywords (get-text-property 0 'denote-keywords c))
                                         (path (get-text-property 0 'denote-path c))
                                         (dirs (directory-file-name (file-relative-name (file-name-directory path) denote-directory))))
                                    (concat c
                                            ;; align keywords
                                            (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
                                            (format "%18s"
                                                    (if keywords
                                                        (concat (propertize "#" 'face 'consult-notes-name)
                                                                (propertize (mapconcat 'identity keywords " ") 'face 'consult-notes-name))
                                                      ""))
                                            (when consult-notes-denote-dir (format "%18s" (propertize (concat "/" dirs) 'face 'consult-notes-name))))))
                                cands)))
          ;; Custom preview
          :state  #'consult-notes-denote--state
          ;; Create new note on match fail
          :new     #'consult-notes-denote--new-note))

  (defconst kb/consult-notes-papers--source
    (list :name     "Papers"
          :narrow   ?p
          :category 'consult-notes-category
          :annotate #'consult-notes-denote--annotate
          :items    (lambda ()
                      (let* ((max-width 0)
                             (cands (mapcar (lambda (f)
                                              (let* ((id (denote-retrieve-filename-identifier f))
                                                     (title (org-fontify-like-in-org-mode
                                                             (if consult-notes-denote-display-id
                                                                 (concat id " " (denote-retrieve-title-value f (denote-filetype-heuristics f)))
                                                               (denote-retrieve-title-value f (denote-filetype-heuristics f)))))
                                                     (dir (file-relative-name (file-name-directory f) denote-directory))
                                                     (keywords (denote-extract-keywords-from-path f)))
                                                (let ((current-width (string-width title)))
                                                  (when (> current-width max-width)
                                                    (setq max-width (+ 24 current-width))))
                                                (propertize title 'denote-path f 'denote-keywords keywords)))
                                            (remove-if-not (lambda (f) (equal (file-name-extension f) "org"))
                                                           (directory-files-recursively
                                                            (expand-file-name "papers" kb/notes-dir)
                                                            denote-id-regexp)))))
                        (mapcar (lambda (c)
                                  (let* ((keywords (get-text-property 0 'denote-keywords c)))
                                    (concat c
                                            ;; align keywords
                                            (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
                                            (format "%18s" (if keywords
                                                               (concat (propertize "#" 'face 'consult-notes-name)
                                                                       (propertize (mapconcat 'identity keywords " ") 'face 'consult-notes-name))
                                                             "")))))
                                cands)))
          ;; Custom preview
          :state  #'consult-notes-denote--state
          ;; Create new note on match fail
          :new     #'consult-notes-denote--new-note)
    "For my papers.")
  (add-to-list 'consult-notes-all-sources 'kb/consult-notes-papers--source 'append)

  (consult-customize
   consult-notes
   :prompt "Go to..."
   :preview-key "C-M-;")

  (advice-add 'denote-rename-file-using-front-matter :around
              #'(lambda (orig-fun &rest args)
                  (let ((save-silently t))
                    (apply orig-fun args)))))

;;;; Citar-denote
(use-package citar-denote
  :demand
  :diminish
  :custom
  (citar-denote-subdir t)
  (citar-denote-signature t)
  (citar-denote-title-format nil)       ; Use citekey as title
  (citar-denote-title-format-authors 2)
  (citar-denote-title-format-andstr "and")
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-template t)
  :general (kb/note-keys
             "b b" 'citar-denote-dwim
             "b c" 'citar-create-note
             "b o" 'citar-denote-open-note
             "b f" 'citar-denote-find-citation
             "b e" 'citar-denote-open-reference-entry
             "b k a" 'citar-denote-add-citekey
             "b k r" 'citar-denote-remove-citekey
             "b r f" 'citar-denote-find-reference
             "b r F" 'citar-denote-nocite
             "b r l" 'citar-denote-link-reference)
  :config
  (citar-denote-mode 1)

  ;; Keep the reference keyword after Denote's identifier keyword
  (defun kb/citar-denote--add-reference (citekey file-type)
    "Add reference with CITEKEY in front matter of the file with FILE-TYPE.

`citar-denote-add-citekey' is the interactive version of this function."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (rx bol (literal "#+identifier:")) nil t)
      (if (eq (or file-type 'org) 'org)
          (forward-line 1)
        (forward-line -2))
      (insert
       (format (citar-denote--reference-format file-type) citekey))))
  (advice-add 'citar-denote--add-reference :override #'kb/citar-denote--add-reference)
  :config
  (setq citar-denote-file-types
        `((org
           :reference-format "#+reference: %s\n" ; Keep single space
           :reference-regex "^#\\+reference\\s-*:")
          (markdown-yaml
           :reference-format "reference:  %s\n"
           :reference-regex "^reference\\s-*:")
          (markdown-toml
           :reference-format "reference  = %s\n"
           :reference-regex "^reference\\s-*=")
          (text
           :reference-format "reference:  %s\n"
           :reference-regex "^reference\\s-*:"))))

(provide 'org-notes-rcp)
;;; org-notes-rcp.el ends here
