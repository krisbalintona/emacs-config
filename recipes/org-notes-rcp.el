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

* 1 Draft                                                            :export:

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

  ;; Rename denote note. Meant to be added to `after-save-hook'
  (defun kb/denote-auto-rename ()
    "Auto rename denote file."
    (when-let ((f (buffer-file-name)))
      (when (and (file-in-directory-p f denote-directory)
                 (denote-filename-is-note-p f))
        (denote-rename-file-using-front-matter f :auto-confirm))))

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
      (set-face-attribute 'denote-faces-title nil :weight 'bold :foreground rainbow-2)
      (set-face-attribute 'denote-faces-keywords nil :weight 'normal :slant 'italic)
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

;;;;; Update link descriptions (old)
;; FIXME 2024-03-04: Check how much of this is still relevant
(with-eval-after-load 'denote
  (defun kb/denote--update-buffer-link-descriptions (buffer)
    "Update the link descriptions for all `denote' links in BUFFER,
then save.

Returns the number of link descriptions corrected."
    (with-current-buffer buffer
      (let ((link-positions
             ;; Creates a list of lists. Each item on the list represents data
             ;; from that denote link in the buffer. This data list has four
             ;; elements: the end property value (in case there is no
             ;; description, this position is used for help) ID, the
             ;; contents-begin value, and the contents-end value (i.e.
             ;; description bounds positions)
             (org-element-map (org-element-parse-buffer) 'link
               (lambda (l)
                 (when (string= (org-element-property :type l) "denote")
                   (list
                    ;; ID
                    (org-element-property :end l)
                    (org-element-property :path l)
                    ;; Desc positions
                    (org-element-property :contents-begin l)
                    (org-element-property :contents-end l))))))
            (replaced-count 0))
        ;; Reverse list so we go backward. Going in order means that our
        ;; positions in link-positions are misaligned with the actual buffer as
        ;; we do replacements
        (save-excursion
          (dolist (l (reverse link-positions))
            (let ((note-title
                   (or (kb/denote-search-from-id (nth 1 l))
                       (user-error "Denote link at position %s in file %s does not have a corresponding note!"
                                   (nth 0 l) buffer)))
                  (desc-beg (nth 2 l))
                  (desc-end (nth 3 l)))
              ;; Replace link if desc doesn't exist or the desc is not the proper
              ;; title
              (unless (and desc-beg (string= note-title (buffer-substring-no-properties desc-beg desc-end)))
                (when desc-beg (delete-region desc-beg desc-end))
                (goto-char (or desc-beg (+ 2 (nth 0 l))))
                (insert note-title)
                (setq replaced-count (1+ replaced-count))))
            ))
        replaced-count)))

  (defun kb/denote-update-link-descriptions (prefix)
    "Updates all link descriptions in current buffer.

If called with `universal-arg', then replace links in all denote buffers."
    (interactive "*p")
    (let* ((files (if (< 1 prefix)
                      (denote-directory-files)
                    (if (and (buffer-file-name) (denote-file-is-note-p (buffer-file-name)))
                        (list (buffer-file-name))
                      (user-error "Not a `denote-file'!"))))
           (initial-buffers (buffer-list))
           (replaced-count 0)
           (updated-notes 0))
      (save-excursion
        (dolist (f files)
          (let* ((b (find-file-noselect f))
                 (new-counts (kb/denote--update-buffer-link-descriptions b)))
            (when (< 0 new-counts)
              (setq updated-notes (1+ updated-notes))
              (setq replaced-count (+ replaced-count new-counts)))
            (save-buffer)
            (unless (member b initial-buffers)
              (kill-buffer b)))))
      (message "Done! Replaced a total of %s links across %s files!"
               replaced-count updated-notes))))

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
  (defun kb/denote-explore-update-link-descriptions ()
    "Recreate denote link descriptions in the current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (denote-org-extras--get-link-type-regexp 'denote) nil :no-error)
        (condition-case er
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
                     (heading-custom-id (cadr (string-split path "::"))))
                (goto-char link-beg)
                (delete-region link-beg link-end)
                (insert
                 ;; TODO 2024-03-04: This is a brittle way to create the link.
                 ;; Changes to Denote might break this. Avoid that.
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
                           (denote--link-get-description file))))))
          (error (message "Error encountered:  %s" (error-message-string err))))))))

;;;; Denote-menu
(use-package denote-menu
  :general
  (kb/note-keys "m" 'denote-menu-list-notes)
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
    '("zettels/[^z-a]n*" "bib/[^z-a]*")
    "The common filters I use.")
  :config
  ;; Custom denote-menu functions and commands
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

  (defun kb/denote-menu-set-signature ()
    "Set the note at point's signature."
    (interactive)
    (let* ((path (kb/denote-menu--get-path-at-point))
           (file-type (denote-filetype-heuristics path))
           (title (denote-retrieve-title-value path file-type))
           (initial-sig (denote-retrieve-filename-signature path))
           (new-sig (denote-signature-prompt
                     (unless (string= initial-sig "000") initial-sig) ; 000 is the "unsorted" signature for me
                     "Choose new signature"))
           (keywords
            (denote-retrieve-front-matter-keywords-value path file-type))
           (denote-rename-no-confirm t)) ; Want it automatic
      (denote-rename-file path title keywords new-sig)))

  (defun kb/denote-menu--signature-decompose (sig)
    "Take a SIG and return a cons.
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
        (if (string-match (if (s-numeric-p (substring sig 0 1))
                              (rx (or (any alpha) "=")) ; Numbered index head
                            (rx (or (any digit) "=")))  ; Alphabet index head
                          sig)
            (setq head (substring sig 0 (match-beginning 0))
                  tail (string-remove-prefix "=" (substring sig (match-beginning 0))))
          (setq head sig
                tail nil)))
      (cons head tail)))

  (defun kb/denote-menu--signature-lessp (sig1 sig2)
    "Compare two strings based on my signature sorting rules.
Returns t if SIG1 should be sorted before SIG2, nil otherwise."
    (let* ((parts1 (kb/denote-menu--signature-decompose sig1))
           (parts2 (kb/denote-menu--signature-decompose sig2))
           (head1 (car parts1))
           (head2 (car parts2))
           (tail1 (cdr parts1))
           (tail2 (cdr parts2))
           ;; HACK 2024-03-03: Right now, this treats uppercase and lowercase as
           ;; the same, as well as ignores the difference between, e.g., "a" and
           ;; "aa"
           (index1 (string-to-number head1 16))
           (index2 (string-to-number head2 16)))
      (cond
       ;; Sig1 is earlier in order than sig2
       ((< index1 index2) t)
       ;; Sig2 is later than sig2
       ((> index1 index2) nil)
       ;; Sig1 has no tail while sig2 has a tail, so it's later than sig2
       ((and (not tail1) tail2) t)
       ;; Sig1 has a tail while sig2 has no tail, so it's earlier than sig2
       ((and tail1 (not tail2)) nil)
       ;; Neither sig2 nor sig2 have a tail, and their indexes must be equal, so
       ;; they must have identical signatures. So do something with it now.
       ;; (Returning nil seems to put the oldest earlier, so we do that.)
       ((and (not tail1) (not tail2)) nil)
       ;; Their indices are equal, and they still have a tail, so process those
       ;; tails next
       ((= index1 index2)
        (kb/denote-menu--signature-lessp tail1 tail2)))))

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
      ;; Use `kb/denote-menu--signature-lessp' if both a and b have signatures.
      ;; If not, then return t if a has a signature, nil if b has a signature,
      ;; and if neither has a signature, then default to `string-collate-lessp'
      (cond ((and sig1 sig2)
             (kb/denote-menu--signature-lessp sig1 sig2))
            (sig1 t)
            (sig2 nil)
            (t
             (string-collate-lessp (car (split-string (car a) "-"))
                                   (car (split-string (car b) "-")))))))

  (defun kb/denote-menu--next-signature (file)
    "Return the signature following the signature of FILE.
The following signature for \"a\" is \"b\", for \"9\" is \"10\", for
\"z\" is \"A\", and for \"Z\" \"aa\"."
    (let* ((sig (denote-retrieve-filename-signature file))
           (parts (kb/denote-menu--signature-decompose sig))
           tail char next)
      (while (cdr parts)                  ; Get final portion of signature
        (setq parts (kb/denote-menu--signature-decompose (cdr parts))))
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

  (defun kb/denote-menu-set-signature-interactively ()
    "Set the note at point's signature by selecting another note.
Select another note and choose whether to be its the sibling or child."
    (interactive)
    (let* ((file-at-point (kb/denote-menu--get-path-at-point))
           (files (remove file-at-point (denote-menu--entries-to-paths)))
           (largest-sig-length
            (cl-loop for file in files
                     maximize (length (denote-retrieve-filename-signature file))))
           (display-sort-function
            (lambda (completions)
              (cl-sort completions
                       'kb/denote-menu--signature-lessp
                       :key (lambda (c) (denote-retrieve-filename-signature c)))))
           (affixation-function
            (lambda (cands)
              (cl-loop for cand in cands collect
                       (list (denote-retrieve-front-matter-title-value cand (denote-filetype-heuristics cand))
                             (string-pad (denote-retrieve-filename-signature cand)
                                         (+ largest-sig-length 3))
                             nil))))
           (selection
            (completing-read "Choose a note: "
                             (lambda (str pred action)
                               (if (eq action 'metadata)
                                   `(metadata
                                     (display-sort-function . ,display-sort-function)
                                     (cycle-sort-function . ,#'identity)
                                     (affixation-function . ,affixation-function))
                                 (complete-with-action action files str pred)))
                             nil t))
           (file-type (denote-filetype-heuristics selection))
           (current-sig (denote-retrieve-filename-signature selection))
           (childp
            (string= "Child" (completing-read "Choose relation: " '("Sibling" "Child"))))
           (new-sig
            (if childp
                (concat current-sig
                        (if (s-numeric-p (substring current-sig (1- (length current-sig))))
                            "a" "1"))
              (kb/denote-menu--next-signature selection)))
           (denote-rename-no-confirm t))
      (denote-rename-file file-at-point
                          (denote-retrieve-front-matter-title-value file-at-point file-type)
                          (denote-retrieve-front-matter-keywords-value file-at-point file-type)
                          new-sig)))

  ;; Redefinitions for built-ins
  (defun kb/denote-menu--path-to-entry (path)
    "Convert PATH to an entry matching the form of `tabulated-list-entries'."
    (if denote-menu-show-file-signature
        `(,(denote-menu--path-to-unique-identifier path)
          [,(replace-regexp-in-string "=" "." (denote-menu-signature path))
           ,(denote-menu-title path)
           ,(propertize (format "%s" (denote-extract-keywords-from-path path)) 'face 'italic)])

      `(,(denote-menu--path-to-unique-identifier path)
        [(,(denote-menu-date path) . (action ,(lambda (button) (funcall denote-menu-action path))))
         ,(denote-menu-title path)
         ,(propertize (format "%s" (denote-extract-keywords-from-path path)) 'face 'italic)])))
  (advice-add 'denote-menu--path-to-entry :override #'kb/denote-menu--path-to-entry)

  (defun kb/denote-menu-signature (path)
    "Return file signature from denote PATH identifier."
    (let ((signature (denote-retrieve-filename-signature path)))
      (if signature
          (propertize signature 'face 'denote-faces-signature)
        (propertize " " 'face 'font-lock-comment-face))))
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
  :after denote
  :diminish
  :custom
  (citar-denote-subdir nil)
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
  :init
  (citar-denote-mode)

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
