;;; org-notes-rcp.el --- Summary -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This is everything directly used for my note-taking needs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Denote
;;;; This
(use-package denote
  :functions kb/denote-search-from-id
  :elpaca (:type git
           :host github
           :repo "emacs-straight/denote"
           :depth nil
           :files ("*" (:exclude ".git")))
  :hook ((dired-mode . denote-dired-mode)
         (before-save . kb/denote-insert-identifier-maybe))
  :general (kb/note-keys
             "i" 'denote-link-insert-link
             "ta" 'denote-keywords-add
             "tr" 'denote-keywords-remove
             "D" 'kb/denote-report-duplicates)
  :custom
  (denote-directory kb/notes-dir)
  (denote-known-keywords '("project"))
  (denote-prompts '(subdirectory title keywords))
  :config
  (denote-rename-buffer-mode)

  (defun kb/denote-report-duplicates ()
    (interactive)
    (let* ((ids (mapcar (lambda (f) (denote-retrieve-filename-identifier f))
                        (denote-directory-files)))
           (dups (delete-dups (seq-filter
                               (lambda (id) (member id (cdr (member id ids))))
                               ids))))
      (if dups
          (message "The following are duplicated denote IDs: %s"
                   (string-join dups ", "))
        (message "No duplicates found. Hooray!!!")))))

;;;; Return denote file path based on ID
(with-eval-after-load 'denote
  (defun kb/denote-search-from-id (id)
    (when-let* ((full-path (car (cl-remove-if-not
                                 (lambda (f) (string-match-p (rx (literal id)) f))
                                 (denote-directory-files))))
                (title (denote-retrieve-title-value full-path 'org)))
      title)))

;;;; Standardizing note front-matter
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
    (when (denote-file-is-note-p (buffer-file-name))
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

;;;; Update link descriptions
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
                    (if (denote-file-is-note-p (buffer-file-name))
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

;;;; New note from region
;; Taken from
;; https://protesilaos.com/emacs/denote#h:d0c7cb79-21e5-4176-a6af-f4f68578c8dd
(with-eval-after-load 'denote
  (defun kb/denote-org-extract-subtree ()
    "Create new Denote note using current Org subtree.
Make the new note use the Org file type, regardless of the value
of `denote-file-type'.

Use the subtree title as the note's title. If available, use the
tags of the heading are used as note keywords.

Delete the original subtree."
    (interactive)
    (if-let ((text (org-get-entry))
             (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment)))
        (progn
          (delete-region (org-entry-beginning-position) (org-entry-end-position))
          (denote heading (org-get-tags) 'org)
          (insert text))
      (user-error "No subtree to extract; aborting"))))

;;;; Consult interface
;; Lighter weight alternative to consult-notes
(with-eval-after-load 'denote
  (defun kb/consult-notes-denote--file (cand)
    (format "%s" (get-text-property 0 'denote-path cand)))

  (defconst kb/consult-notes--time-relative
    `((100 "sec" 1)
      (,(* 60 100) "min" 60.0)
      (,(* 3600 30) "hour" 3600.0)
      (,(* 3600 24 400) "day" ,(* 3600.0 24.0))
      (nil "year" ,(* 365.25 24 3600)))
    "Formatting used by the function `consult-notes--time-relative'.")

  (defun kb/consult-notes--time-relative (time)
    "Format TIME as a relative age."
    (setq time (max 0 (float-time (time-since time))))
    (let ((sts kb/consult-notes--time-relative) here)
      (while (and (car (setq here (pop sts))) (<= (car here) time)))
      (setq time (round time (caddr here)))
      (format "%s %s%s ago" time (cadr here) (if (= time 1) "" "s"))))

  (defun kb/consult-notes--time-absolute (time)
    "Format TIME as an absolute age."
    (let ((system-time-locale "C"))
      (format-time-string
       (if (> (decoded-time-year (decode-time (current-time)))
              (decoded-time-year (decode-time time)))
           " %Y %b %d"
         "%b %d %H:%M")
       time)))

  (defun kb/consult-notes--time (time)
    "Format file age TIME, suitably for use in annotations."
    (if (< (float-time (time-since time)) (* 60 60 24 14))
        (kb/consult-notes--time-relative time)
      (kb/consult-notes--time-absolute time)))

  (defun kb/consult-notes-denote--annotate (cand)
    "Annotate CAND in `consult-notes-denote'."
    (let* ((path (get-text-property 0 'denote-path cand))
           (attrs (file-attributes path))
           (ftime (kb/consult-notes--time (file-attribute-modification-time attrs)))
           (fsize (file-size-human-readable (file-attribute-size attrs))))
      (put-text-property 0 (length fsize) 'face '(:inherit (warning) :weight light) fsize)
      (put-text-property 0 (length ftime) 'face '(:inherit (warning) :weight light) ftime)
      (format "%8s  %8s" fsize ftime)))

  (defun kb/consult-notes-denote--state ()
    "File preview for denote files."
    (let ((open (consult--temporary-files))
          (state (consult--file-state)))
      (lambda (action cand)
        (unless cand
          (funcall open))
        (funcall state action (and cand
                                   (kb/consult-notes-denote--file cand))))))

  (defun kb/consult-notes-denote--new-note (cand)
    "Create new note with Denote with title CAND.

Input \"foo\", then create \"id-foo\", file type is determined by
`denote-file-type', choose manually when `denote-prompts' includes
'file-type, or simply include the extension; \"foo.txt\", creates
\"id-foo.txt\."
    (let* ((f (expand-file-name cand denote-directory))
           (f-dir (file-name-directory f))
           (f-name-base (file-name-base f))
           (file-type (pcase (file-name-extension f)
                        ("org" "org")
                        ("md" "markdown-toml")
                        ("txt" "text")))
           keywords date subdirectory template)
      (dolist (prompt denote-prompts)
        (pcase prompt
          ('keywords (setq keywords (denote-keywords-prompt)))
          ('file-type (setq file-type (denote-file-type-prompt)))
          ('subdirectory (setq subdirectory (denote-subdirectory-prompt)))
          ('date (setq date (denote-date-prompt)))
          ('template (setq template (denote-template-prompt)))))
      (denote (string-trim f-name-base) keywords file-type subdirectory date template)))

  (defconst kb/denote-consult--source
    (list :name     "Notes"
          :narrow   ?n
          :category 'kb/denote-consult
          :annotate #'kb/consult-notes-denote--annotate
          :items    (lambda ()
                      (let* ((max-width 0)
                             (cands (mapcar (lambda (f)
                                              (let* ((id (denote-retrieve-filename-identifier f))
                                                     (title (org-fontify-like-in-org-mode
                                                             (denote-retrieve-title-value f (denote-filetype-heuristics f))))
                                                     (dir (file-relative-name (file-name-directory f) denote-directory))
                                                     (keywords (denote-extract-keywords-from-path f)))
                                                (let ((current-width (string-width title)))
                                                  (when (> current-width max-width)
                                                    (setq max-width (+ 24 current-width))))
                                                (propertize title 'denote-path f 'denote-keywords keywords)))
                                            (-difference
                                             (denote-directory-files nil nil t) ; See `denote-file-is-note-p'
                                             (denote-directory-files "papers/"))))) ; Exclude papers directory
                        (mapcar (lambda (c)
                                  (let* ((keywords (get-text-property 0 'denote-keywords c))
                                         (path (get-text-property 0 'denote-path c))
                                         (dirs (directory-file-name (file-relative-name (file-name-directory path) denote-directory))))
                                    (concat c
                                            ;; align keywords
                                            (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
                                            (format "%18s"
                                                    (if keywords
                                                        (concat (propertize "#" 'face '(:inherit (warning) :weight light))
                                                                (propertize (mapconcat 'identity keywords " ") 'face '(:inherit (warning) :weight light)))
                                                      ""))
                                            (format "%18s" (propertize (concat "/" dirs) 'face '(:inherit (warning) :weight light))))))
                                cands)))
          ;; Custom preview
          :state  #'kb/consult-notes-denote--state
          ;; Create new note on match fail
          :new     #'kb/consult-notes-denote--new-note))

  (defconst kb/denote-consult-papers--source
    (list :name     "Papers"
          :narrow   ?p
          :category 'kb/denote-consult
          :annotate #'kb/consult-notes-denote--annotate
          :items    (lambda ()
                      (let* ((max-width 0)
                             (cands (mapcar (lambda (f)
                                              (let* ((id (denote-retrieve-filename-identifier f))
                                                     (title (org-fontify-like-in-org-mode
                                                             (denote-retrieve-title-value f (denote-filetype-heuristics f))))
                                                     (dir (file-relative-name (file-name-directory f) denote-directory))
                                                     (keywords (denote-extract-keywords-from-path f)))
                                                (let ((current-width (string-width title)))
                                                  (when (> current-width max-width)
                                                    (setq max-width (+ 24 current-width))))
                                                (propertize title 'denote-path f 'denote-keywords keywords)))
                                            (denote-directory-files (rx (literal "papers/"))))))
                        (mapcar (lambda (c)
                                  (let* ((keywords (get-text-property 0 'denote-keywords c)))
                                    (concat c
                                            ;; align keywords
                                            (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
                                            (format "%18s" (if keywords
                                                               (concat (propertize "#" 'face '(:inherit (warning) :weight light))
                                                                       (propertize (mapconcat 'identity keywords " ") 'face '(:inherit (warning) :weight light)))
                                                             "")))))
                                cands)))
          ;; Custom preview
          :state  #'kb/consult-notes-denote--state
          ;; Create new note on match fail
          :new     #'kb/consult-notes-denote--new-note))

  (defun kb/denote-consult ()
    "Consult interface for denote notes."
    (interactive)
    (consult--multi '(kb/denote-consult--source kb/denote-consult-papers--source)))
  (kb/note-keys "f" 'kb/denote-consult)

  (consult-customize kb/denote-consult
                     :prompt "Go to..."
                     :preview-key "C-M-;"))

;;; Denote-menu
(use-package denote-menu
  :elpaca (:type git :host github :repo "namilus/denote-menu")
  :general
  (kb/note-keys "d" 'denote-menu-list-notes)
  (:keymaps 'denote-menu-mode-map
   "|" 'denote-menu-clear-filters
   "/ r" 'denote-menu-filter
   "/ /" 'denote-menu-filter
   "/ k" 'denote-menu-filter-by-keyword
   "e" 'denote-menu-export-to-dired))

;;; Consult-notes
(use-package consult-notes
  :disabled                             ; Over-engineered; made my own solution
  :elpaca (consult-notes :type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; In case using `org-roam'
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :general (kb/note-keys "f" '(consult-notes :wk "Consult-notes"))
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
                                            (cl-set-difference
                                             (denote-directory-files-matching-regexp
                                              (rx (group (literal ".org") eol))) ; Only org files
                                             (directory-files-recursively (expand-file-name "papers" kb/notes-dir)
                                                                          consult-notes-file-match))))) ; Exclude papers directory
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
                                            (remove-if-not (lambda (f) (string-match-p (rx (literal ".org") eol) f))
                                                           (directory-files-recursively
                                                            (expand-file-name "papers" kb/notes-dir)
                                                            consult-notes-file-match)))))
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

;;; org-notes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-notes-rcp)
