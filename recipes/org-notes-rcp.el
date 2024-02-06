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
         (before-save . kb/denote-insert-identifier-maybe)
         (after-save . kb/denote-auto-rename))
  :general (kb/note-keys
             "f" 'denote-open-or-create
             "i" 'denote-link-insert-link
             "ta" 'denote-keywords-add
             "tr" 'denote-keywords-remove
             "D" 'kb/denote-report-duplicates)
  :custom
  (denote-directory kb/notes-dir)
  (denote-modules '(xref ffap))
  (denote-known-keywords '("project"))
  (denote-prompts '(subdirectory title keywords))
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
  :config
  (denote-rename-buffer-mode)
  (denote-modules-global-mode)

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
        (message "No duplicates found. Hooray!!!"))))

  (defun kb/denote-auto-rename ()
    "Auto rename denote file."
    (when-let ((f (buffer-file-name)))
      (when (and (file-in-directory-p f denote-directory)
                 (denote-filename-is-note-p f))
        (denote-rename-file-using-front-matter f :auto-confirm)))))

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
          (denote heading
                  (denote-keywords-prompt nil (mapconcat 'identity (org-get-tags) ","))
                  'org
                  (denote-subdirectory-prompt))
          (insert text))
      (user-error "No subtree to extract; aborting"))))

;;; Denote-explore
;; Useful Denote utilities
(use-package denote-explore
  :after denote)

;;; Denote-menu
(use-package denote-menu
  :elpaca (:type git :host github :repo "namilus/denote-menu")
  :general
  (kb/note-keys "m" 'denote-menu-list-notes)
  (:keymaps 'denote-menu-mode-map
            "|" 'denote-menu-clear-filters
            "/ r" 'denote-menu-filter
            "/ /" 'denote-menu-filter
            "/ k" 'denote-menu-filter-by-keyword
            "e" 'denote-menu-export-to-dired))

;;; Consult-notes
(use-package consult-notes
  :disabled                             ; Trying without
  :elpaca (consult-notes :type git :host github :repo "mclear-tools/consult-notes")
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

;;; Citar-denote
(use-package citar-denote
  :after denote
  :diminish
  :custom
  (citar-denote-subdir t)
  (citar-denote-signature nil)
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

;;; org-notes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-notes-rcp)
