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
  :autoload (denote-directory-files kb/denote-auto-rename)
  :commands (denote denote-open-or-create)
  :hook ((dired-mode . denote-dired-mode)
         (denote-dired-mode . toggle-truncate-lines)
         (before-save . kb/denote-insert-identifier-maybe)
         (after-save . kb/denote-auto-rename)
         (kb/themes . kb/themes-setup-denote-faces))
  :bind
  ( :map kb/note-keys
    ("f" . denote-open-or-create)
    ("i" . denote-insert-link)
    ("I" . denote-link-or-create)
    ("e" . denote-org-extras-extract-org-subtree)
    ("t" . denote-rename-file-keywords)
    ("l" . denote-backlinks))
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
\\center
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
  (denote-backlinks-display-buffer-action
   '((display-buffer-reuse-window display-buffer-below-selected)
     (window-height . fit-window-to-buffer)
     (post-command-select-window . t)
     (dedicated . t)))
  (denote-rename-buffer-format "%s %t")
  (denote-rename-confirmations '(add-front-matter))
  :preface
  (add-to-list 'package-pinned-packages '(denote . "gnu-elpa-devel"))
  :config
  (denote-rename-buffer-mode 1)
  (denote-menu-bar-mode 1)

  ;; Set `org-refile-targets'
  (with-eval-after-load 'org-refile
    (add-to-list 'org-refile-targets
                 `(,(car (denote-directory-files "20221011T101254")) . (:maxlevel . 2))))

  ;; Rename denote note. Meant to be added to `after-save-hook'
  (defun kb/denote-auto-rename ()
    "Auto rename denote file."
    (when-let ((f (buffer-file-name)))
      (when (and (file-in-directory-p f denote-directory)
                 (denote-filename-is-note-p f))
        (with-demoted-errors "Error: %S"
          (denote-rename-file-using-front-matter f)))))

  (require 's)
  ;; Camel cased keywords
  (defun kb/denote-sluggify-keyword (str)
    "Sluggify STR while joining separate words.
  My version camelCases keywords."
    (s-lower-camel-case
     (denote-slug-hyphenate str)))
  (setq denote-file-name-slug-functions
        '((title . denote-sluggify-title)
          (signature . denote-sluggify-signature)
          (keyword . kb/denote-sluggify-keyword)))

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
      (set-face-attribute 'denote-faces-link nil :weight 'normal :foreground fg-active-argument :inherit 'unspecified)
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
    (let ((existing-buffers (buffer-list)))
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
              (delete-trailing-whitespace))
            (with-demoted-errors "Error: %S"
              (denote-rename-file-using-front-matter file))
            ;; Kill buffer unless it already exists
            (unless (member (get-buffer (buffer-name)) existing-buffers)
              (kill-buffer))))))))

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
  (denote-explore-network-graphviz-filetype "pdf")
  (denote-explore-network-keywords-ignore '("archive")))

;;;;; Update link descriptions
(with-eval-after-load 'org
  (defun kb/denote-explore-update-link-descriptions (confirmp)
    "Recreate denote link descriptions in the current buffer.
If called with CONFIMP, then prompt user to confirm a replacement. When
interactively called, CONFIRMP is non-nil by default, flipping the value
with prefix-arg."
    (interactive (list (not current-prefix-arg)))
    (require 'denote-org-extras)
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
                         'face 'denote-faces-title)))

  (defun kb/denote-explore-update-link-descriptions-globally (dir confirmp)
    "Update the link description of all notes in DIR.
If CONFIRMP is non-nil, then prompt the user to confirm each
replacement."
    (interactive (list (denote-subdirectory-prompt) current-prefix-arg))
    (save-window-excursion
      (dolist (f (denote-directory-files (concat (file-name-nondirectory dir) "/") nil t))
        (save-excursion
          (let* ((live-buffer (get-file-buffer f)))
            (with-current-buffer (find-file-noselect f)
              (kb/denote-explore-update-link-descriptions confirmp))
            (unless live-buffer (kill-buffer live-buffer))))))
    (message "Updated all links in %s!" dir)))

;;;; Denote-interface
(use-package denote-interface
  :vc (:url "git@github.com:krisbalintona/denote-interface.git"
            :rev :newest)
  :hook (denote-interface-mode . (lambda () (kb/puni-mode -1)))
  :bind
  ( :map kb/note-keys
    ("m" . denote-interface-list)
    ("r" . denote-interface-set-signature-list)
    ("R" . denote-interface-set-signature-minibuffer))
  :custom
  (denote-interface-signature-column-width
   (+ 6 (cl-loop for file in (denote-directory-files)
                 maximize (length (denote-retrieve-filename-signature file)))))
  (denote-interface-title-column-width 120)
  (denote-interface-starting-filter-presets
   '("zettels/[^z-a]*" "bib/[^z-a]*"))
  (denote-interface-starting-filter "zettels/[^z-a]*")
  :config
  (advice-add 'denote-sort-signature-lessp
              :override (lambda (f1 f2)
                          (denote-interface--signature-lessp (denote-retrieve-filename-signature f1)
                                                             (denote-retrieve-filename-signature f2)))))

;;;; Consult-notes
(use-package consult-notes
  :disabled                             ; Trying without
  :ensure (consult-notes :type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; In case using `org-roam'
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :bind
  ( :map kb/note-keys
    ("f" . consult-notes))
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
  (citar-denote-subdir t)
  (citar-denote-signature t)
  (citar-denote-title-format nil)       ; Use citekey as title
  (citar-denote-title-format-authors 2)
  (citar-denote-title-format-andstr "and")
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-template t)
  :bind
  ( :map kb/note-keys
    ("b b" . citar-denote-dwim)
    ("b c" . citar-create-note)
    ("b o" . citar-denote-open-note)
    ("b f" . citar-denote-find-citation)
    ("b e" . citar-denote-open-reference-entry)
    ("b k a" . citar-denote-add-citekey)
    ("b k r" . citar-denote-remove-citekey)
    ("b r f" . citar-denote-find-reference)
    ("b r F" . citar-denote-nocite)
    ("b r l" . citar-denote-link-reference))
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
