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
  :autoload denote-interface--signature-lessp
  :hook (denote-interface-mode . (lambda () (kb/puni-mode -1)))
  :bind
  ( :map krisb-note-keymap
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
  :init
  (with-eval-after-load 'denote
    (setopt denote-sort-signature-comparison-function #'denote-interface--signature-lessp)))

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
  ( :map krisb-note-keymap
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
                                                            (expand-file-name "papers" krisb-notes-directory)
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
  :demand t
  :after citar
  :diminish
  :bind (("C-c b c" . citar-create-note)
         :map krisb-note-keymap
         ("b b" . citar-denote-link-reference)
         ("b o" . citar-denote-dwim)
         ("b c" . citar-create-note)
         ("b n" . citar-denote-open-note)
         ("b k a" . citar-denote-add-citekey)
         ("b k r" . citar-denote-remove-citekey))
  :custom
  (citar-denote-subdir "/bib/")
  (citar-denote-signature nil)
  (citar-denote-title-format nil)       ; Use citekey as title
  (citar-denote-title-format-authors 2)
  (citar-denote-title-format-andstr "and")
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-template 'default)
  :config
  (citar-denote-mode 1)

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
           :reference-regex "^reference\\s-*:")))

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
  (advice-add 'citar-denote--add-reference :override #'kb/citar-denote--add-reference))

;;;; Darkroom
(use-package darkroom
  :bind
  ( :map krisb-toggle-keymap
    ("d" . darkroom-mode)
    ("D" . darkroom-tentative-mode))
  :custom
  (darkroom-text-scale-increase 1.3))

;;;; Typewriter-roll-mode
(use-package typewriter-roll-mode
  :bind ( :map krisb-toggle-keymap
          ("t" . typewriter-roll-mode)))

(provide 'org-notes-rcp)
;;; org-notes-rcp.el ends here
