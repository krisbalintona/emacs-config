;;; org-blogging-rcp.el --- Blogging with Org-mode   -*- lexical-binding: t; -*-

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

;; Everything necessary for creating static websites using Hugo in org-mode.

;;; Code:
(require 'cl)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Ox-hugo

;;;;; Magic keyword management
(with-eval-after-load 'ox-hugo
  (defun kb/find-blog-files-org ()
    "Return a list of org files which are within the blog subdirectory
of `krisb-notes-directory'."
    (directory-files-recursively krisb-blog-directory ""))

  (defun kb/org-hugo--add-tag-maybe ()
    "Add a FILETAGS value if necessary. Right now I only need the
draft tag for nodes with a value of true for hugo_draft."
    (when (and (not (active-minibuffer-window))
               (member buffer-file-name (kb/find-blog-files-org))
               (assoc "TITLE" (org-collect-keywords '("title"))))
      (save-excursion
        (beginning-of-buffer)
        (let* ((keywords '("filetags" "hugo_draft"))
               (collected-keywords (org-collect-keywords keywords))
               (hugo_draft_value (cadr (assoc "HUGO_DRAFT" collected-keywords)))
               (filetags_value (cadr (assoc "FILETAGS" collected-keywords))))
          (pcase hugo_draft_value
            ("false"
             (when (stringp filetags_value)
               (cond
                ((featurep 'denote)
                 (let ((tags-list (split-string filetags_value ":" 'omit-nulls)))
                   (if (< 0 (length tags-list))
                       (denote--rewrite-keywords (buffer-file-name)
                                                 (denote-keywords-sort
                                                  (remove "draft" tags-list))
                                                 'org)
                     (delete-region (re-search-forward (denote--keywords-key-regexp 'org) nil t 1) (pos-eol)))))
                ((featurep 'org-roam)
                 (org-roam-tag-remove '("draft"))))))
            ("true"
             (cond
              ((featurep 'denote)
               (denote--rewrite-keywords (buffer-file-name)
                                         (denote-keywords-sort
                                          (delete-dups
                                           (append (list "draft")
                                                   (split-string filetags_value ":" 'omit-nulls))))
                                         'org))
              ((featurep 'org-roam)
               (org-roam-tag-add '("draft"))))))))))
  ;; FIXME 2022-06-01: Point isn't preserved if added to `before-save-hook'
  (add-hook 'before-save-hook #'kb/org-hugo--add-tag-maybe)

  ;; Set the value of the hugo_bundle keyword (for blog post org files) if it is
  ;; empty. Inspired by `vulpea-project-update-tag'
  (defun kb/org-hugo--add-hugo-metadata-maybe ()
    "Update the export_file_name and hugo_draft file properties in
the current hugo buffer if they do not exist."
    (when (and (not (active-minibuffer-window))
               (member buffer-file-name (kb/find-blog-files-org))
               (assoc "TITLE" (org-collect-keywords '("title"))))
      (save-excursion
        (let* ((keywords '("export_file_name" "hugo_draft"))
               (keywords (mapcar #'(lambda (elt) (upcase elt)) keywords))
               (collected-keywords (org-collect-keywords keywords))
               (non-existent-keywords (cl-remove-if
                                       (lambda (keyword) (assoc keyword collected-keywords))
                                       keywords))
               (empty-keywords (cl-remove-if-not
                                (lambda (keyword) (string= (cadr (assoc keyword collected-keywords)) ""))
                                keywords))
               (default-export-file-name "index")
               (default-hugo-draft "true")
               (new-value))
          (dolist (keyword (append non-existent-keywords empty-keywords))
            (setq new-value (pcase keyword
                              ("EXPORT_FILE_NAME" default-export-file-name)
                              ("HUGO_DRAFT" default-hugo-draft)))
            (kb/org-set-keyword keyword new-value))))))

  ;; Org-export all files in an org-roam subdirectory. Modified from
  ;; https://sidhartharya.me/exporting-org-roam-notes-to-hugo/
  (defun kb/org-hugo-export-all ()
    "Export all org-roam files to Hugo in my blogging directory."
    (interactive)
    (when (featurep 'org-roam)
      (require 'org-roam)
      (org-roam-update-org-id-locations)) ; Necessary for id's to be recognized for exports
    ;; First delete all old posts; only works if `kb/org-hugo-bundle-workflow'
    ;; is non-nil. Useful for if I renamed a node.
    (when-let ((kb/org-hugo-bundle-workflow)
               (subdirs (cdr (ffap-all-subdirs (file-name-concat org-hugo-base-dir "content/" org-hugo-section) 1))))
      (dolist (post-dir subdirs "Deleted old posts")
        (delete-directory post-dir t t)))
    (let ((post-count 0))
      (dolist (file (cl-remove-if-not
                     (lambda (file)
                       ;; Don't look at files without the title,
                       ;; hugo_publishdate, or hugo_draft keywords or with an
                       ;; empty value. The criterion of having a
                       ;; hugo_publishdate is ignored if the value of hugo_draft
                       ;; is true. The criteria of a hugo_publishdate and
                       ;; hugo_draft are ignored if there is a "series" filetag.
                       (with-temp-buffer
                         (delay-mode-hooks (org-mode))
                         (when file (insert-file-contents file))
                         (let* ((keywords '("title" "filetags" "hugo_publishdate" "hugo_draft" "hugo_section"))
                                (collected-keywords (org-collect-keywords keywords))
                                (title-p (assoc "TITLE" collected-keywords))
                                (taxonomy-p (assoc "HUGO_SECTION" collected-keywords)))
                           ;; If hugo_draft is false, then the hugo_publishdate
                           ;; should exist and have a value. If hugo_draft doesn't
                           ;; exist, then it'll return nil.
                           (cond
                            ((and title-p taxonomy-p))
                            (title-p
                             (pcase (cadr (assoc "HUGO_DRAFT" collected-keywords))
                               ("false"
                                (let* ((publish-pair (assoc "HUGO_PUBLISHDATE" collected-keywords))
                                       (date (cadr publish-pair)))
                                  (and (stringp date)
                                       (not (string= date "")))))
                               ("true" t)))))))
                     (kb/find-blog-files-org)))
        (condition-case nil
            (let ((inhibit-message t))  ; Don't show the messages in Echo area
              (with-current-buffer (find-file-noselect file)
                (read-only-mode -1)
                (kb/org-hugo--add-hugo-metadata-maybe)
                (kb/org-hugo--add-tag-maybe)
                (kb/format-buffer-indentation)
                (org-hugo-export-wim-to-md)
                (setq post-count (1+ post-count))

                (unless (member (get-buffer (buffer-name)) (buffer-list)) ; Kill buffer unless it already exists
                  (kill-buffer))))
          (error (message "[kb/org-hugo-export-all]: error exporting %s" file))))
      (message "Done - Exported %s blog notes!" post-count))))

;;;;; Fix TOC including tags
(with-eval-after-load 'ox-hugo
  (defun kb/org-hugo--build-toc (info &optional n scope local)
    "Return table of contents as a string.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is a positive integer
specifying the depth of the table.

When optional argument SCOPE is non-nil, build a table of
contents according to the specified element.

When optional argument LOCAL is non-nil, build a table of
contents according to the current heading.

My version of this function has tags processed through
`org-hugo-tag-processing-functions' before they are added to the
TOC."
    (let* ((toc-heading
            (unless local
              (format "\n<div class=\"heading\">%s</div>\n"
                      (org-html--translate "Table of Contents" info))))
           (current-level nil)
           (toc-items
            (mapconcat
             (lambda (heading)
               (let* ((level-raw (org-export-get-relative-level heading info))
                      (level (if scope
                                 (let* ((current-level-inner
                                         (progn
                                           (unless current-level
                                             (setq current-level level-raw))
                                           current-level))
                                        (relative-level
                                         (1+ (- level-raw current-level-inner))))
                                   ;; (message (concat "[ox-hugo build-toc DBG] "
                                   ;;                  "current-level-inner:%d relative-level:%d")
                                   ;;          current-level-inner relative-level)
                                   relative-level)
                               level-raw))
                      (indentation (make-string (* 4 (1- level)) ?\s))
                      (todo (and (org-hugo--plist-get-true-p info :with-todo-keywords)
                                 (org-element-property :todo-keyword heading)))
                      (todo-str (if todo
                                    (concat (org-hugo--todo todo info) " ")
                                  ""))
                      (heading-num-list (org-export-get-headline-number heading info))
                      (number (if heading-num-list
                                  ;; (message "[ox-hugo TOC DBG] heading-num-list: %S" heading-num-list)
                                  (org-hugo--get-heading-number heading info :toc)
                                ""))
                      (toc-entry
                       (format "[%s%s](#%s)"
                               todo-str
                               (org-export-data-with-backend
                                (org-export-get-alt-title heading info)
                                (org-export-toc-entry-backend 'hugo)
                                info)
                               (org-hugo--get-anchor heading info)))
                      (tags (and (plist-get info :with-tags)
                                 (not (eq 'not-in-toc (plist-get info :with-tags)))
                                 (let ((tags (org-export-get-tags heading info)))
                                   ;; I added these two lines to have tags
                                   ;; properly processed
                                   (dolist (fn org-hugo-tag-processing-functions tags)
                                     (setq tags (funcall fn tags info)))
                                   (and tags
                                        (format ":%s:"
                                                (mapconcat #'identity tags ":")))))))
                 ;; (message "[ox-hugo build-toc DBG] level:%d, number:%s" level number)
                 ;; (message "[ox-hugo build-toc DBG] indentation: %S" indentation)
                 ;; (message "[ox-hugo build-toc DBG] todo: %s | %s" todo todo-str)
                 (concat indentation "- " number toc-entry tags)))
             (org-export-collect-headlines info n scope)
             "\n"))                       ;Newline between TOC items
           ;; Remove blank lines from in-between TOC items, which can
           ;; get introduced when using the "UNNUMBERED: t" heading
           ;; property.
           (toc-items (org-string-nw-p
                       (replace-regexp-in-string "\n\\{2,\\}" "\n" toc-items))))
      ;; (message "[ox-hugo build-toc DBG] toc-items:%s" toc-items)
      (when toc-items
        (let ((toc-classes '("toc" "ox-hugo-toc"))
              ;; `has-section-numbers' is non-nil if section numbers are
              ;; present for even one heading.
              (has-section-numbers (string-match-p "^\\s-*\\-\\s-<span class=\"section\\-num\"" toc-items)))
          (when has-section-numbers
            (push "has-section-numbers" toc-classes))
          (when local
            (push "local" toc-classes))
          (concat (format "<div class=\"%s\">\n" (string-join (reverse toc-classes) " "))
                  (unless (org-hugo--plist-get-true-p info :hugo-goldmark)
                    "<div></div>\n") ;This is a nasty workaround till Hugo/Blackfriday support
                  toc-heading    ;wrapping Markdown in HTML div's.
                  "\n"
                  toc-items ;https://github.com/kaushalmodi/ox-hugo/issues/93
                  "\n\n"
                  "</div>\n"
                  ;; Special comment that can be use to filter out the TOC
                  ;; from .Summary in Hugo templates.
                  ;;
                  ;;     {{ $summary_splits := split .Summary "<!--endtoc-->" }}
                  ;;     {{ if eq (len $summary_splits) 2 }}
                  ;;         <!-- If that endtoc special comment is present, output only the part after that comment as Summary. -->
                  ;;         {{ index $summary_splits 1 | safeHTML }}
                  ;;     {{ else }}
                  ;;         <!-- Print the whole Summary if endtoc special comment is not found. -->
                  ;;         {{ .Summary }}
                  ;;     {{ end }}
                  "<!--endtoc-->\n")))))
  (advice-add 'org-hugo--build-toc :override #'kb/org-hugo--build-toc))

(provide 'org-blogging-rcp)
;;; org-blogging-rcp.el ends here
