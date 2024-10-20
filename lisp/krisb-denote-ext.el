;;; krisb-denote-ext.el --- Denote extensions        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: lisp

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

;; Bespoke code to extend Denote.

;;; Code:
(require 'denote)
(require 'org-element)
(require 'denote-org-extras)

;;; Standardizing note front-matter
(defun krisb-org-set-keyword (keyword value)
  "Set org KEYWORD in current buffer to VALUE."
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

(defun krisb-note-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                             (point-max) t)
      (denote-trim-whitespace
       (buffer-substring-no-properties
        (match-beginning 1)
        (match-end 1))))))

(defun krisb-denote-insert-identifier-maybe ()
  (when (and (buffer-file-name) (denote-file-is-note-p (buffer-file-name)))
    (cond
     ;; ID doesn't exist
     ((not (krisb-note-buffer-prop-get "identifier"))
      (save-excursion
        (goto-char (point-min))
        ;; Move cursor until after the first of following
        ;; properties exists: filetags, date, or title
        (while (and (not (eobp))
                    (cond
                     ((krisb-note-buffer-prop-get "filetags")
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
                     ((krisb-note-buffer-prop-get "date")
                      (re-search-forward (rx bol "#+"
                                             (or "D" "d")
                                             (or "A" "a")
                                             (or "T" "t")
                                             (or "E" "e")
                                             ":")
                                         (point-max) t))
                     ((krisb-note-buffer-prop-get "title")
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
                    (krisb-note-buffer-prop-get "identifier")))
      (krisb-org-set-keyword "identifier" (denote-retrieve-filename-identifier (buffer-file-name)))))))
(defun krisb-denote-rearrange-keywords-maybe ()
  (let* ((f (buffer-file-name))
         (file-type (denote-filetype-heuristics f))
         (cur-keywords (seq-uniq (denote-retrieve-keywords-value f file-type)))
         (sorted-keywords (denote-keywords-sort (cl-copy-list cur-keywords))))
    (denote-rewrite-keywords f sorted-keywords file-type)
    ;; Add empty filetags property if one isn't already present
    (unless (krisb-note-buffer-prop-get "filetags")
      (goto-char (point-min))
      (while (and (not (eobp))
                  (cond
                   ((krisb-note-buffer-prop-get "date")
                    (re-search-forward (rx bol "#+"
                                           (or "D" "d")
                                           (or "A" "a")
                                           (or "T" "t")
                                           (or "E" "e")
                                           ":")
                                       (point-max) t))
                   ((krisb-note-buffer-prop-get "title")
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
(defun krisb-denote-ensure-title-space ()
  (save-excursion
    (goto-char (point-min))
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

;;;###autoload
(defun krisb-denote-standardize-front-matter ()
  (interactive)
  (let ((existing-buffers (buffer-list)))
    (save-mark-and-excursion
      (dolist (file (denote-directory-files (rx (literal ".org") eol)))
        ;; Export all the files
        (with-current-buffer (find-file-noselect file)
          (read-only-mode -1)
          (save-restriction
            (widen)
            (krisb-denote-insert-identifier-maybe)
            (krisb-denote-rearrange-keywords-maybe)
            (krisb-denote-ensure-title-space)
            (delete-trailing-whitespace))
          (with-demoted-errors "Error: %S"
            (denote-rename-file-using-front-matter file))
          ;; Kill buffer unless it already exists
          (unless (member (get-buffer (buffer-name)) existing-buffers)
            (kill-buffer)))))))

;;; Update link descriptions
(defun krisb-denote-update-link-descriptions (confirmp)
  "Recreate denote link descriptions in the current buffer.
If called with CONFIMP, then prompt user to confirm a replacement. When
interactively called, CONFIRMP is non-nil by default, flipping the value
with `prefix-arg'."
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
        (error (message "[krisb-denote-update-link-descriptions] Error encountered:  %s"
                        (error-message-string err))))))
  (message "Corrected links in %s"
           (propertize (denote-retrieve-front-matter-title-value
                        (buffer-file-name)
                        (denote-filetype-heuristics (buffer-file-name)))
                       'face 'denote-faces-title)))

(defun krisb-denote-update-link-descriptions-globally (dir confirmp)
  "Update the link description of all notes in DIR.
If CONFIRMP is non-nil, then prompt the user to confirm each
replacement."
  (interactive (list (denote-subdirectory-prompt) current-prefix-arg))
  (save-window-excursion
    (dolist (f (denote-directory-files (concat (file-name-nondirectory dir) "/") nil t))
      (save-excursion
        (let* ((live-buffer (get-file-buffer f)))
          (with-current-buffer (find-file-noselect f)
            (krisb-denote-update-link-descriptions confirmp))
          (unless live-buffer (kill-buffer live-buffer))))))
  (message "Updated all links in %s!" dir))

;;; Provide
(provide 'krisb-denote-ext)
;;; krisb-denote-ext.el ends here
