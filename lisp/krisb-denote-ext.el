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

;;; Standardizing note front-mattert
(defun krisb-org-set-keyword (keyword value)
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
        (beginning-of-buffer)
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
         (sorted-keywords (denote-keywords-sort (copy-list cur-keywords))))
    (denote--rewrite-keywords f sorted-keywords file-type)
    ;; Add empty filetags property if one isn't already present
    (unless (krisb-note-buffer-prop-get "filetags")
      (beginning-of-buffer)
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

;;;###autoload
(defun krisb-denote-standardize-front-matter ()
  (interactive)
  (let ((existing-buffers (buffer-list)))
    (save-mark-and-excursion
      (dolist (file (denote-directory-files-matching-regexp (rx (literal ".org") eol)))
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

;;; Provide
(provide 'krisb-denote-ext)
;;; krisb-denote-ext.el ends here
