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

(provide 'org-notes-rcp)
;;; org-notes-rcp.el ends here
