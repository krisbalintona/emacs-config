;; -*- lexical-binding: t; -*-

;;; krisb-wombag-ext.el --- Wombag extensions        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: multimedia, comm

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

;; Extensions for Wombag.el.

;;; Code:
(require 'wombag)
(require 'wombag-show)
(require 'ol)

;;; Custom wombag org-link
(defun krisb-wombag-org-store-link ()
  "Stores link to the current wombag entry."
  (when (eq major-mode 'wombag-show-mode)
    (let* ((title (alist-get 'title (buffer-local-value 'wombag-show-entry (current-buffer))))
           (id (alist-get 'id (buffer-local-value 'wombag-show-entry (current-buffer))))
           (pt (save-restriction (widen) (point)))
           (url (concat "wombag:" (number-to-string id) "::" (number-to-string pt)))
           (desc (format "%s (at point %s)" title pt)))
      (org-link-store-props
       :type "wombag"
       :link url
       :description desc))))

(defun krisb-wombag-org-follow-link (path)
  "Open wombag entry.
The PATH is formatted in the following way:
- \"wombag:\"
- a wombag entry ID
- \"::\"
- an optional number that represents the point in the buffer."
  (let* ((option (and (string-match "::\\(.*\\)\\'" path)
                      (match-string 1 path)))
         (id (string-to-number
              (if (not option)
                  path
                (substring path 0 (match-beginning 0)))))
         (pt (when option
               (string-to-number (substring path (+ 2 (match-beginning 0))))))
         (entry (car
                 (wombag-db-get-entries
                  `[:select ,(vconcat (buffer-local-value 'wombag-search-columns (current-buffer))) :from items :where (= id ,id)]
                  (buffer-local-value 'wombag-search-columns (current-buffer))))))
    (with-current-buffer (wombag-show-entry entry)
      (when pt (goto-char pt)))))

(org-link-set-parameters
 "wombag"
 :follow #'krisb-wombag-org-follow-link
 :store #'krisb-wombag-org-store-link)

;;; Comparability with org-remark
(defun krisb-org-remark-wombag-find-file-name ()
  "Return the ID of the entry.
It assumes the buffer is a `wombag-show-mode' buffer and has a variable
`wombag-show-entry' value.

This function is meant to be set to hook
`org-remark-source-find-file-name-functions'."
  (when (eq major-mode 'wombag-show-mode)
    (concat "wombag:" (number-to-string (alist-get 'id wombag-show-entry)))))

(defun krisb-org-remark-wombag-highlight-link-to-source (filename point)
  "Return org-link pointing to the source wombag entry (i.e. FILENAME).
It assumes the major mode is `wombag-show-mode'.

Saves the POINT in buffer.

 This function is meant to be set to hook
`org-remark-highlight-link-to-source-functions'."
  (when (eq major-mode 'wombag-show-mode)
    (let* ((file-title filename)
           (id (string-to-number (cadr (string-split filename ":"))))
           (title (or (caar (wombag-db-query `[:select title :from items :where (= id ,id)]))
                      "UNTITLED")) ; NOTE 2024-09-24: This is what `wombag' currently titles its untitled notes
           (pt (number-to-string point)))
      (concat "[[" file-title "::" pt "][" title " (at point " pt ")" "]]"))))

(declare-function org-remark-auto-on "org-remark")
(define-minor-mode krisb-org-remark-wombag-mode
  "Enable Org-remark to work with Wombag."
  :global t
  :group 'org-remark-wombag
  (if krisb-org-remark-wombag-mode
      ;; Enable
      (progn
        (add-hook 'wombag-show-mode-hook #'org-remark-auto-on)
        (add-hook 'org-remark-source-find-file-name-functions
                  #'krisb-org-remark-wombag-find-file-name)
        (add-hook 'org-remark-highlight-link-to-source-functions
                  #'krisb-org-remark-wombag-highlight-link-to-source))
    ;; Disable
    (remove-hook 'wombag-show-mode-hook #'org-remark-auto-on)
    (remove-hook 'org-remark-source-find-file-name-functions
                 #'krisb-org-remark-wombag-find-file-name)
    (remove-hook 'org-remark-highlight-link-to-source-functions
                 #'krisb-org-remark-wombag-highlight-link-to-source)))

;;; Provide
(provide 'krisb-wombag-ext)
;;; krisb-wombag-ext.el ends here
