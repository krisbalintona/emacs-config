;;; custom-directories-rcp.el --- Bespoke directory and file definitions  -*- lexical-binding: t; -*-

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

;; These are directory and file definitions that I use frequently enough to
;; define.

;;; Code:
(require 'cl-seq)

;;; For org and org-agenda
(defvar kb/org-dir (expand-file-name "org-database" "~/Documents"))
(defvar kb/notes-dir (expand-file-name "notes" kb/org-dir))
(defvar kb/blog-dir (expand-file-name "blog" kb/notes-dir))
(defvar kb/agenda-dir (expand-file-name "agenda" kb/org-dir))
(defvar kb/agenda-main-todo-file (expand-file-name "todo.org" kb/agenda-dir))

(defvar kb/all-org-dir-files (cl-remove-if
                              (lambda (it)
                                (string-match-p (rx "archive.org") it))
                              (directory-files-recursively kb/org-dir ".org$")))
(defvar kb/all-agenda-dir-files (cl-remove-if
                                 (lambda (it)
                                   (string-match-p (rx "archive.org") it))
                                 (directory-files-recursively kb/agenda-dir ".org$")))
(defvar kb/all-agenda-dir-files-minus-inbox (cl-remove-if
                                             (lambda (it)
                                               (string-match-p (rx "archive.org") it)
                                               (string-match-p (rx "inbox.org") it))
                                             (directory-files-recursively kb/agenda-dir ".org$")))

;;; For my frequently visited directories and files
(defvar kb/library-dir (concat kb/org-dir "library"))
(defvar kb/emacs-etc-config-file (concat user-emacs-directory "configs/etc-config.org"))
(defvar kb/emacs-config-dir (concat user-emacs-directory "configs/"))

(defvar kb/dot-config-dir "~/.config/")
(defvar kb/wm-config-file (concat kb/dot-config-dir "awesome/rc.lua"))

;;; Other
(defvar kb/bib-files
  (list (expand-file-name "master-lib.bib" kb/org-dir)))

(provide 'custom-directories-rcp)
;;; custom-directories-rcp.el ends here
