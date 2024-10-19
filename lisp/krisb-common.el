;;; krisb-common.el --- Common variables and functions  -*- lexical-binding: t; -*-

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

;; Variables and functions I use throughout my Emacs config.

;;; Code:

;;; Variables

;;;; Org

(defvar krisb-org-directory (expand-file-name "org-database" "~/Documents")
  "The directory holding my org files.
Meant to be used as the value of `org-directory'.")

(defvar krisb-notes-directory (expand-file-name "notes" krisb-org-directory)
  "My notes directory.")

(defvar krisb-blog-directory (expand-file-name "blog" krisb-notes-directory)
  "The directory for my pre-export blog files.")

(defvar krisb-org-agenda-directory (expand-file-name "agenda" krisb-org-directory)
  "The directory holding my main org-agenda files.")

(defvar krisb-org-agenda-main-file (expand-file-name "todo.org" krisb-org-agenda-directory)
  "My main org-agenda file.")

(defvar krisb-org-agenda-directory-files (cl-remove-if
                                          (lambda (f)
                                            (string-match-p (rx "archive.org") f))
                                          (directory-files-recursively krisb-org-agenda-directory ".org$"))
  "A list of all org and org_archive files in `krisb-org-directory'.")

(defvar krisb-bibliography-files (list (expand-file-name "master-lib.bib" krisb-org-directory))
  "A list of my bibliography (.bib) files.")

;;; Leader keymaps

;;; Leader keys
(defvar-keymap krisb-note-keymap
  :doc "Prefix for my note-taking needs.")
(bind-key "C-c n" krisb-note-keymap 'global-map)

(defvar-keymap krisb-lsp-keymap
  :doc "Prefix for lsp-related commands.")
(with-eval-after-load 'lsp-mode
  (bind-key "C-c l" krisb-lsp-keymap 'lsp-mode-map))

(defvar-keymap krisb-file-keymap
  :doc "Prefix for file-related commands.")
(bind-key "C-c f" krisb-file-keymap 'global-map)

(defvar-keymap krisb-yank-keymap
  :doc "Prefix for yanking stuff.")
(bind-key "C-c i" krisb-yank-keymap 'global-map)

(defvar-keymap krisb-open-keymap
  :doc "Prefix for opening various hings.")
(bind-key "C-c o" krisb-open-keymap 'global-map)

(defvar-keymap krisb-toggle-keymap
  :doc "Prefix for toggling stuff.")
(bind-key "C-M-s-t" krisb-toggle-keymap 'global-map)


;;; Provide
(provide 'krisb-common)
;;; krisb-common.el ends here
