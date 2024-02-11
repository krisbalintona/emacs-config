;;; programming-profiling-and-debug-rcp.el --- Profiling and debugging Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona;;; Code: <krisbalintona@gmail.com>
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

;; Profile Emacs startup and Emacs performance as well as debugging startup.

;;; Code:
(require 'use-package-rcp)

;;; Emacs-startup-profiler
;; Profile my startup time without leaving Emacs
(use-package esup
  :custom
  (esup-user-init-file (expand-file-name "init.el" user-emacs-directory))
  (esup-depth 10)
  (esup-child-max-depth 10)
  )

;;; Explain-pause-mode
;; Profile what's causing your Emacs to slow down
(use-package explain-pause-mode
  :ensure (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  )

;;; Bug-hunter
;; Easy way to see if there is an error in your config files
;; NOTE: Not sure if this looks through literate configs?
(use-package bug-hunter)

(provide 'programming-profiling-and-debug-rcp)
;;; programming-profiling-and-debug-rcp.el ends here
