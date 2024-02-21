;;; dungeons-and-dragons-rcp.el --- Bespoke Dungeons and Dragons features  -*- lexical-binding: t; -*-

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

;; My custom configuration for playing Dungeons and Dragons.

;;; Code:
(require 'ol)
(require 'org-export-rcp)

;;;; Bespoke keywords
(defcustom kb/dnd-keywords
  '(("character" . '((((background light)) (:foreground "PaleVioletRed3"))
                     (((background dark)) (:foreground "PaleVioletRed1"))))
    ("place" . '((((background light)) (:foreground "DeepSkyBlue"))
                 (((background dark)) (:foreground "DeepSkyBlue"))))
    ("designation" . '((((background light)) (:background "orange2"))
                       (((background dark)) (:background "orange1")))))
  "Keywords used for for playing Dungeons and Dragons.
Is an alist from keyword, as a string, to the foreground of the
link whose path is that keyword.")

(defface kb/dnd-faces-missing '((((background light)) (:strike-through "red"))
                                (((background dark)) (:strike-through "red")))
  "Face used when keyword does not exist.
Is the fallback face when using a keyword that does not exist in
`kb/dnd-keywords.'")

(defun kb/dnd-link-face (keyword)
  "Calculate face used with `dnd' org link type.
KEYWORD is a string that should be present in `kb/dnd-keywords'.
If not, uses the face denoted by `kb/dnd-faces-missing'.

Used in `org-link-set-parameters'."
  (or (cdr (assoc keyword kb/dnd-keywords))
      'kb/dnd-faces-missing))

(defun kb/dnd-link-complete (&optional _arg)
  "Function for completions with `dnd' org link type.
Used in `org-link-set-parameters'."
  (concat "dnd:"
          (completing-read "Which keyword?: "
                           (s-split-words (key-description kb/dnd-keywords)))))

(org-link-set-parameters "dnd"
                         :face #'kb/dnd-link-face
                         :complete #'kb/dnd-link-complete)

(provide 'dungeons-and-dragons-rcp)
;;; dungeons-and-dragons-rcp.el ends here
