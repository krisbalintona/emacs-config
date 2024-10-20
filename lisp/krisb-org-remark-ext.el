;;; krisb-org-remark-ext.el --- Extensions for org-remark  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: multimedia, convenience

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

;; Extensions for org-remark.

;;; Code:
(require 'org-remark)
(require 'transient)

;;; My `org-remark' pens
(defgroup org-remark-ext nil
  "Extension for org-remark."
  :group 'org-remark)

;; Sets up `org-remark' faces according to the following schema:
;; - Resonant (red)
;; - Thesis (yellow)
;; - Detail (blue)
;; - Outline (green)
;; - External (purple/magenta)
(defface krisb-org-remark-resonant-face
  `((t :background "red"))
  "Face for resonant annotations."
  :group 'org-remark-ext)
(defface krisb-org-remark-resonant-minor-face
  `((t :underline (:color "red" :style wave)))
  "Face for less resonant (underlined) annotations."
  :group 'org-remark-ext)

(defface krisb-org-remark-thesis-face
  `((t :background "yellow"))
  "Face for thesis annotations."
  :group 'org-remark-ext)
(defface krisb-org-remark-thesis-minor-face
  `((t :underline (:color "yellow" :style wave)))
  "Face for less thesis (underlined) annotations."
  :group 'org-remark-ext)

(defface krisb-org-remark-detail-face
  `((t :background "deep sky blue"))
  "Face for detail annotations."
  :group 'org-remark-ext)
(defface krisb-org-remark-detail-minor-face
  `((t :underline (:color "deep sky blue" :style wave)))
  "Face for less detail (underlined) annotations."
  :group 'org-remark-ext)

(defface krisb-org-remark-outline-face
  `((t :background "lawn green"))
  "Face for outline annotations."
  :group 'org-remark-ext)
(defface krisb-org-remark-outline-minor-face
  `((t :underline (:color "lawn green" :style wave)))
  "Face for less outline (underlined) annotations."
  :group 'org-remark-ext)

(defface krisb-org-remark-external-face
  `((t :background "magenta"))
  "Face for \"external\" annotations."
  :group 'org-remark-ext)
(defface krisb-org-remark-external-minor-face
  `((t :underline (:color "magenta" :style wave)))
  "Face for less \"external\" (underlined) annotations."
  :group 'org-remark-ext)

(org-remark-create "resonant"
                   'krisb-org-remark-resonant-face
                   `(CATEGORY "resonant" help-echo "Annotation that resonates with me."))
(org-remark-create "resonant-underline"
                   'krisb-org-remark-resonant-minor-face
                   `(CATEGORY "resonant" help-echo "Annotation that resonates with me but I don't want to be as noticeable."))

(org-remark-create "thesis"
                   'krisb-org-remark-thesis-face
                   `(CATEGORY "thesis" help-echo "Annotation that denotes something relevant to a thesis."))
(org-remark-create "thesis-underline"
                   'krisb-org-remark-thesis-minor-face
                   `(CATEGORY "thesis" help-echo "Annotation that denotes something relevant to a thesis but I don't want to be as noticeable."))

(org-remark-create "detail"
                   'krisb-org-remark-detail-face
                   `(CATEGORY "detail" help-echo "Annotation that denotes a notable detail."))
(org-remark-create "detail-underline"
                   'krisb-org-remark-detail-minor-face
                   `(CATEGORY "detail" help-echo "Annotation that denotes a notable detail but I don't want to be as noticeable."))

(org-remark-create "outline"
                   'krisb-org-remark-outline-face
                   `(CATEGORY "outline" help-echo "Annotation that foreshadows structure or main idea(s)."))
(org-remark-create "outline-underline"
                   'krisb-org-remark-outline-minor-face
                   `(CATEGORY "outline" help-echo "Annotation that foreshadows structure or main idea(s) but I don't want to be as noticeable."))

(org-remark-create "external"
                   'krisb-org-remark-external-face
                   `(CATEGORY "external" help-echo "Annotation that resonates with me but is external to the text."))
(org-remark-create "external-underline"
                   'krisb-org-remark-external-minor-face
                   `(CATEGORY "external" help-echo "Annotation that resonates with me but is external to the text but I don't want to be as noticeable."))

;;;###autoload
(transient-define-prefix krisb-org-remark-mark-transient ()
  "Transient menu for my pre-defined `org-remark' pens."
  [["Resonant"
    ("r" "Highlight" org-remark-mark-resonant)
    ("R" "Underline" org-remark-mark-resonant-underline)]
   ["Thesis"
    ("t" "Highlight" org-remark-mark-thesis)
    ("T" "Underline" org-remark-mark-thesis-underline)]]
  [["Detail"
    ("d" "Highlight" org-remark-mark-detail)
    ("D" "Underline" org-remark-mark-detail-underline)]
   ["Outline"
    ("o" "Highlight" org-remark-mark-outline)
    ("O" "Underline" org-remark-mark-outline-underline)]
   ["External"
    ("e" "Highlight" org-remark-mark-external)
    ("E" "Underline" org-remark-mark-external-underline)]])

(provide 'krisb-org-remark-ext)
;;; krisb-org-remark-ext.el ends here
