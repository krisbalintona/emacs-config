;;; org-agenda-other-rcp.el --- Other org-agenda configs  -*- lexical-binding: t; -*-

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

;; Other org-agenda packages.

;;; Code:

;;;; Custom org-tags-view org-link type
(with-eval-after-load 'org
  (defun kb/org-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
  With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view (null current-prefix-arg) tag))

  (org-add-link-type "tag" 'kb/org-tag-link))

(provide 'org-agenda-other-rcp)
;;; org-agenda-other-rcp.el ends here
