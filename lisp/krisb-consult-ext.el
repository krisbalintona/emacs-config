;;; krisb-consult-ext.el --- Extensions for Consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: help, tools

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

;; Bespoke extensions or Consult.

;;; Code:

;;; `consult-buffer' sources
(defvar krisb-consult-buffer--dired-source
  (list :name     "Dired"
        :category 'buffer
        :narrow   ?d
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    'consult--buffer-state
        :action   'consult--buffer-action
        :items (lambda ()
                 (mapcar #'buffer-name
                         (seq-filter
                          (lambda (x)
                            (eq (buffer-local-value 'major-mode x) 'dired-mode))
                          (buffer-list))))))
(add-to-list 'consult-buffer-sources #'krisb-consult-buffer--dired-source 'append)

(defvar krisb-consult-buffer--info-source
  (list :name     "Info"
        :category 'buffer
        :narrow   ?i
        :face     'info-title-1
        :history  'buffer-name-history
        :state    'consult--buffer-state
        :action   'consult--buffer-action
        :items (lambda ()
                 (mapcar #'buffer-name
                         (seq-filter
                          (lambda (x)
                            (eq (buffer-local-value 'major-mode x) 'Info-mode))
                          (buffer-list))))))
(add-to-list 'consult-buffer-sources #'krisb-consult-buffer--info-source 'append)

(defvar krisb-consult-buffer--customize-source
  (list :name     "Customize"
        :category 'buffer
        :narrow   ?c
        :face     'custom-group-tag
        :history  'buffer-name-history
        :state    'consult--buffer-state
        :action   'consult--buffer-action
        :items (lambda ()
                 (mapcar #'buffer-name
                         (seq-filter
                          (lambda (x)
                            (eq (buffer-local-value 'major-mode x) 'Custom-mode))
                          (buffer-list))))))
(add-to-list 'consult-buffer-sources #'krisb-consult-buffer--customize-source 'append)

;;; Provide
(provide 'krisb-consult-ext)
;;; krisb-consult-ext.el ends here
