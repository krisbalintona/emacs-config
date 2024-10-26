;;; krisb-themes-ext.el --- Manage switching/toggling of themes  -*- lexical-binding: t; -*-

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

;; Bespoke code for switching between two themes on-demand according to
;; time-of-day.

;;; Code:
(require 'color)

;;; Theme switcher
;;;; Variables
(defgroup krisb-themes-ext ()
  "Extensions for using themes."
  :group 'faces
  :prefix "krisb-themes-ext-")


(defcustom krisb-themes-ext-dark nil
  "The chosen dark theme."
  :type 'symbol)

(defcustom krisb-themes-ext-light nil
  "The chosen light theme."
  :type 'symbol)

;;;; Function definitions
(defun krisb-themes-ext-ensure-themes-loaded ()
  "Ensure that the themes in `krisb-themes-ext-list' are loaded."
  (unless (or (custom-theme-p krisb-themes-ext-dark)
              (custom-theme-p krisb-themes-ext-light))
    (load-theme krisb-themes-ext-dark t t)
    (load-theme krisb-themes-ext-light t t)))

(defun krisb-themes-ext-proper-load-theme-light ()
  "Properly load `krisb-theme-light' theme.
Also disables its light counterpart."
  (interactive)
  (disable-theme krisb-themes-ext-dark)
  (load-theme krisb-themes-ext-light t))

(defun krisb-themes-ext-proper-load-theme-dark ()
  "Properly load `krisb-theme-dark' theme.
Also disables its dark counterpart."
  (interactive)
  (disable-theme krisb-themes-ext-light)
  (load-theme krisb-themes-ext-dark t))

(defun krisb-theme-ext-enable-theme-time-of-day (day-start night-start)
  "Enables the theme based on time of day.
If daytime, call `krisb-themes-ext-proper-load-theme-light'. If
nighttime, call `krisb-themes-ext-proper-load-theme-dark'.

Nighttime begins at NIGHT-START hour and daytime begins at DAY-START
hour."
  (interactive (list 8 19))
  (let ((hour (string-to-number (format-time-string "%H"))))
    ;; Dark theme between NIGHT-START and DAY-START
    (if (or (<= night-start hour) (<= hour day-start))
        (krisb-themes-ext-proper-load-theme-dark)
      (krisb-themes-ext-proper-load-theme-light))))

;;;; Command
(defun krisb-themes-ext-theme-switcher ()
  "Switch between the light and dark themes."
  (interactive)
  (krisb-themes-ext-ensure-themes-loaded)
  (let* ((current (car custom-enabled-themes)))
    (cond ((equal krisb-themes-ext-light current)
           (krisb-themes-ext-proper-load-theme-dark))
          ((equal krisb-themes-ext-dark current)
           (krisb-themes-ext-proper-load-theme-light)))))

;;; Window transparency toggle
(add-to-list 'default-frame-alist '(alpha-background . 100))
(defun krisb-themes-ext-toggle-window-transparency (&optional arg)
  "Toggle the value of `alpha-background'.

Toggles between 100 and 72 by default. Can choose which value to
change to if called with ARG."
  (interactive "P")
  (let ((transparency (pcase arg
                        ((pred numberp) arg)
                        ((pred car) (read-number "Change the transparency to which value (0-100)? "))
                        (_
                         (pcase (frame-parameter nil 'alpha-background)
                           (72 100)
                           (100 72)
                           (t 100))))))
    (set-frame-parameter nil 'alpha-background transparency)))

;;; Provide
(provide 'krisb-themes-ext)
;;; krisb-themes-ext.el ends here
