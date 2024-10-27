;;; web-browsing-rcp.el --- Browsing the web         -*- lexical-binding: t; -*-

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

;; Configurations related to rendering web pages and browsing the web.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Browse-url
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function 'browse-url-generic) ; Primary browser
  (browse-url-secondary-browser-function 'eww-browse-url) ; Secondary browser
  (browse-url-generic-program (executable-find "firefox"))
  (browse-url-generic-args (list "--new-window")) ; Opens new Firefox window (i.e. "frame")
  (browse-url-new-window-flag nil) ; NOTE 2023-07-08: Not sufficient for Firefox to open new frame
  (browse-url-handlers
   `((,(rx (literal "file://") (1+ anychar) (literal ".html")) .
      (lambda (url &rest args)
        (let ((browse-url-generic-args (remove "--new-window" browse-url-generic-args)))
          (funcall browse-url-browser-function url args)))))))

;;; Eww
;; Emacs' web browser
(use-package eww
  :ensure nil
  :custom
  (eww-restore-desktop nil)
  (eww-desktop-remove-duplicates t)     ; Don't duplicate pages in history
  (eww-header-line-format nil)
  (eww-search-prefix "https://duckduckgo.com/html/?q=") ; Use duckduckgo search engine
  (eww-download-directory (no-littering-expand-var-file-name "eww/downloads/")) ; Where to put downloads
  (eww-history-limit 150)
  (eww-browse-url-new-window-is-tab nil)
  (eww-form-checkbox-selected-symbol "[X]")
  (eww-form-checkbox-symbol "[ ]")
  (eww-auto-rename-buffer 'title))

(provide 'web-browsing-rcp)
;;; web-browsing-rcp.el ends here
