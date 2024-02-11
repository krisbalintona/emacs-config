;;; external-programs-rcp.el --- External programs   -*- lexical-binding: t; -*-

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

;; Make sure any external programs (e.g. pip and python) are -Sed here and
;; are available in my PATH.

;;; Code:

;;;; Python
(unless (executable-find "python")
  (system-packages-install "python"))
(unless (executable-find "pip")
  (system-packages-install "pip"))

;;;; Javascript
(unless (executable-find "npm")
  (system-packages-install "npm"))
(unless (executable-find "yarn")
  (system-packages-install "yarn"))

;;;; Rust
(unless (executable-find "rustc")
  (system-packages-install "rust"))
(unless (executable-find "cargo")
  (system-packages-install "cargo"))
(unless (executable-find "watchexec")
  (system-packages-install "watchexec"))

;;;; Java
(unless (executable-find "java")
  (system-packages-install "jre-openjdk"))

;;;; Racket
(unless (executable-find "racket")
  (system-packages-install "racket"))

(provide 'external-programs-rcp)
;;; external-programs-rcp.el ends here
