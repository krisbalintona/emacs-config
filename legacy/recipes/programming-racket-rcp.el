;;; programming-racket-rcp.el --- Racket             -*- lexical-binding: t; -*-

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

;; Packages related to using Racket.

;;;; Code:
(require 'keybinds-general-rcp)

;;;; Racket-mode
(use-package racket-mode
  :disabled ; FIXME 2023-07-16: Haven't figured out a way to programmatically detect racket-langserver's presence
  :ensure-system-package ("/home/krisbalintona/.local/share/racket/8.9/pkgs/racket-langserver" . "raco pkg install racket-langserver")
  :hook ((racket-mode . display-fill-column-indicator-mode)
         (racket-mode . eglot-ensure)))

(provide 'programming-racket-rcp)
;;; programming-racket-rcp.el ends here
