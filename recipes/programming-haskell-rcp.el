;;; programming-haskell-rcp.el --- Haskell           -*- lexical-binding: t; -*-

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

;; Packages related to using Haskell.

;;; Code:
(require 'keybinds-general-rcp)

;;;; Haskell-mode
(use-package haskell-mode
  :hook ((haskell-mode . turn-on-haskell-doc-mode)
         ;; hslint on the command line only likes this indentation mode;
         ;; alternatives commented out below.
         (haskell-mode . turn-on-haskell-indentation)
         ;; (haskell-mode . turn-on-haskell-indent)
         ;; (haskell-mode . turn-on-haskell-simple-indent)
         ))

;;;; Lsp-haskell
(use-package lsp-haskell
  :hook ((haskell-mode . lsp)
         (haskell-literate-mode . lsp)))

(provide 'programming-haskell-rcp)
;;; programming-haskell-rcp.el ends here
