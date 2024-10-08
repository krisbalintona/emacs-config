;;; programming-rust-rcp.el --- Rust                 -*- lexical-binding: t; -*-

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

;; Packages related to using Rust.

;;; Code:
(require 'keybinds-general-rcp)

;;;; Rustic
;; Rust-mode but with improvements for IDE experience (e.g. lsp-mode and eglot
;; integration)
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-client 'lsp-mode)
  (rustic-format-on-save nil))          ; I use apheleia

(provide 'programming-rust-rcp)
;;; programming-rust-rcp.el ends here
