;;; garbage-collection-rcp.el --- GC stuff           -*- lexical-binding: t; -*-

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

;; Faster Emacs startup and session.
;; Other things that can help:
;; 1) Native compilation (`libgccjit')
;; 2) Native (`libjansson') JSON support (alternative Elisp parser)
;;
;; The flags I use when manually compiling Emacs are:
;; CFLAGS='-march=native -O2' ./configure --with-dbus --with-gif --with-xwidgets --with-modules --with-json --with-x-toolkit=gtk3 --with-native-compilation --enable-link-time-optimization --with-imagemagick --with-mailutils --with-tree-sitter
;; This follows ./autogen.sh. Then I run make -j$(nproc) and then make install.

;;; Code:

(provide 'garbage-collection-rcp)
;;; garbage-collection-rcp.el ends here
