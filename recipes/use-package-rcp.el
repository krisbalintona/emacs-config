;;; use-package-rcp.el --- Use-package               -*- lexical-binding: t; -*-

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

;; Install use-package, set it up, and load repositories.

;;; Code:

(require 'use-package)

(setq use-package-always-ensure t)
;; (setq use-package-expand-minimally t)   ; Less verbose
(setq use-package-always-defer t)       ; Always defer

;; Set use-package-verbose to t for interpreted .emacs, and to nil for
;; byte-compiled .emacs.elc.
(setq use-package-verbose (not (bound-and-true-p byte-compile-current-file)))

(provide 'use-package-rcp)
;;; use-package-rcp.el ends here
