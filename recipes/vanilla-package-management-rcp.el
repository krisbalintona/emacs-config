;;; vanilla-package-management-rcp.el --- package.el thingies  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>

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

;; All configuration related to package.el.

;;; Code:

;; Initialize package sources
(require 'package)

(setq package-archive-priorities '(("gnu" . 10)
                                   ("melpa" . 5))
      package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ;; ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa-devel" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; For Elpaca. I still want to keep my elpaca-wait declarations in my
;; configuration files just in case I ever go back to Elpaca.
(defalias 'elpaca-wait #'ignore)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;;; Package-vc
(setq package-install-upgrade-built-in t)

(provide 'vanilla-package-management-rcp)
;;; vanilla-package-management-rcp.el ends here
