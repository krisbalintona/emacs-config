;;; personal-variables-rcp.el --- Variables for my use  -*- lexical-binding: t; -*-

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

;; Variables pertinent to me and my system.

;;; Code:

;;; User information
(setq user-full-name "Kristoffer Balintona"
      user-mail-address "krisbalintona@gmail.com")

;;; System information
(defconst kb/sys-win
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst kb/sys-linux
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst kb/sys-mac
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst kb/linux-distribution
  (if kb/sys-linux
      (shell-command-to-string "printf %s \"$(lsb_release -sd)\"")
    nil)
  "An escaped string that has the name of my Linux distribution.")

(defconst kb/linux-ubuntu
  (integerp (string-match "Ubuntu" kb/linux-distribution))
  "Is this Ubuntu?")

(defconst kb/linux-fedora
  (integerp (string-match "Fedora" kb/linux-distribution))
  "Is this Fedora?")

(defconst kb/linux-arch
  (integerp (string-match "Arch" kb/linux-distribution))
  "Is this Arch Linux?")

;;; Return package manager
(defun kb/which-package-manager (&optional sudo)
  "Return the current system's package manager as a string."
  (interactive)
  (concat
   (if sudo "sudo ")
   (cond (kb/linux-arch "paru")
         (kb/linux-fedora "dnf")
         (kb/linux-ubuntu "apt")
         (t "Unsupported distribution!"))))

(provide 'personal-variables-rcp)
;;; personal-variables-rcp.el ends here
