;;; personal-variables-rcp.el --- Configure and load repositories
;;
;;; Commentary:
;;
;; Variables pertinent to me and my system.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; User information
(setq user-full-name "Kristoffer Balintona"
      user-mail-address "krisbalintona@gmail.com")

;;;; System information
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
"Is this Ubuntu?"
      (integerp (string-match "Ubuntu" kb/linux-distribution)))

(defconst kb/linux-fedora
      "Is this Fedora?"
      (integerp (string-match "Fedora" kb/linux-distribution)))

(defconst kb/linux-arch
      "Is this Arch Linux?"
      (integerp (string-match "Arch" kb/linux-distribution)))

;;; personal-variables-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'personal-variables-rcp)
