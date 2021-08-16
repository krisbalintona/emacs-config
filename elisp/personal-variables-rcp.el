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
  "Are we running on a WinTel system?"
  (eq system-type 'windows-nt))

(defconst kb/sys-linux
  "Are we running on a GNU/Linux system?"
  (eq system-type 'gnu/linux))

(defconst kb/sys-mac
  "Are we running on a Mac system?"
  (eq system-type 'darwin))

(defconst kb/linux-distribution
  "An escaped string that has the name of my Linux distribution."
  (shell-command-to-string "printf %s \"$(lsb_release -sd)\""))

;;; personal-variables-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'personal-variables-rcp)
