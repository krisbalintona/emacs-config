;;; personal-variables-rcp.el --- Configure and load repositories
;;
;;; Commentary:
;;
;; Variables pertinent to me and my system.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
         (t "Unsupported distribution!"))
   ))

;;; personal-variables-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'personal-variables-rcp)
