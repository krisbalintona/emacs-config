;;; custom-directories-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are directory and file definitions that I use frequently enough to
;; define
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; For org and org-agenda
(require 'org)
(setq org-directory "~/Documents/org-database/")
(defvar kb/agenda-dir (concat org-directory "agenda/"))
(defvar kb/roam-dir (concat org-directory "roam/"))
(defvar kb/library-dir (concat org-directory "library/"))

(defvar kb/all-org-dir-files (cl-remove-if
                              (lambda (it)
                                (string-match-p (rx "archive.org") it))
                              (directory-files-recursively org-directory ".org$")))
(defvar kb/all-agenda-dir-files (cl-remove-if
                                 (lambda (it)
                                   (string-match-p (rx "archive.org") it))
                                 (directory-files-recursively kb/agenda-dir ".org$")))
(defvar kb/all-agenda-dir-files-minus-inbox (cl-remove-if
                                             (lambda (it)
                                               (string-match-p (rx "archive.org") it)
                                               (string-match-p (rx "inbox.org") it))
                                             (directory-files-recursively kb/agenda-dir ".org$")))

;;;; For my frequently visited directories and files
(defvar kb/library-dir (concat org-directory "library"))
(defvar kb/emacs-etc-config-file (concat user-emacs-directory "configs/etc-config.org"))
(defvar kb/emacs-config-dir (concat user-emacs-directory "configs/"))

(defvar kb/dot-config-dir "~/.config/")
(defvar kb/wm-config-file (concat kb/dot-config-dir "i3/config"))

;;; custom-directories-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'custom-directories-rcp)
