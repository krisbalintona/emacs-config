;;; init.el --- -*- lexical-binding: t -*-
;;
;;; package --- Summary
;;; Commentary:
;;; Add Melpa and Elpa package archives.  Then update package archives if they
;;; haven't done so already and then install and reqeire use-package if it isn't
;;; installed already.  Then load my actual init file that is in the form of an
;;; org file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;: InitialGC
(setq gc-cons-threshold most-positive-fixnum)
;;: InitialGC

;;; LoadPath
;; Add elisp in folder to load path
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))
(update-to-load-path (expand-file-name "elisp" user-emacs-directory))
;;; LoadPath

;;; Packages
;; Load all config files in my config directory

(require 'auto-gc-rcp)
;; (require 'default-package-management-rcp) ; Use package.el
(require 'straight-package-management-rcp) ; Use straight.el
(require 'better-defaults-rcp)
(require 'default-package-paths-rcp)
(require 'profiling-and-debug-rcp)

(org-babel-load-file (concat user-emacs-directory "configs/" "base-config.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "qol-packages.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "completion-frameworks.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "buffer-management.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "evil-packages.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "email.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "calendar-integration.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "org-packages.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "org-agenda.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "second-brain.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "file-and-directory-nav.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "programming.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "shell.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "undoing.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "spelling-and-grammar.org"))

;;; Packages

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
