;;; init.el --- -*- lexical-binding: t -*-

;;; package --- Summary
;;; Commentary:
;;; Add Melpa and Elpa package archives.  Then update package archives if they
;;; haven't done so already and then install and reqeire use-package if it isn't
;;; installed already.  Then load my actual init file that is in the form of an
;;; org file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;; UnnecessaryUI
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; UnnecessaryUI

;; BetterGC
;; Set GC
(defvar better-gc-cons-threshold (round (* 1024 1024 0.8)) ; In mb
  "The default value to use for `gc-cons-threshold'.

  If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            ))
;; BetterGC

;; AutoGC
;; Garbage Collect when Emacs is out of focus and avoid garbage collection when using minibuffer
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              ;; (add-hook 'after-focus-change-function 'garbage-collect))
              (add-hook 'focus-out-hook 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; AutoGC

;; LoadPath
;; Add elisp folder that contains config files to load path
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
;; LoadPath

;; Packages
;; Load all config files in my config directory

(require 'package-management-rcp)
(require 'repo-configure-rcp)

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
(org-babel-load-file (concat user-emacs-directory "configs/" "eshell.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "undoing.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "spelling-and-grammar.org"))

;; Packages
(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
