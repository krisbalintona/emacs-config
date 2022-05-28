;;; quickstart.el --- -*- lexical-binding: t -*- --- Summary
;;
;;; Commentary:
;;
;; A speedy init.el used for asynchronous org exporting.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;; Instead of `garbage-collection-rcp'
(setq garbage-collection-messages nil)  ; Don't spam logs
(setq gc-cons-threshold most-positive-fixnum)

;;; Load elisp config path
;; Add elisp directory that contains configuration files to load path
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
(setq user-emacs-directory "/home/krisbalintona/main-emacs/") ; Only for quickstart.el!
(update-to-load-path (expand-file-name "recipes" user-emacs-directory))

;;; Load packages
;;;; Configuration prerequisites
(require 'straight-package-management-rcp)
(require 'better-defaults-rcp)
(require 'personal-variables-rcp)
(require 'custom-directories-rcp)
(require 'use-package-rcp)

;;;; Org
(require 'org-general-rcp)
(require 'org-export-rcp)
(require 'org-citations-rcp)
(use-package org-transclusion :demand t) ; Org-transclusion is needed for some reason?

;;; quickstart.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'quickstart)
