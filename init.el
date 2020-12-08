;;; init.el --- -*- lexical-binding: t -*- --- Summary
;;
;; Set native-compilation settings. Also define paths for my config (org and
;; elisp) files and load them
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Native compilation defer?
(setq comp-deferred-compilation t) ; AoT or JiT compilation?

;;;; Initial GC threshold
;; Set as high as possible threshold for GC as early as possible in order to
;; reduce load time. This value is then lowered to a normal threshold later
(setq gc-cons-threshold most-positive-fixnum)

;;;; Load elisp config path
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
(update-to-load-path (expand-file-name "elisp" user-emacs-directory))

;;;; Load packages
;; Load all elisp and org configuration files

;;;;; Necessary packages and settings
(require 'auto-gc-rcp)
;; (require 'default-package-management-rcp) ; Use package.el
(require 'straight-package-management-rcp) ; Use straight.el
(require 'early-packages-rcp)
(require 'better-defaults-rcp)
(require 'faces-rcp)

;;;;; Emacs-wide packages
;; Packages that are useful across my entire Emacs experience. Other packages
;; defined afterward will often rely on these
(require 'keybinds-frameworks-rcp)
(require 'custom-directories-rcp)
(require 'convenient-functions-rcp)
(require 'themes-rcp)

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
(org-babel-load-file (concat user-emacs-directory "configs/" "latex.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "file-and-directory-nav.org"))

;;;;; Programmming
(require 'profiling-and-debug-rcp)
(require 'programming-general-rcp)
(require 'programming-elisp-rcp)
(require 'programming-lua-rcp)

(org-babel-load-file (concat user-emacs-directory "configs/" "shell.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "undoing.org"))

;;;;; Spelling and grammar
(require 'checking-spelling-rcp)
(org-babel-load-file (concat user-emacs-directory "configs/" "spelling-and-grammar.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
;;; Commentary:
;;
;;; init.el ends here
