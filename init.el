;;; init.el --- -*- lexical-binding: t -*- --- Summary
;;
;;; Commentary:
;;
;; Set native-compilation settings. Also define paths for my config (org and
;; elisp) files and load them
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

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

;;;;; Emacs-wide packages
;; Packages that are useful across my entire Emacs experience. Other packages
;; defined afterward will often rely on these
(require 'auto-gc-rcp)
(require 'personal-variables-rcp)
(require 'straight-package-management-rcp)
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)
(require 'early-packages-rcp)
(require 'better-defaults-rcp)
(require 'faces-rcp)

(require 'custom-directories-rcp)
(require 'convenient-functions-rcp)
(require 'themes-rcp)

(org-babel-load-file (concat user-emacs-directory "configs/" "etc-config.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "qol-packages.org"))

;;;;; Completion frameworks
(require 'completion-general-rcp)
(require 'completion-company-rcp)
(require 'completion-ivy-rcp)
(require 'completion-selectrum-rcp)
(require 'completion-helm-rcp)

;;;;; Buffer management
(require 'buffer-and-window-management-rcp)

;;;;; Evil
(require 'evil-rcp)

;;;;; Org

(org-babel-load-file (concat user-emacs-directory "configs/" "org-packages.org"))

;; (require 'org-agenda-general-rcp)
;; (require 'org-agenda-views-rcp)
;; (require 'org-agenda-other-rcp)

(require 'org-roam-general-rcp)
(require 'org-roam-other-rcp)

;; (org-babel-load-file (concat user-emacs-directory "configs/" "second-brain.org"))

(org-babel-load-file (concat user-emacs-directory "configs/" "calendar-integration.org"))
(org-babel-load-file (concat user-emacs-directory "configs/" "file-and-directory-nav.org"))

;;;;; Email
;; (require 'mu4e-rcp)

;;;;; RSS feed
(require 'rss-feed-rcp)

;;;;; LaTeX
(require 'latex-general-rcp)
(require 'latex-pdf-rcp)

;;;;; Programmming
(require 'programming-profiling-and-debug-rcp)
(require 'programming-general-rcp)
(require 'programming-elisp-rcp)
(require 'programming-lua-rcp)
(require 'programming-web-rcp)

;;;;; Shells
(require 'shell-eshell-rcp)
;; (require 'shell-vterm-rcp)

;;;;; Undoing
(require 'undoing-rcp)

;;;;; Spelling and grammar
(require 'checking-spelling-rcp)
(require 'checking-grammar-rcp)
(require 'checking-words-rcp)

;;;;; Finances
(require 'finance-rcp)

;;; init.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
