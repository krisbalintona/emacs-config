;;; init.el --- -*- lexical-binding: t -*- --- Summary
;;
;;; Commentary:
;;
;; Set native-compilation settings. Also define paths for my config (org and
;; elisp) files and load them
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

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
(update-to-load-path (expand-file-name "recipes" user-emacs-directory))
(update-to-load-path (expand-file-name "site-lisp" user-emacs-directory))
(update-to-load-path (expand-file-name "exwm" user-emacs-directory))

;;; Load packages
;; Load all elisp and org configuration files

;;;; Configuration prerequisites
;; These are packages which come before others because their config files rely
;; on them.
(require 'straight-package-management-rcp)
(require 'personal-variables-rcp)
(require 'external-programs-rcp)
(require 'better-defaults-rcp)
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'garbage-collection-rcp)
;; (require 'exwm-rcp)
;; (require 'exwm-extras-rcp)

;;;; Keybind infrastructure
(require 'keybinds-general-rcp)
(require 'keybinds-native-rcp)
(require 'keybinds-evil-rcp)

;;;; Aethetics
(require 'fonts-rcp)
(require 'themes-rcp)
(require 'kb-themes)

;;;; Completion frameworks
(require 'completion-general-rcp)
(require 'completion-vanilla-rcp)
(require 'completion-inline-rcp)
(require 'completion-company-rcp)
(require 'completion-ivy-rcp)
(require 'completion-helm-rcp)

;;;; Buffers, text, and windows
(require 'persistence-rcp)
(require 'buffers-and-windows-rcp)
(require 'undoing-rcp)
(require 'buffer-nav-rcp)

;;;; Spelling and grammar
(require 'checking-spelling-rcp)
(require 'checking-grammar-rcp)
(require 'checking-words-rcp)

;;;; Org
(require 'org-general-rcp)
(require 'org-export-rcp)
(require 'org-citations-rcp)
(require 'org-blogging-rcp)
(require 'org-roam-general-rcp)
(require 'org-roam-other-rcp)
(require 'org-agenda-general-rcp)
;; (require 'org-agenda-views-rcp)
;; (require 'org-agenda-other-rcp)

;;;; LaTeX
(require 'latex-general-rcp)

;;;; Programmming
;;;;; Core
(require 'programming-profiling-and-debug-rcp)
(require 'programming-general-rcp)
(require 'programming-directories-rcp)
(require 'programming-projects-rcp)
(require 'programming-linting-rcp)

;;;;; IDE
(require 'programming-ide-base-rcp)
(require 'programming-eglot-rcp)
(require 'programming-lsp-mode-rcp)
(require 'programming-lsp-bridge-rcp)
(require 'programming-debugging-rcp)

;;;;; Langauges
(require 'programming-elisp-rcp)
(require 'programming-shell-rcp)
(require 'programming-lua-rcp)
(require 'programming-web-rcp)
(require 'programming-python-rcp)
(require 'programming-java-rcp)

;;;; Shells
(require 'shell-basic-rcp)
(require 'shell-eshell-rcp)
(require 'shell-vterm-rcp)

;;;; Communication interfaces
(require 'email-notmuch-rcp)
;; (require 'email-mu4e-rcp)
(require 'email-sending-rcp)
(require 'rss-feed-rcp)
(require 'irc-rcp)

;;;; Other
(require 'web-browsing-rcp)
(require 'application-framework-rcp)
(require 'finance-rcp)
(require 'convenient-functions-rcp)
(require 'template-rcp)
(require 'misc-packages-rcp)

;;; init.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
