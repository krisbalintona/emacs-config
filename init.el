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
(update-to-load-path (expand-file-name "elisp" user-emacs-directory))
(update-to-load-path (expand-file-name "exwm" user-emacs-directory))

;;; Load packages
;; Load all elisp and org configuration files

;;;; Configuration prerequisites
;; These are packages which come before others because their config files rely
;; on them.
(require 'garbage-collection-rcp)
(require 'straight-package-management-rcp)
(require 'better-defaults-rcp)
(require 'personal-variables-rcp)
(require 'external-programs-rcp)
(require 'custom-directories-rcp)
(require 'use-package-rcp)
;; (require 'exwm-rcp)
;; (require 'exwm-extras-rcp)

;;;; Keybind infrastructure
(require 'keybinds-general-rcp)
(require 'keybinds-native-rcp)
(require 'keybinds-evil-rcp)
;; (require 'keybinds-kakoune-rcp)

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
(require 'kb-comment)
(require 'undoing-rcp)
(require 'buffer-nav-rcp)

;;;; Spelling and grammar
(require 'checking-spelling-rcp)
(require 'checking-grammar-rcp)
(require 'checking-words-rcp)

;;;; Org
(require 'org-general-rcp)
(require 'org-citations-rcp)
(require 'org-blogging-rcp)
(require 'org-roam-general-rcp)
(require 'org-roam-other-rcp)
(require 'org-agenda-general-rcp)
;; (require 'org-agenda-views-rcp)
;; (require 'org-agenda-other-rcp)

;; FIXME 2022-05-24: For some reaosn, I have to load the `org-macs' library late
;; into startup - perhaps after the window is loaded?
(with-eval-after-load 'org-roam (load "org-macs"))

;;;; LaTeX
(require 'latex-general-rcp)

;;;; Programmming
(require 'programming-profiling-and-debug-rcp)
(require 'programming-general-rcp)
(require 'programming-directories-rcp)
(require 'programming-projects-rcp)
(require 'programming-linting-rcp)
(require 'programming-ide-rcp)
(require 'programming-elisp-rcp)
(require 'programming-shell-rcp)
(require 'programming-lua-rcp)
(require 'programming-web-rcp)
(require 'programming-haskell-rcp)
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
