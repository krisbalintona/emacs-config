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
(update-to-load-path (expand-file-name "exwm" user-emacs-directory))

;;;; Load packages
;; Load all elisp and org configuration files

;;;;; Infrastructure
(require 'auto-gc-rcp)
(require 'straight-package-management-rcp)
(require 'better-defaults-rcp)
(require 'personal-variables-rcp)
(require 'external-programs-rcp)
(require 'custom-directories-rcp)
;; (require 'exwm-rcp)
;; (require 'exwm-extras-rcp)

;;;;; Configuration prerequisties
;; These are packages which come before others because their config files rely
;; on them.
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)
(require 'evil-rcp)
(require 'early-packages-rcp)

;;;;; Aethetics
(require 'faces-rcp)
(require 'themes-rcp)

;;;;; Completion frameworks
(require 'completion-general-rcp)
(require 'completion-default-rcp)
(require 'completion-text-rcp)
(require 'completion-ivy-rcp)
(require 'completion-helm-rcp)

;;;;; Org
(require 'org-general-rcp)
(require 'org-roam-general-rcp)
(require 'org-roam-other-rcp)
;; (require 'org-agenda-general-rcp)
;; (require 'org-agenda-views-rcp)
;; (require 'org-agenda-other-rcp)

;;;;; LaTeX
(require 'latex-general-rcp)
(require 'latex-pdf-rcp)

;;;;; Programmming
(require 'programming-profiling-and-debug-rcp)
(require 'programming-general-rcp)
(require 'programming-vc-rcp)
(require 'programming-linting-rcp)
(require 'programming-ide-rcp)
(require 'programming-elisp-rcp)
(require 'programming-lua-rcp)
(require 'programming-web-rcp)
(require 'programming-haskell-rcp)

;;;;; Shells
(require 'shell-basic-rcp)
(require 'shell-eshell-rcp)
(require 'shell-vterm-rcp)

;;;;; Spelling and grammar
(require 'checking-spelling-rcp)
(require 'checking-grammar-rcp)
(require 'checking-words-rcp)

;;;;; Communication interfaces
;; (require 'email-mu4e-rcp)
(require 'email-sending-rcp)
(require 'rss-feed-rcp)

;;;;; Other
(require 'undoing-rcp)
(require 'buffer-and-window-management-rcp)
(require 'file-editing-rcp)
(require 'finance-rcp)
(require 'convenient-functions-rcp)
(require 'misc-packages-rcp)

;;; init.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
