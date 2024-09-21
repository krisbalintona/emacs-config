;;; init.el --- -*- lexical-binding: t -*- --- Summary
;;
;;; Commentary:
;;
;; Set native-compilation settings. Also define paths for my config (org and
;; elisp) files and load them
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;; Recursively add files to `load-path'
(dolist (path (list (expand-file-name "recipes" user-emacs-directory)
                    (expand-file-name "site-lisp" user-emacs-directory)
                    (expand-file-name "site-lisp/org-wc-targets/" user-emacs-directory)
                    (expand-file-name "exwm" user-emacs-directory)))
  (add-to-list 'load-path path))

;;; Load packages
;; Load all elisp and org configuration files

;;;; Configuration prerequisites
;; These are packages which come before others because their config files rely
;; on them.
;; (require 'straight-package-management-rcp)
;; (require 'elpaca-package-management-rcp)
(require 'vanilla-package-management-rcp)
(require 'use-package-rcp)
(require 'system-packages-rcp)
(require 'garbage-collection-rcp)
(require 'personal-variables-rcp)
(require 'external-programs-rcp)
(require 'better-defaults-rcp)     ; FIXME 2024-02-11: Distribute these settings
(require 'custom-directories-rcp)
;; (require 'exwm-rcp)
;; (require 'exwm-extras-rcp)

;;;; Keybind infrastructure
(require 'keybinds-general-rcp)
(require 'keybinds-native-rcp)
;; (require 'keybinds-evil-rcp)

;;;; Aesthetics
(require 'fonts-rcp)
(require 'ui-rcp)

;;;; Completion frameworks
(require 'completion-general-rcp)
(require 'completion-vanilla-rcp)
(require 'completion-inline-rcp)
;; (require 'completion-company-rcp)
;; (require 'completion-ivy-rcp)
;; (require 'completion-helm-rcp)

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
(require 'org-notes-rcp)
(require 'org-notes-other-rcp)
(require 'org-agenda-general-rcp)
;; (require 'org-agenda-views-rcp)
(require 'org-agenda-other-rcp)

;;;; Other major modes
(require 'latex-general-rcp)
(require 'markdown-general-rcp)

;;;; Programming
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

;;;;; Languages
(require 'programming-elisp-rcp)
(require 'programming-shell-rcp)
(require 'programming-lua-rcp)
(require 'programming-web-rcp)
(require 'programming-python-rcp)
(require 'programming-java-rcp)
(require 'programming-c-rcp)
(require 'programming-racket-rcp)
(require 'programming-rust-rcp)
(require 'programming-haskell-rcp)

;;;; Shells
(require 'shell-basic-rcp)
(require 'shell-fish-rcp)
(require 'shell-eshell-rcp)
(require 'shell-vterm-rcp)

;;;; Communication interfaces
(require 'email-notmuch-rcp)
(require 'email-mu4e-rcp)
(require 'email-sending-rcp)
(require 'rss-feed-rcp)
(require 'epub-rcp)

;;;; Other
(require 'web-browsing-rcp)
(require 'application-framework-rcp)
;; (require 'finance-rcp)
(require 'convenient-functions-rcp)
(require 'template-rcp)
(require 'dungeons-and-dragons-rcp)
(require 'misc-packages-rcp) ; FIXME 2024-02-11: Distribute these configs elsewhere

;; Startup time stats
(add-hook 'window-setup-hook
          #'(lambda ()
              (if (and (fboundp 'elpaca--queued) (boundp 'elpaca-after-init-time))
                  (message "Emacs took %s to start up and load %s packages!"
                           (format "%f seconds" (float-time (time-subtract elpaca-after-init-time
                                                                           before-init-time)))
                           (length (elpaca--queued)))
                (message "Emacs took %s to start up and load %s packages!"
                         (format "%f seconds" (float-time (time-subtract after-init-time
                                                                         before-init-time)))
                         (length package-selected-packages)))))

;;; init.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
