;;; Add modules and lisp to load path
(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))

;;; Load libraries
(require 'krisb-common)
(require 'krisb-essentials)

;;;; Modules
(require 'krisb-garbage-collection)
(require 'krisb-system-env)
(require 'krisb-essentials)
(require 'krisb-core)

(require 'krisb-icons)
(require 'krisb-themes)
(require 'krisb-fonts)
(require 'krisb-mode-line)

(require 'krisb-saving-state)
(require 'krisb-persistence)
(require 'krisb-completion)
(require 'krisb-expansion)
(require 'krisb-formatting)
(require 'krisb-windows)
(require 'krisb-navigation)

(require 'krisb-prose)
(require 'krisb-org)
(require 'krisb-org-agenda)
(require 'krisb-org-export)
(require 'krisb-denote)
(require 'krisb-citations)
(require 'krisb-spelling)

(require 'krisb-pdfs)
(require 'krisb-web-annotations)
(require 'krisb-epub)

(require 'krisb-programming-essentials)
(require 'krisb-directories)
(require 'krisb-treesit)
(require 'krisb-vc)
(require 'krisb-projects)
(require 'krisb-shell)
(require 'krisb-flymake)

(require 'krisb-elisp)
(require 'krisb-info)

(require 'krisb-hugo)
(require 'krisb-mermaid)

(require 'krisb-email-composition)
(require 'krisb-notmuch)

(require 'krisb-web)

;;; Load custom file
(when (file-exists-p custom-file)
  (load custom-file))
