;; -*- lexical-binding: t; -*-

;;; Add modules and lisp to load path
(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))

;;; Load libraries
(require 'krisb-common)
(require 'krisb-oblique-strategies)

;;;; Modules
(require 'krisb-core)
(require 'krisb-garbage-collection)
(require 'krisb-system-env)
(require 'krisb-essentials)

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
(require 'krisb-buffers)
(require 'krisb-navigation)

(require 'krisb-alternative-editing-schemes)

(require 'krisb-prose)
(require 'krisb-org)
(require 'krisb-org-agenda)
(require 'krisb-org-export)
;; 2024-11-06: Migrating to org-roam.  We remove the require altogether since
;; some of my Denote packages are lazy-loaded after Denote is, and they
;; overshadow some of the functionality from my org-roam related packages.  I
;; don't use Denote, but I still keep the package around (for now) since I
;; depend on some Denote forms throughout my configuration currently.
;; (require 'krisb-denote)
(require 'krisb-org-roam)
(require 'krisb-org-node)
(require 'krisb-markdown)
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
(require 'krisb-folding)
(require 'krisb-other-languages)
(require 'krisb-lsp)
(require 'krisb-debugging)

(require 'krisb-elisp)
(require 'krisb-info)

(require 'krisb-hugo)
(require 'krisb-mermaid)

(require 'krisb-email-composition)
(require 'krisb-notmuch)

(require 'krisb-web)
