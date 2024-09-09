;;; programming-ide-base-rcp.el --- Emacs makes a good IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; IDE features in Emacs.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Treesit
;;;;; Itself
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 3))

;;;;; Treesit-auto
(use-package treesit-auto
  :demand
  :custom
  (treesit-auto-install 'prompt)
  (treesit-extra-load-path              ; Where language files are found
   (list (no-littering-expand-var-file-name "tree-sitter")))
  :init
  ;; `Treesit-auto' doesn't allow us to set the installation path yet because
  ;; Emacs 29 `treesit-install-language-grammar' didn't have a parameter for it
  ;; (whereas the master branch of Emacs does). See
  ;; https://github.com/renzmann/treesit-auto/issues/18. Note that
  ;; `treesit-auto-install-all' doesn't use this function to install languages
  (defun kb/treesit-auto--prompt-to-install-package (lang)
    "Ask the user if they want to install a tree-sitter grammar for `LANG'.

Non-nil only if installation completed without any errors."
    (when (cond ((eq t treesit-auto-install) t)
                ((eq 'prompt treesit-auto-install)
                 (y-or-n-p (format "Tree-sitter grammar for %s is missing.  Install it from %s? "
                                   (symbol-name lang)
                                   (car (alist-get lang treesit-language-source-alist))))))
      (message "Installing the tree-sitter grammar for %s" lang)
      ;; treesit-install-language-grammar will return nil if the
      ;; operation succeeded and 't if a warning was sent to the
      ;; warning buffer. I don't think this is by design but just
      ;; because of the way `display-warning' works, so this might not
      ;; work in the future.
      (not (treesit-install-language-grammar lang
                                             ;; OPTIMIZE 2023-07-14: Manually
                                             ;; set path here. I set it to the
                                             ;; path I added to
                                             ;; "/home/krisbalintona/.emacs.d/var/tree-sitter"
                                             (no-littering-expand-var-file-name "tree-sitter")))))
  (advice-add 'treesit-auto--prompt-to-install-package :override #'kb/treesit-auto--prompt-to-install-package)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;; Turbo-log
(use-package turbo-log
  :disabled                             ; Used old tree-sitter package
  :requires tree-sitter
  :ensure (turbo-log :type git :host github :repo "artawower/turbo-log.el")
  :general (:prefix "H-;"
                    ";" 'turbo-log-print
                    "i" 'turbo-log-print-immediately
                    "h" 'turbo-log-comment-all-logs
                    "s" 'turbo-log-uncomment-all-logs
                    "[" 'turbo-log-paste-as-logger
                    "]" 'turbo-log-paste-as-logger-immediately
                    "d" 'turbo-log-delete-all-logs)
  :custom
  (turbo-log-msg-format-template "\"tk %s\"") ; "tk" is a rare bigram!
  (turbo-log-allow-insert-without-tree-sitter-p t))

;;;; Apheleia
;; Quality code formatting for (arbitrarily) many languages
(use-package apheleia
  :ensure-system-package ((black . python-black)
                          (prettier)
                          (clang-format . clang-format-all-git)
                          (latexindent . texlive-binextra)
                          (stylua)
                          (google-java-format)
                          (shfmt)
                          (rustfmt))
  :config
  ;; Configure `apheleia-formatters' and `apheleia-mode-alist' here. I use setf
  ;; instead of defining the variables directly so that it is agnostic to any
  ;; package changes. Take a look at the `format-all' package for how to install
  ;; particular formatters as well as their proper CLI commands. Namely, inspect
  ;; `format-all-formatters'.
  (setf
   ;; Major modes
   (alist-get 'lua-mode apheleia-mode-alist) '(stylua)
   (alist-get 'ruby-mode apheleia-mode-alist) '(rufo)
   (alist-get 'haskell-mode apheleia-mode-alist) '(fourmolu)
   ;; Formatters
   (alist-get 'black apheleia-formatters) '("black" "-l 80" "-")
   (alist-get 'google-java-format apheleia-formatters)
   '("google-java-format" "--aosp" "--skip-removing-unused-imports" "-")
   (alist-get 'stylua apheleia-formatters)
   `("stylua" "--indent-type" "Spaces" "--line-endings" "Unix"  "--column-width" ,(number-to-string fill-column) "--quote-style" "ForceDouble" "-")
   (alist-get 'latexindent apheleia-formatters)
   '("latexindent" "--cruft=/tmp/" "--logfile" "indent.log")
   (alist-get 'rufo apheleia-formatters) '("rufo" "--simple-exit" "--filename" filepath)
   (alist-get 'fourmolu apheleia-formatters) '("fourmolu")))

;;;; Breadcrumb
(use-package breadcrumb
  :hook ((lsp-ui-mode eglot-managed-mode) . (lambda () (when (derived-mode-p 'prog-mode) (breadcrumb-local-mode))))
  :custom
  (which-func-functions '(breadcrumb-imenu-crumbs)))

;;;; Devdocs
(use-package devdocs
  :general (:keymaps 'prog-mode-map
                     "C-c D" 'devdocs-lookup)
  :hook ((python-base-mode
          . (lambda () (setq-local devdocs-current-docs '("python~3.9"))))
         (c-ts-base-mode
          . (lambda () (setq-local devdocs-current-docs '("c"))))
         (lua-mode
          . (lambda () (setq-local devdocs-current-docs '("lua~5.3"))))
         (latex-mode
          . (lambda () (setq-local devdocs-current-docs '("latex")))))
  :custom
  (devdocs-window-select t))

;;;; Dash-docs
;; Viewing of documentation via browser.
(use-package dash-docs
  :hook ((python-base-mode . (lambda () (setq-local dash-docs-common-docsets '("Python 3"))))
         (haskell-mode . (lambda () (setq-local dash-docs-common-docsets '("Haskell"))))
         (js2-mode . (lambda () (setq-local dash-docs-common-docsets '("JavaScript"))))
         (lua-mode . (lambda () (setq-local dash-docs-common-docsets '("Lua"))))
         (LaTeX-mode . (lambda () (setq-local dash-docs-common-docsets '("LaTeX")))))
  :custom
  (dash-docs-docsets-path (expand-file-name "dash-docs-docsets" no-littering-var-directory))
  (dash-docs-browser-func 'eww)

  (dash-docs-enable-debugging nil) ; Get rid of annoying messages when searching
  (dash-docs-min-length 2)
  (dash-enable-fontlock t))

;;;;; Dash-docs-completing-read
;; My own interface for accessing docsets via `completing-read'.
(use-package dash-docs-completing-read
  :ensure nil
  :after dash-docs
  :general (kb/lsp-keys
             "D" '(:ignore t :wk "Dashdocs")
             "Di" '(dash-docs-install-docset :wk "Install docs")
             "Dl" '(dash-docs-completing-read-at-point :wk "At-point search")
             "DL" '(dash-docs-completing-read :wk "Manual search")))

;;;; Hideshow
(use-package hideshow
  :ensure nil
  :hook ((prog-mode conf-mode) . hs-minor-mode)
  :custom
  (hs-set-up-overlay 'kb/hideshow-display)
  (hs-isearch-open t)
  (hs-allow-nesting t)
  :init
  (defun kb/hideshow-display (ov)
    "Text to replace hidden text with."
    (overlay-put ov 'display
                 (propertize (format " [%d lines elided…] "
                                     (1- (count-lines (overlay-start ov)
                                                      (overlay-end ov))))
                             'face 'shadow)))
  :config
  (with-eval-after-load 'diminish
    (diminish 'hs-minor-mode)))

(provide 'programming-ide-base-rcp)
;;; programming-ide-base-rcp.el ends here
