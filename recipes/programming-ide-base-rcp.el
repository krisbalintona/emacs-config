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
  :hook (on-first-buffer . global-treesit-auto-mode)
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
  (treesit-auto-add-to-auto-mode-alist 'all))

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
;; Which-function stuff but more performant and prettier formatting. Read
;; package commentary for a description on how.
(use-package breadcrumb
  :commands which-function-mode
  :hook ((lsp-ui-mode eglot-managed-mode) . (lambda () (when (derived-mode-p 'prog-mode) (breadcrumb-local-mode))))
  :custom
  (which-func-functions '(breadcrumb-imenu-crumbs)))

;;;; Devdocs
(use-package devdocs
  :bind
  ( :map prog-mode-map
    ("C-c D" . devdocs-lookup))
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
  :bind
  ( :map kb/lsp-keys
    ("Di" . dash-docs-install-docset)
    ("Dl" . dash-docs-completing-read-at-point)
    ("DL" . dash-docs-completing-read)))

;;;; Hideshow
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook ((prog-mode conf-mode) . hs-minor-mode)
  :bind (("C-M-s-<tab>" . kb/hs-cycle)
         ("C-M-s-S-<tab>" . kb/hs-global-cycle))
  :custom
  (hs-set-up-overlay 'kb/hideshow-display)
  (hs-isearch-open t)
  (hs-allow-nesting t)
  :config
  (defun kb/hideshow-display (ov)
    "Text to replace hidden text with."
    (overlay-put ov 'display
                 (propertize (format " [%d lines elided…] "
                                     (1- (count-lines (overlay-start ov)
                                                      (overlay-end ov))))
                             'face '(:weight light :inherit shadow))))

  ;; Taken from karthink's config. Also read
  ;; https://karthinks.com/software/simple-folding-with-hideshow/
  (defun kb/hs-cycle (&optional level)
    (interactive "p")
    (save-excursion
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;;TODO: Fix this case. `hs-show-block' needs to be called twice to
             ;;open all folds of the parent block.
             (hs-show-block)
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  ;; Taken from karthink's config. Also read
  ;; https://karthinks.com/software/simple-folding-with-hideshow/
  (defun kb/hs-global-cycle ()
    "Cycle all blocks in buffer."
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all)))))

(provide 'programming-ide-base-rcp)
;;; programming-ide-base-rcp.el ends here
