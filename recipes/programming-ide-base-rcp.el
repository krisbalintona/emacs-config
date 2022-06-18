;;; programming-ide-base-rcp.el --- Summary
;;
;;; Commentary:
;;
;; General IDE features.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Apheleia
;; Quality code formatting for (arbitrarily) many languages
(use-package apheleia
  :ensure-system-package ((black . python-black)
                          (prettier)
                          (clang-format . clang-format-all-git)
                          (latexindent . texlive-latexindent-meta)
                          (stylua)
                          (google-java-format)
                          (shfmt))
  :config
  ;; Configure `apheleia-formatters' and `apheleia-mode-alist' here. I use setf
  ;; instead of defining the variables directly so that it is agnostic to any
  ;; package changes. Take a look at the `format-all' package for how to install
  ;; particular formatters as well as their proper CLI commands. Namely, inspect
  (setf (alist-get 'lua-mode apheleia-mode-alist) '(stylua)
        (alist-get 'black apheleia-formatters) '("black" "-l 80" "-")
        (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "--aosp" "--skip-removing-unused-imports" "-")
        (alist-get 'stylua apheleia-formatters)
        `("stylua" "--indent-type" "Spaces" "--line-endings" "Unix"  "--column-width" ,(number-to-string fill-column) "--quote-style" "ForceDouble" "-")
        (alist-get 'latexindent apheleia-formatters)
        '("latexindent" "--cruft=/tmp/" "--logfile" "indent.log")))

;;; Devdocs
;; Viewing documentation within Emacs. Requires internet connection.
(use-package devdocs
  :hook ((python-mode   . (lambda () (setq-local devdocs-current-docs '("python~3.9"))))
         (haskell-mode  . (lambda () (setq-local devdocs-current-docs '("haskell~8"))))
         (js2-mode      . (lambda () (setq-local devdocs-current-docs '("JavaScript"))))
         (lua-mode      . (lambda () (setq-local devdocs-current-docs '("lua~5.3"))))
         (LaTeX-mode    . (lambda () (setq-local devdocs-current-docs '("latex"))))
         )
  :general (kb/lsp-keys
             "D" '(:ignore t :wk "Devdocs")
             "Di" '(devdocs-install :wk "Install documentation for a language")
             "Dl" '(devdocs-lookup :wk "Documentation lookup")
             "DL" '(devdocs-search :wk "Search for docs in site"))
  )

;;; Dash-docs
;; Offline viewing of documentation via browser. Doesn't require an internet
;; connection.
(use-package dash-docs
  :hook ((python-mode   . (lambda () (setq-local dash-docs-common-docsets '("Python 3"))))
         (haskell-mode  . (lambda () (setq-local dash-docs-common-docsets '("Haskell"))))
         (js2-mode      . (lambda () (setq-local dash-docs-common-docsets '("JavaScript"))))
         (lua-mode      . (lambda () (setq-local dash-docs-common-docsets '("Lua"))))
         (LaTeX-mode    . (lambda () (setq-local dash-docs-common-docsets '("LaTeX")))))
  :custom
  (dash-docs-docsets-path (expand-file-name "dash-docs-docsets" no-littering-var-directory))
  (dash-docs-browser-func 'eww)

  (dash-docs-enable-debugging nil) ; Get rid of annoying messages when searching
  (dash-docs-min-length 2)
  (dash-enable-fontlock t))

;;;; Dash-docs-completing-read
;; My own interface for accessing docsets via `completing-read'.
(use-package dash-docs-completing-read
  :straight nil
  :after dash-docs
  :general (kb/lsp-keys
             "D" '(:ignore t :wk "Dashdocs")
             "Di" '(dash-docs-install-docset :wk "Install docs")
             "Dl" '(dash-docs-completing-read-at-point :wk "At-point search")
             "DL" '(dash-docs-completing-read :wk "Manual search")
             ))

;;; Tree-sitter
;;;; Itself
;; Create a syntax tree (e.g. the role of each piece of code) and add syntax
;; highlighting from it (rather than regex and indentation). Additionally, the
;; syntax tree itself can help debug and quick editing in some cases.
(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode) ; Enable syntax highlighting whenever possible
  :init
  (global-tree-sitter-mode))    ; Enable for all supported tree-sitter languages

;;;; Tree-sitter-langs
;; Supported language bundle for tree-sitter
(use-package tree-sitter-langs
  :after tree-sitter
  :defer 5                              ; Best solution I could think of...
  :config
  (tree-sitter-langs-install-grammars   ; Install language bundles
   t
   tree-sitter-langs--bundle-version
   tree-sitter-langs--os))

;;;; Evil-textobj-tree-sitter
;; Navigation of text objects with tree-sitter
(use-package evil-textobj-tree-sitter
  :straight (evil-textobj-tree-sitter :type git
                                      :host github
                                      :repo "meain/evil-textobj-tree-sitter"
                                      :files (:defaults "queries"))
  :general
  (:keymaps 'evil-inner-text-objects-map
            "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
            )
  (:keymaps 'evil-outer-text-objects-map
            "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
            ;; You can also bind multiple items and we will match the first one
            ;; we can find
            "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))
            )
  (:keymaps 'prog-mode-map
            :states 'normal
            "[f" '(lambda ()                 ; Goto start of previous/this function
                    (interactive)
                    (evil-textobj-tree-sitter-goto-textobj "function.outer" t))

            "]f" '(lambda ()                 ; Goto start of next function
                    (interactive)
                    (evil-textobj-tree-sitter-goto-textobj "function.outer"))
            "]F" '(lambda ()                 ; Goto end of next function
                    (interactive)
                    (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t))
            "[F" '(lambda ()                 ; Goto end of previous/this function
                    (interactive)
                    (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))
            ))

;;; programming-ide-base-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-ide-base-rcp)
