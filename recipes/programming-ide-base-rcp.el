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

;;; Quickrun
(use-package quickrun
  :general (:prefix "<f2>"
            "<f2>" 'quickrun
            "<f3>" '(lambda ()
                       (interactive)
                       (let ((quickrun-focus-p t))
                         (quickrun-shell))))
  :custom
  (quickrun-focus-p nil))

;;; Apheleia
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

;;; Devdocs
;; Viewing documentation within Emacs.
(use-package devdocs-browser
  :general (:keymaps '(eglot-mode-map lsp-mode-map lsp-bridge-mode-map)
            :prefix "H-d"
            "h" '(devdocs-browser-open :wk "Open")
            "H" '(devdocs-browser-open-in :wk "Open-in")
            "i" '(devdocs-browser-install-doc :wk "Install")
            "d" '(devdocs-browser-download-offline-data :wk "Download")
            "D" '(devdocs-browser-upgrade-all-docs :wk "Upgrade"))
  :custom
  (devdocs-browser-major-mode-docs-alist
   '((c++-mode "cpp")
     (c-mode "c")
     (go-mode "go")
     (python-mode "Python")
     (emacs-lisp-mode "elisp")
     (cmake-mode "CMake")
     (haskell-mode "Haskell"))))

;;; Dash-docs
;; Viewing of documentation via browser.
(use-package dash-docs
  :elpaca  (dash-docs :type git
                        :host github
                        :repo "dash-docs-el/dash-docs"
                        :fork (:host github
                               :repo "krisbalintona/dash-docs"))
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
  :elpaca nil
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
  :diminish "TSitter"
  :hook (tree-sitter-after-on . tree-sitter-hl-mode) ; Enable syntax highlighting whenever possible
  :init
  (global-tree-sitter-mode))    ; Enable for all supported tree-sitter languages

;;;; Tree-sitter-langs
;; ;; Supported language bundle for tree-sitter
;; (use-package tree-sitter-langs
;;   :after tree-sitter
;;   :defer 5                              ; Best solution I could think of...
;;   :config
;;   (tree-sitter-langs-install-grammars   ; Install language bundles
;;    t
;;    tree-sitter-langs--bundle-version
;;    tree-sitter-langs--os))
;;
;;;; Turbo-log
;; (use-package turbo-log
;;   :requires tree-sitter
;;   :elpaca (turbo-log :type git :host github :repo "artawower/turbo-log.el")
;;   :general (:prefix "H-;"
;;             ";" 'turbo-log-print
;;             "i" 'turbo-log-print-immediately
;;             "h" 'turbo-log-comment-all-logs
;;             "s" 'turbo-log-uncomment-all-logs
;;             "[" 'turbo-log-paste-as-logger
;;             "]" 'turbo-log-paste-as-logger-immediately
;;             "d" 'turbo-log-delete-all-logs)
;; :custom
;; (turbo-log-msg-format-template "\"tk %s\"") ; "tk" is a rare bigram!
;; (turbo-log-allow-insert-without-tree-sitter-p t))

;;; Treesit (built-in)
;; Taken from https://github.com/casouri/tree-sitter-module/issues/13
;; (use-package tree-sitter-module
;;   :elpaca (tree-sitter-module
;;              :type git :host github
;;              :repo "casouri/tree-sitter-module"
;;              :pre-build (("./batch.sh"))
;;              :files ("dist/*.so" "dist/*.dll" "dist/*.dylib"))
;;   :init
;;   ;; Search for tree-sitter modules in this packages build directory.
;;   (with-eval-after-load 'treesit
;;     (add-to-list 'treesit-extra-load-path
;;                  (straight--build-dir "tree-sitter-module"))))

;;; programming-ide-base-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-ide-base-rcp)
