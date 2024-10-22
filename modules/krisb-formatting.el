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
   (alist-get 'haskell-mode apheleia-mode-alist) '(fourmolu))
  (setf
   ;; Formatters
   (alist-get 'black apheleia-formatters)
   '("black"
     "-l 80"
     "-")
   (alist-get 'google-java-format apheleia-formatters)
   '("google-java-format"
     "--aosp"
     "--skip-removing-unused-imports"
     "-")
   (alist-get 'stylua apheleia-formatters)
   `("stylua"
     "--indent-type" "Spaces"
     "--line-endings" "Unix"
     "--column-width" ,(number-to-string fill-column)
     "--quote-style" "ForceDouble"
     "-")
   (alist-get 'latexindent apheleia-formatters)
   '("latexindent"
     "--cruft=/tmp/"
     "--logfile"
     "indent.log")
   (alist-get 'rufo apheleia-formatters)
   '("rufo"
     "--simple-exit"
     "--filename"
     filepath)
   (alist-get 'fourmolu apheleia-formatters)
   '("fourmolu")))

;;; Krisb-indentation
(use-package krisb-indentation
  :ensure nil
  :bind ([remap indent-region] . krisb-format-buffer-indentation))

;;; Provide
(provide 'krisb-formatting)
