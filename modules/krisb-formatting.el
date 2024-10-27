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
  :hook (apheleia-post-format . delete-trailing-whitespace)
  :custom
  (apheleia-mode-lighter "")
  (apheleia-log-only-errors t)
  (apheleia-log-debug-info nil)         ; Can be useful to set to non-nil in order to temporarily debug
  (apheleia-hide-log-buffers nil)

  (apheleia-formatters-respect-fill-column t)
  (apheleia-formatters-respect-indent-level t)
  :config
  (apheleia-global-mode 1)

  ;; Configure `apheleia-formatters' and `apheleia-mode-alist' here. I use setf
  ;; instead of defining the variables directly so that it is agnostic to any
  ;; package changes. Take a look at the `format-all' package for how to install
  ;; particular formatters as well as their proper CLI commands. Namely, inspect
  ;; `format-all-formatters'.

  ;; Major modes
  (setf
   (alist-get 'lua-mode apheleia-mode-alist) '(stylua)
   (alist-get 'ruby-mode apheleia-mode-alist) '(rufo)
   (alist-get 'haskell-mode apheleia-mode-alist) '(fourmolu))

  ;; Formatters
  (setf
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
   '("fourmolu"))

  ;; Custom formatters
  (cl-defun krisb-apheleia-format-org-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Format an Org BUFFER.
Use SCRATCH as a temporary buffer and CALLBACK to apply the
transformation.

For more implementation detail, see `apheleia--run-formatter-function'."
    (with-current-buffer scratch
      (funcall (with-current-buffer buffer major-mode))
      (setq-local indent-line-function (buffer-local-value 'indent-line-function buffer)
                  indent-tabs-mode (buffer-local-value 'indent-tabs-mode buffer))
      (goto-char (point-min))
      (let ((inhibit-message t)
            (message-log-max nil))
        (org-align-tags 'all)
        (krisb-org-ext-add-blank-lines 'whole-buffer)
        (indent-region (point-min) (point-max)))
      (funcall callback)))
  (add-to-list 'apheleia-mode-alist '(org-mode . krisb-org-formatter))
  (add-to-list 'apheleia-formatters '(krisb-org-formatter . krisb-apheleia-format-org-buffer)))

;;; Krisb-indentation
(use-package krisb-indentation
  :ensure nil
  :bind ([remap indent-region] . krisb-format-buffer-indentation))

;;; Provide
(provide 'krisb-formatting)
