;;; Project
(use-package project
  :custom
  (project-vc-extra-root-markers '("Makefile"))
  (project-vc-merge-submodules nil)

  (project-file-history-behavior 'relativize)

  (project-mode-line t)
  (project-mode-line-face nil))

;;; Xref
(use-package xref
  :bind ("C-M-?". xref-find-references-and-replace) ; Emacs 29.1
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function #'xref-show-definitions-buffer)
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep)
  (xref-history-storage 'xref-window-local-history) ; Per-window history of `xref-go-*'
  :config
  ;; We remove the fallback backend, `etags--xref-backend', which prompts the
  ;; user for an etags table -- this is undesirable for me.
  (setq-default xref-backend-functions nil)
  ;; Then add `elisp--xref-backend' as the global value of
  ;; `xref-backend-functions', which means it is run when the local value ends
  ;; with `t'. See (info "(elisp) Running Hooks") for an explanation.
  (add-hook 'xref-backend-functions #'elisp--xref-backend)

  ;; Revealing headings
  (with-eval-after-load 'krisb-reveal
    (defun krisb-reveal-xref-find-information ()
      "Return information required by `krisb-reveal-fold-commands'.
See the docstring of `krisb-reveal-fold-commands'."
      (save-window-excursion
        (save-excursion
          (xref-goto-xref)
          (cons (point) (current-buffer)))))
    (dolist (command '(xref-prev-line
                       xref-next-line
                       xref-quit-and-goto-xref))
      (add-to-list 'krisb-reveal-fold-commands
                   (cons command #'krisb-reveal-xref-find-information)))))

;;; Provide
(provide 'krisb-projects)
