;; -*- lexical-binding: t; -*-

;;; Goto definition

;;;; Consult-xref-stack
(use-package consult-xref-stack
  :vc ( :url "https://github.com/brett-lempereur/consult-xref-stack"
        :rev :newest)
  :bind (([remap xref-go-back] . krisb-consult-xref-stack-backward)
         ([remap xref-go-forward] . krisb-consult-xref-stack-forward))
  :config
  (defun krisb-consult-xref-stack-backward (arg)
    "Call `xref-go-back' or `consult-xref-stack-backward' when called with ARG."
    (interactive "p")
    (call-interactively
     (if (< 1 arg) 'consult-xref-stack-backward 'xref-go-back)))

  (defun krisb-consult-xref-stack-forward (arg)
    "Call `xref-go-forward' or `consult-xref-stack-forward' when called with ARG."
    (interactive "p")
    (call-interactively
     (if (< 1 arg) 'consult-xref-stack-forward 'xref-go-forward))))

;;;; Dumber-jump
;; A lean fork of dumb-jump.  For a list of supported languages, see
;; https://github.com/zenspider/dumber-jump?tab=readme-ov-file#supported-languages.
(use-package dumber-jump
  ;; For AUR:
  ;; :ensure-system-package (rg . ripgrep)
  :custom
  (dumber-jump-default-project user-emacs-directory)
  :init
  ;; Add to global value so it is used as a fallback (when local value ends in
  ;; t)
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumber-jump-xref-activate 100))
  :config
  (setopt dumber-jump-project-denoters
          (cl-remove-duplicates
           (append dumber-jump-project-denoters project-vc-extra-root-markers))))

;;; Hl-todo
;; Highlight todos
(use-package hl-todo
  :custom
  (hl-todo-include-modes
   '(prog-mode text-mode conf-mode))
  (hl-todo-text-modes '(text-mode))
  (hl-todo-exclude-modes nil)
  :config
  (global-hl-todo-mode 1)

  ;; Bespoke regexp-creating command.  In
  ;; https://github.com/tarsius/hl-todo/issues/42, the package author explained
  ;; their reasoning as to why regexps are not limited only to comments: todos
  ;; in docstrings are useful and backwards compatibility.  This is different
  ;; from my use-case though, so I manually enable highlighting only in comments
  (el-patch-defun hl-todo--setup-regexp ()
    "Setup keyword regular expression.
See the function `hl-todo--regexp'."
    (when-let ((bomb (assoc "???" hl-todo-keyword-faces)))
      ;; If the user customized this variable before we started to treat the
      ;; strings as regexps, then the string "???" might still be present.  We
      ;; have to remove it because it results in the regexp search taking
      ;; forever.
      (setq hl-todo-keyword-faces (delete bomb hl-todo-keyword-faces)))
    (setq hl-todo--regexp
          (concat (el-patch-add (bound-and-true-p comment-start-skip))
                  "\\(\\<"
                  "\\(" (mapconcat #'car hl-todo-keyword-faces "\\|") "\\)"
                  "\\>"
                  (and (not (equal hl-todo-highlight-punctuation ""))
                       (concat "[" hl-todo-highlight-punctuation "]"
                               (if hl-todo-require-punctuation "+" "*")))
                  "\\)"))))

;;; Compile-multi
(use-package compile-multi
  :bind (([remap project-compile] . compile-multi)
         :map project-prefix-map
         ("c" . compile-multi))
  :custom
  (compile-multi-default-directory (lambda () (project-root (project-current))))
  :config
  (use-package consult-compile-multi
    :after consult
    :config
    (consult-compile-multi-mode 1))

  (use-package compile-multi-nerd-icons
    :after nerd-icons-completion))

;;; Provide
(provide 'krisb-projects)
