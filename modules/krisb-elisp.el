;; -*- lexical-binding: t; -*-

;;; Font locking
;;;; Highlight-function-calls
(use-package highlight-function-calls
  :hook ((emacs-lisp-mode lisp-interaction-mode) . highlight-function-calls-mode)
  :custom
  (highlight-function-calls-not nil)
  (highlight-function-calls-macro-calls nil)
  (highlight-function-calls-special-forms nil)
  :custom-face
  (highlight-function-calls-face ((t (:underline nil :inherit font-lock-function-call-face)))))

;;;; Paren-face
;; Creates a face just for parentheses. Useful for lispy languages where readers
;; want the parentheses as unnoticeable as possible.
(use-package paren-face
  :custom
  (paren-face-mode-lighter "")
  :config
  (global-paren-face-mode 1))

;;;; Highlight-parentheses
(use-package highlight-parentheses
  :diminish
  :hook ((emacs-lisp-mode . highlight-parentheses-mode)
         (minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (highlight-parentheses-mode . (lambda () (show-paren-local-mode -1))))
  :custom
  ;; TODO 2025-04-08: Configure `highlight-parentheses-colors'
  (highlight-parentheses-highlight-adjacent t)
  :config
  ;; See also (info "(modus-themes) Note on highlight-parenthesesel").
  (krisb-modus-themes-setup-faces
   "highlight-parentheses"
   (set-face-attribute 'highlight-parentheses-highlight nil :inherit 'bold)
   (modus-themes-with-colors
     ;; (let ((color yellow-intense))
     (let ((color red-warmer))
       (setopt
        highlight-parentheses-attributes (make-list 3 '(:inherit bold))
        highlight-parentheses-colors (cl-loop for amt in (cl-loop for i from 0 below 5 collect (* i 15))
                                              collect (color-darken-name color amt))
        highlight-parentheses-background-colors nil)))))

;;; Documentation
;;;; Help
(use-package help
  :ensure nil
  :bind ("C-h C-k" . describe-keymap)
  :custom
  (help-window-select t)
  (help-window-keep-selected t)

  (help-enable-variable-value-editing t)
  (help-clean-buttons t)
  (help-enable-symbol-autoload t)

  (describe-bindings-outline t)
  (describe-bindings-show-prefix-commands t)

  (help-at-pt-display-when-idle t)
  :config
  (require 'shortdoc)
  (add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function))

;;;; Apropos
(use-package apropos
  :ensure nil
  :bind ("C-h u" . apropos-user-option))

;;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :config
  (add-hook 'help-fns-describe-function-functions #'elisp-demos-advice-describe-function-1)

  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;;; Find-funtion-mode
;; Binds useful commands for jumping to variables, functions, and libraries
(use-package find-func
  :ensure nil
  :bind ( :map tab-prefix-map
          ("F" . krisb-find-function-other-tab)
          ("L" . krisb-find-library-other-tab))
  :init
  ;; Useful keybinds for my usage
  (defun krisb-find-library-other-tab (library)
    "Find LIBRARY in other tab."
    (interactive (list (read-library-name)))
    (switch-to-buffer-other-tab (save-window-excursion (funcall-interactively #'find-library library))))

  (defun krisb-find-function-other-tab (function)
    "Find FUNCTION in other tab."
    (interactive (find-function-read))
    (find-function-do-it function nil 'switch-to-buffer-other-tab))
  :config
  (find-function-mode 1))

;;; Debugging
;;;; Eros-mode
;; Overlay lisp evaluations into the current buffer (near cursor)
(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  :custom
  (eros-eval-result-prefix "⟹  "))

;;;; Inspector
;; Introspect list expressions.  This is similar in role to CEDET's
;; data-debug.el.  Also integrates with the debugging backtrace and edebug (see
;; https://github.com/mmontone/emacs-inspector?tab=readme-ov-file#from-the-emacs-debugger).
(use-package inspector)

;;;; IELM
(use-package ielm
  :ensure nil
  :custom
  (ielm-noisy nil)
  (ielm-dynamic-return nil))

;;;; Edebug
(use-package edebug
  :ensure nil
  :custom
  (edebug-initial-mode 'step)
  :config
  ;; Better indication for evaluated sexps in during edebugging. Taken from
  ;; https://xenodium.com/inline-previous-result-and-why-you-should-edebug/.

  (with-eval-after-load 'eros
    (defun krisb-edebug-previous-result--around (_ &rest r)
      "Adviced `edebug-previous-result'."
      (require 'eros)
      (eros--make-result-overlay edebug-previous-result
        :where (point)
        :duration eros-eval-result-duration))
    (advice-add #'edebug-previous-result :around #'krisb-edebug-previous-result--around))

  (defun krisb-edebug-compute-previous-result (previous-value)
    (if edebug-unwrap-results
        (setq previous-value
              (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
          (concat "Result: "
                  (edebug-safe-prin1-to-string previous-value)
                  (eval-expression-print-format previous-value))))

  (defun edebug-previous-result ()
    "Print the previous result."
    (interactive)
    (message "%s" edebug-previous-result))

  (defun adviced:edebug-compute-previous-result (_ &rest r)
    "Adviced `krisb-edebug-compute-previous-result'."
    (let ((previous-value (nth 0 r)))
      (if edebug-unwrap-results
          (setq previous-value
                (edebug-unwrap* previous-value)))
      (setq edebug-previous-result
            (edebug-safe-prin1-to-string previous-value))))
  (advice-add #'krisb-edebug-compute-previous-result :around #'adviced:edebug-compute-previous-result))

;;;; Lazy-guard
;; Help validate Emacs configuration.  See https://codeberg.org/vifon/lazy-guard
;; for more information, including how to automate this with git commit hooks.
;; NOTE 2025-03-13: Using the git hook method results in false positives from
;; faulty autoloading; I think it's related to use-package, I think.  So I don't
;; use that method for now.
(use-package lazy-guard
  :vc ( :url "https://codeberg.org/vifon/lazy-guard.git")
  :custom
  (lazy-guard-autoloads-strict nil)
  (lazy-guard-autoloads-known-broken nil))

;;; Packages
;;;; Try
;; Install a package only for the current Emacs session.
(use-package try
  :config
  ;; Add `try' to embark keymap for packages
  (with-eval-after-load 'embark
    (keymap-set embark-package-map "t" #'try)))

;;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  :hook (scratch-create-buffer . krisb-scratch-buffer-setup)
  :bind ( :map krisb-open-keymap
          ("S". scratch))
  :config
  (defun krisb-scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
 Taken from
 https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/"
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t))))

;;; Other
;;;; Suggest
;; Query `suggest' for elisp coding suggestions!
(use-package suggest
  :bind ( :map krisb-open-keymap
          ("C-s" . suggest))
  :custom
  (suggest-insert-example-on-start nil))

;;;; Package authorship
;;;;; Package-lint-flymake
(use-package package-lint-flymake
  :hook (emacs-lisp-mode . package-lint-flymake-setup))

;;;;; Org-make-toc
(use-package org-make-toc
  :custom
  (org-make-toc-insert-custom-ids t))

;;;;; Package-build
;; For help with creating package recipes for, e.g., MELPA.  See
;; https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org#preparing-a-pull-request-to-melpa
;; for how package-build is used to create MELPA PRs.
(use-package package-build
  :custom
  (package-build-recipes-dir (expand-file-name "recipes/" krisb-melpa-directory))
  (package-build-archive-dir  (expand-file-name "packages/" krisb-melpa-directory))
  (package-build-working-dir (expand-file-name "working/" krisb-melpa-directory)))

;;;;; Lice
;; Insert license headers for emacs-lisp files.
(use-package lice)

;;;;; Mode-minder
;; Visually represent the hierarchy of major mode inheritance via
;; tabulated-list-mode.  NOTE: Calling `mode-minder' loads all packages with a
;; known major mode.
(use-package mode-minder
  :vc ( :url "https://github.com/jdtsmith/mode-minder.git"
        :rev :newest))

;;; Provide
(provide 'krisb-elisp)
