;; -*- lexical-binding: t; -*-

;;; Font locking

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

;;; Debugging

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
