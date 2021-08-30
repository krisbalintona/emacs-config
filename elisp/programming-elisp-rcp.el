;;; programming-elisp-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages that are helpful for programming or working in elisp.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'buffer-and-window-management-rcp)

;;;; Elisp-mode
;; Elisp-mode overwrites my eyebrowse-last-window-config binding
(use-package elisp-mode
  :straight nil
  )

;;;; Eros-mode
;; Overlay lisp evaluations into the current buffer (near cursor)
(use-package eros
  :ghook 'emacs-lisp-mode-hook
  )

;;;; Syntax highlighting
;;;;; Lisp-extra-font-lock
;; Give faces to elisp symbols
(use-package lisp-extra-font-lock
  :ghook 'emacs-lisp-mode-hook
  )

;;;;; Highlight-function-calls
;; Give function calls a special face (default is underline)
(use-package highlight-function-calls
  :ghook 'emacs-lisp-mode-hook
  )

;;;;; Rainbow-delimiters
;; Highlight matching delimiters (e.g. parenthesis)
(use-package rainbow-delimiters
  :ghook 'prog-mode-hook
  )

;;;; Custom font lock words
;; Mainly for `prot-comment-timestamp-keyword'. Faces taken from
;; `org-todo-keyword-faces' in org-agenda-general-rcp.el.
(font-lock-add-keywords 'emacs-lisp-mode
                        '(;; TODO wrapped between whitespace
                          ("\\s-TODO\\s-" 0 '(t :foreground "orange") t)
                          ;; NOTE wrapped between whitespace
                          ("\\s-NOTE\\s-" 0 '(t :foreground "turquoise") t)
                          ;; REVIEW wrapped between whitespace
                          ("\\s-REVIEW\\s-" 0 '(t :foreground "orchid") t)
                          ;; FIXME wrapped between whitespace
                          ("\\s-FIXME\\s-" 0 '(t :foreground "deep pink") t)
                          ))

;;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
  :gfhook 'visual-line-mode
  :general
  ;; NOTE 2021-08-20: Emacs' describe-function includes both functions and
  ;; macros
  ([remap describe-function] '(helpful-callable :which-key "Helpful function")
   [remap describe-command] '(helpful-command :which-key "Helpful command")
   [remap describe-variable] '(helpful-variable :which-key "Helpful variable")
   [remap describe-symbol] '(helpful-symbol :which-key "Helpful symbol")
   [remap describe-key] '(helpful-key :which-key "Helpful key")
   )
  (:states '(visual normal motion)
           "f" 'helpful-at-point
           )
  (kb/leader-keys
    "hk" '(helpful-key :which-key "Desc key")
    "hc" '(helpful-command :which-key "Helpful command"))
  :custom
  (describe-bindings-outline t) ; Include interactive outline headings for each major mode in `describe-keys' buffer
  )

;;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :requires helpful
  :commands elisp-demos-advice-helpful-update
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  )

;;; programming-elisp-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-elisp-rcp)
