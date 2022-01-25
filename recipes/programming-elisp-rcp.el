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
(require 'buffers-and-windows-rcp)

;;; Elisp-mode
;; Elisp-mode overwrites my eyebrowse-last-window-config binding
(use-package elisp-mode
  :straight nil
  )

;;; Eros-mode
;; Overlay lisp evaluations into the current buffer (near cursor)
(use-package eros
  :ghook 'emacs-lisp-mode-hook
  :custom
  (eros-eval-result-prefix "⟹  ")       ; Fancy!
  )

;;; Lispyville
(use-package lispyville
  :hook ((emacs-lisp-mode lisp-mode) . lispyville-mode)
  :ghook 'turn-off-smartparens-mode
  :custom
  (lispy-close-quotes-at-end-p t)
  :config
  (lispyville-set-key-theme
   '(;; Just the theme symbol will apply to normal and visual mode, unless there
     ;; is a corresponding set of default modes
     (operators normal)
     ;; c-w
     prettify
     text-objects
     (atom-movement t)
     (escape insert)
     commentary
     ;; slurp/barf-cp
     slurp/barf-lispy
     (additional-movement normal visual motion)
     ;; wrap
     additional-wrap
     additional
     additional-insert
     ;; arrows
     ))

  ;; Evil-collection-unimpaired-mode conflicts with the additional-movement
  ;; them. So Move those prefix elsewhere when lispyville-mode-map is active.
  (general-define-key
   :keymaps '(lispyville-mode-map general-override-mode-map)
   :states 'normal
   ", b" 'evil-prev-buffer
   ". b" 'evil-next-buffer
   ", e" 'evil-collection-unimpaired-move-text-up
   ". e" 'evil-collection-unimpaired-move-text-down
   ", l" 'evil-collection-unimpaired-previous-error
   ". l" 'evil-collection-unimpaired-next-error
   ", L" 'evil-collection-unimpaired-first-error
   ". L" 'evil-collection-unimpaired-last-error
   ", q" 'evil-collection-unimpaired-previous-error
   ". q" 'evil-collection-unimpaired-next-error
   ", Q" 'evil-collection-unimpaired-first-error
   ". Q" 'evil-collection-unimpaired-last-error
   ", n" 'evil-collection-unimpaired-previous-SCM-conflict-marker
   ". n" 'evil-collection-unimpaired-next-SCM-conflict-marker
   ", p" 'evil-collection-unimpaired-paste-above
   ". p" 'evil-collection-unimpaired-paste-below
   ", P" 'evil-collection-unimpaired-paste-above
   ". P" 'evil-collection-unimpaired-paste-below
   ", SPC" 'evil-collection-unimpaired-insert-newline-above
   ". SPC" 'evil-collection-unimpaired-insert-newline-below)
  (general-define-key
   :keymaps 'lispyville-mode-map
   :states 'motion
   ", e" 'evil-collection-unimpaired-move-text-up
   ". e" 'evil-collection-unimpaired-move-text-down
   ", n" 'evil-collection-unimpaired-previous-SCM-conflict-marker
   ". n" 'evil-collection-unimpaired-next-SCM-conflict-marker)
  (general-define-key
   :keymaps 'lispyville-mode-map
   :states 'normal
   ", u" 'evil-collection-unimpaired-url-encode
   ". u" 'evil-collection-unimpaired-url-decode
   ", 6" 'evil-collection-unimpaired-b64-encode
   ". 6" 'evil-collection-unimpaired-b64-decode)

  ;; General
  (evil-define-key 'insert lispyville-mode-map
    (kbd "C-g") #'lispyville-normal-state)

  ;; Commentary
  (evil-define-key 'normal lispyville-mode-map
    "gc" #'lispyville-comment-or-uncomment
    "gy" #'lispyville-comment-and-clone-dwim
    (kbd "g/") #'lispyville-comment-or-uncomment-line)

  ;; Additional-movement
  (general-define-key
   :keymaps '(lispyville-mode-map general-override-mode-map)
   :states '(normal visual motion)
   "[" #'lispyville-previous-opening
   "]" #'lispyville-next-closing
   "M-h" #'lispyville-beginning-of-defun
   "M-l" #'lispyville-end-of-defun))

;;; Syntax highlighting
;;;; Lisp-extra-font-lock
;; Give faces to elisp symbols
(use-package lisp-extra-font-lock
  :init
  (lisp-extra-font-lock-global-mode)
  )

;;;; Highlight-function-calls
;; Give function calls a special face (default is underline)
(use-package highlight-function-calls
  :ghook 'emacs-lisp-mode-hook
  )

;;;; Rainbow-delimiters
;; Highlight matching delimiters (e.g. parenthesis)
(use-package rainbow-delimiters
  :ghook 'prog-mode-hook
  )

;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
  :gfhook 'visual-line-mode
  :ryo
  ("f" helpful-at-point)
  :general
  ;; NOTE 2021-08-20: Emacs' describe-function includes both functions and
  ;; macros
  ([remap describe-function] '(helpful-callable :wk "Helpful function")
   [remap describe-command] '(helpful-command :wk "Helpful command")
   [remap describe-variable] '(helpful-variable :wk "Helpful variable")
   [remap describe-symbol] '(helpful-symbol :wk "Helpful symbol")
   [remap describe-key] '(helpful-key :wk "Helpful key")
   [remap apropos-command] '(helpful-command :wk "Helpful command")
   )
  (:states '(visual normal motion)
            "f" 'helpful-at-point
            )
  (kb/help-keys
    "K" '(describe-key-briefly :wk "Desc key echo")
    "a" '(apropos-command :wk "Apropos command")
    "f" '(describe-function :wk "Desc func")
    "ha" '(consult-apropos :wk "Consult apropos")
    "k" '(helpful-key :wk "Desc key")
    "o" '(describe-symbol :wk "Desc sym")
    "v" '(describe-variable :wk "Desc var")
    "w" '(where-is :wk "Where is...?")
    )
  :custom
  (describe-bindings-outline t) ; Include interactive outline headings for each major mode in `describe-keys' buffer
  )

;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :demand t
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  )

;;; programming-elisp-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-elisp-rcp)
