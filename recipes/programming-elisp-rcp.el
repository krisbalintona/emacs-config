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
  :if (bound-and-true-p evil-local-mode)
  :hook ((emacs-lisp-mode lisp-mode) . lispyville-mode)
  :gfhook 'turn-off-smartparens-mode
  :custom
  (lispy-close-quotes-at-end-p t)
  (lispyville-key-theme
   '(;; Just the theme symbol will apply to normal and visual mode, unless there
     ;; is a corresponding set of default modes
     ;; (operators normal)
     c-w
     prettify
     text-objects
     (atom-movement t)
     ;; (escape insert)     ; But where it inserts "l" if point is before an l char
     commentary
     c-u
     ;; slurp/barf-cp
     slurp/barf-lispy
     additional-movement
     ;; wrap
     additional-wrap
     additional
     additional-insert
     ;; arrows
     ))
  :config
  (lispyville-set-key-theme)

  ;; Evil-collection-unimpaired-mode conflicts with the additional-movement
  ;; them. So Move those prefix elsewhere when lispyville-mode-map is active.
  (general-define-key
   :keymaps 'lispyville-mode-map
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
  (general-define-key
   :keymaps 'lispyville-mode-map
   :states 'insert
   "C-g" #'lispyville-normal-state)

  ;; Commentary
  (general-define-key
   :keymaps 'lispyville-mode-map
   :states 'normal
   "gc" #'lispyville-comment-or-uncomment
   "gy" #'lispyville-comment-and-clone-dwim
   "g/" #'lispyville-comment-or-uncomment-line)

  ;; Additional-movement
  (add-hook 'lispyville-mode-hook #'(lambda ()
                                      (general-define-key
                                       :keymaps '(lispyville-mode-map local)
                                       :states '(normal visual motion)
                                       "[" #'lispyville-previous-opening
                                       "]" #'lispyville-next-closing
                                       "M-h" #'lispyville-beginning-of-defun
                                       "M-l" #'lispyville-end-of-defun))))

;;; Lispy
;; TODO 2022-06-04: Also get `special-lispy-shifttab' working
(use-package lispy
  :disabled t 
  :if (not (bound-and-true-p evil-local-mode))
  :ghook 'emacs-lisp-mode-hook
  :hook (minibuffer-setup . (lambda ()        ; Also use lispy for `eval-expression'
                             (when (eq this-command 'eval-expression)
                               (lispy-mode 1))))
  :gfhook
  'turn-off-smartparens-mode
  '(lambda ()
     "Set `outline-level' and `outline-regexp' to the values outshine
sets since lispy changes the local values."
     (when (bound-and-true-p outshine-mode)
       (setq-local outline-level #'outshine-calc-outline-level
                   outline-regexp ";;[;]\\{1,8\\} ")))
  :general (:keymaps 'lispy-mode-map
                     [remap lispy-shifttab] 'outshine-kbd-<backtab>
                     [remap lispy-meta-return] 'outshine-insert-heading))

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
  :general
  (:keymaps 'helpful-mode-map
            (general-chord "jj") 'helpful-at-point)
  ;; NOTE 2021-08-20: Emacs' describe-function includes both functions and
  ;; macros
  ([remap describe-function] 'helpful-function
   [remap describe-command] 'helpful-command
   [remap describe-variable] 'helpful-variable
   [remap describe-symbol] 'helpful-symbol
   [remap describe-key] 'helpful-key
   [remap apropos-command] 'helpful-command))

;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :demand t
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;; Suggest
;; Query `suggest' for elisp coding suggestions!
(use-package suggest
  :general ("C-c S" '(suggest :wk "Suggest.el buffer")))

;;; programming-elisp-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-elisp-rcp)
