;;; programming-elisp-rcp.el --- Emacs-lisp          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These are packages that are helpful for programming or working in elisp.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'buffers-and-windows-rcp)

;;;; Ielm
(use-package ielm
  :ensure nil
  :custom
  (ielm-noisy nil)
  (ielm-dynamic-return nil))

;;;; Lisp-keyword-indent
;; Better keyword, cl-defun, and cl-loop indenting. See
;; https://github.com/twlz0ne/lisp-keyword-indent.el#usage for examples
(use-package lisp-keyword-indent
  :disabled                             ; Change of heart...?
  :demand
  :ensure (lisp-keyword-indent :type git
                               :host github
                               :repo "twlz0ne/lisp-keyword-indent.el")
  :functions kb/lisp-keyword-indent-mode
  :init
  ;; FIXME 2024-01-14: kb/lisp-keyword-indent-allow isn't being set properly
  (defvar-local kb/lisp-keyword-indent-allow nil
    "Whether to allow `lisp-keyword-indent-mode' in current buffer.")
  (define-minor-mode kb/lisp-keyword-indent-mode
    "Minor mode for keyword indent of Emacs Lisp."
    :init-value nil
    :lighter (:eval (when (and kb/lisp-keyword-indent-mode
                               (derived-mode-p 'emacs-lisp-mode))
                      (propertize " LKI" 'face '(:inherit shadow))))
    :keymap nil
    :global t
    (if kb/lisp-keyword-indent-mode
        (advice-add 'lisp-indent-function :override 'lisp-keyword-indent)
      (when kb/lisp-keyword-indent-allow
        (advice-remove 'lisp-indent-function 'lisp-keyword-indent))))
  :config
  (kb/lisp-keyword-indent-mode))

;;;; Eros-mode
;; Overlay lisp evaluations into the current buffer (near cursor)
(use-package eros
  :ghook 'emacs-lisp-mode-hook
  :custom
  (eros-eval-result-prefix "⟹  "))      ; Fancy!

;;;; Syntax highlighting
;;;;; Lisp-extra-font-lock
;; Give faces to elisp symbols
(use-package lisp-extra-font-lock
  :disabled t
  :init
  (lisp-extra-font-lock-global-mode))

;;;;; Highlight-function-calls
;; Give function calls a special face (default is underline)
(use-package highlight-function-calls
  :disabled t
  :ghook 'emacs-lisp-mode-hook)

;;;;; Rainbow-delimiters
;; Highlight matching delimiters (e.g. parenthesis)
(use-package rainbow-delimiters
  :ghook 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook 'inferior-emacs-lisp-mode-hook)

;;;; Help
(use-package help
  :ensure nil
  :general ("C-h M-k" 'describe-keymap)
  :custom
  (help-window-select t)
  (help-window-keep-selected t)

  (help-enable-variable-value-editing t)
  (help-clean-buttons t)

  (describe-bindings-outline t)
  (describe-bindings-show-prefix-commands t)
  :config
  (add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function))

;;;; Help-find
;; Provides `help-find-function' and `help-find-keybinding'
(use-package help-find
  :general
  (:keymaps 'help-map
            :prefix "M-f"
            "f" 'help-find-function
            "r" 'help-find-keybinding))

;;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
  :disabled                          ; Trying out built-in `help' functionality
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

;;;; Edebug
(use-package edebug
  :ensure nil
  :custom
  (edebug-initial-mode 'go)
  :init
  ;; The following is taken from
  ;; https://xenodium.com/inline-previous-result-and-why-you-should-edebug/.
  ;; Better indication for evaluated sexps in during edebugging.

  (defun adviced:edebug-previous-result (_ &rest r)
    "Adviced `edebug-previous-result'."
    (require 'eros)
    (eros--make-result-overlay edebug-previous-result
      :where (point)
      :duration eros-eval-result-duration))

  (defun edebug-compute-previous-result (previous-value)
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
    "Adviced `edebug-compute-previous-result'."
    (let ((previous-value (nth 0 r)))
      (if edebug-unwrap-results
          (setq previous-value
                (edebug-unwrap* previous-value)))
      (setq edebug-previous-result
            (edebug-safe-prin1-to-string previous-value))))

  (advice-add #'edebug-compute-previous-result
              :around #'adviced:edebug-compute-previous-result)

  (advice-add #'edebug-previous-result
              :around #'adviced:edebug-previous-result))

;;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :demand
  :config
  (add-hook 'help-fns-describe-function-functions #'elisp-demos-advice-describe-function-1)
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;;; Suggest
;; Query `suggest' for elisp coding suggestions!
(use-package suggest
  :general (kb/open-keys
             "S" 'suggest)
  :custom
  (suggest-insert-example-on-start nil))

(provide 'programming-elisp-rcp)
;;;; Recursion-indicator
(use-package recursion-indicator
  :init
  (recursion-indicator-mode)
  (minibuffer-depth-indicate-mode -1)
  :config
  ;; Thanks to Daniel Mendler for this! It removes the square brackets that
  ;; denote recursive edits in the modeline. I do not need them because I am
  ;; using Daniel's `recursion-indicator':
  ;; <https://github.com/minad/recursion-indicator>.
  (with-eval-after-load 'themes-rcp
    (setq-default kb/mode-line-modes
                  (seq-filter (lambda (s)
                                (not (and (stringp s)
                                          (string-match-p
                                           "^\\(%\\[\\|%\\]\\)$" s))))
                              kb/mode-line-modes))))

;;; programming-elisp-rcp.el ends here
