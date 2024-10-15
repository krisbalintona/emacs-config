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

;;;; Highlight-function-calls
(use-package highlight-function-calls
  :hook ((emacs-lisp-mode lisp-interaction-mode) . highlight-function-calls-mode)
  :custom
  (highlight-function-calls-not nil)
  (highlight-function-calls-macro-calls nil)
  (highlight-function-calls-special-forms nil)
  :custom-face
  (highlight-function-calls-face ((t (:underline nil :inherit font-lock-function-call-face)))))

;;;; IELM
(use-package ielm
  :ensure nil
  :custom
  (ielm-noisy nil)
  (ielm-dynamic-return nil))

;;;; Eros-mode
;; Overlay lisp evaluations into the current buffer (near cursor)
(use-package eros
  :hook
  (emacs-lisp-mode . eros-mode)
  :custom
  (eros-eval-result-prefix "⟹  "))

;;;; Rainbow-delimiters
;; Highlight matching delimiters (e.g. parenthesis)
(use-package rainbow-delimiters
  :disabled          ; I think with my experience, I now favor `paren-face-mode'
  :hook
  ((emacs-lisp-mode lisp-interaction-mode inferior-emacs-lisp-mode) . rainbow-delimiters-mode))

;;;; Paren-faces
(use-package paren-face
  :demand
  :custom
  (paren-face-mode-lighter "")
  :config
  (global-paren-face-mode 1))

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
  :config
  (add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function))

;;;; Help-find
;; Provides `help-find-function' and `help-find-keybinding'
(use-package help-find
  :bind
  ( :map help-map
    ("M-f f" . help-find-function)
    ("M-f r" . help-find-keybinding)))

;;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
  :disabled                          ; Trying out built-in `help' functionality
  :hook
  (helpful-mode . visual-line-mode)
  :bind
  (([remap describe-function] . helpful-function)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-key] . helpful-key)
   ([remap apropos-command] . helpful-command))
  :chords
  ( :map helpful-mode-map
    ("jj" . helpful-at-point)))

;;;; Apropos
(use-package apropos
  :ensure nil
  :bind ("C-h u" . apropos-user-option))

;;;; Edebug
(use-package edebug
  :ensure nil
  :custom
  (edebug-initial-mode 'step)
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
  :config
  (advice-add #'edebug-compute-previous-result
              :around #'adviced:edebug-compute-previous-result)

  (advice-add #'edebug-previous-result
              :around #'adviced:edebug-previous-result))

;;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :defer 7
  :config
  (add-hook 'help-fns-describe-function-functions #'elisp-demos-advice-describe-function-1)
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;;; Suggest
;; Query `suggest' for elisp coding suggestions!
(use-package suggest
  :bind
  ( :map kb/open-keys
    ("S" . suggest))
  :custom
  (suggest-insert-example-on-start nil))

;;;; Recursion-indicator
(use-package recursion-indicator
  :hook (on-first-buffer . recursion-indicator-mode)
  :config
  (minibuffer-depth-indicate-mode -1)

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

;;;; Inspector
;; Introspect list expressions. Also integrates with the debugging backtrace and
;; edebug (see
;; https://github.com/mmontone/emacs-inspector?tab=readme-ov-file#from-the-emacs-debugger).
(use-package inspector)

(provide 'programming-elisp-rcp)
;;; programming-elisp-rcp.el ends here
