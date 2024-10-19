;;; completion-vanilla-rcp.el --- Completing-read based completion  -*- lexical-binding: t; -*-

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

;; Completion framework and cousin packages which are lightweight and faithful
;; to the base Emacs architecture.
;;
;; Additionally, you can see a benchmark of fuzzy finding completions in Emacs
;; here: https://github.com/axelf4/emacs-completion-bench#readme

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :custom
  (completion-styles
   ;; '(initials orderless substring basic flex))
   '(orderless flex))
  (orderless-matching-styles
   '(orderless-regexp
     orderless-prefixes
     orderless-initialism
     ;; orderless-literal
     ;; orderless-flex
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  ;; Sets many defaults unfavorable to `orderless', so I set it to nil to use
  ;; just the default `completion-styles'
  (completion-category-defaults nil)
  (completion-category-overrides
   '(;; Include `partial-completion' to enable wildcards and partial paths.
     (file (styles . (orderless partial-completion flex)))
     ;; Eglot forces `flex' by default.
     (eglot (styles orderless flex))
     ;; For citar
     (citar-candidate (styles basic substring))))
  (orderless-style-dispatchers '(kb/orderless-consult-dispatch))
  :config
  ;; Taken from Doom
  (defun kb/orderless-consult-dispatch (pattern _index _total)
    "Basically `orderless-affix-dispatch-alist' but with prefixes too."
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1 -1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "," pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "," pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1))))))

;;;; Hotfuzz
;; Faster version of the flex completion style.
(use-package hotfuzz
  :after orderless              ; Let orderless set up `completion-styles' first
  :demand
  :config
  ;; Replace flex style with hotfuzz style; it's much faster. See
  ;; https://github.com/axelf4/emacs-completion-bench#readme
  (setq completion-styles (cl-substitute 'hotfuzz 'flex completion-styles)))

(provide 'completion-vanilla-rcp)
;;; completion-vanilla-rcp.el ends here
