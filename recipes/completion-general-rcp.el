;;; completion-general-rcp.el --- Emacs-wide completion settings  -*- lexical-binding: t; -*-

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

;; These are settings and/or packages which are package agnostic, some involved
;; with the default Emacs completion

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Built-in
;; A lot of this is taken from
;; https://protesilaos.com/dotemacs/#h:c110e399-3f43-4555-8427-b1afe44c0779
(setq completion-styles '(basic initials partial-completion flex)
      ;; A non-exhaustve list of known completion categories:
      ;;
      ;; - `bookmark'
      ;; - `buffer'
      ;; - `charset'
      ;; - `coding-system'
      ;; - `color'
      ;; - `command' (e.g. `M-x')
      ;; - `customize-group'
      ;; - `environment-variable'
      ;; - `expression'
      ;; - `face'
      ;; - `file'
      ;; - `function' (the `describe-function' command bound to `C-h f')
      ;; - `info-menu'
      ;; - `imenu'
      ;; - `input-method'
      ;; - `kill-ring'
      ;; - `library'
      ;; - `minor-mode'
      ;; - `multi-category'
      ;; - `package'
      ;; - `project-file'
      ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
      ;; - `theme'
      ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
      ;; - `variable' (the `describe-variable' command bound to `C-h v')
      ;; - `consult-grep'
      ;; - `consult-isearch'
      ;; - `consult-kmacro'
      ;; - `consult-location'
      ;; - `embark-keybinding'
      completion-category-overrides
      '((file (styles . (basic
                         partial-completion)))) ; Partial completion for file paths!
      completion-flex-nospace t
      completion-cycle-threshold nil ; Number of candidates until cycling turns off
      completion-lazy-hilit t        ; Performance; added Emacs 30.1
      completion-show-help nil
      completion-auto-help 'always
      completion-auto-select 'second-tab
      completions-max-height 20
      completion-ignore-case t
      completion-pcm-complete-word-inserts-delimiters nil
      completion-pcm-word-delimiters "-_./:| " ; Word delimiters

      ;; The following two are updated in Emacs 28.  They concern the
      ;; *Completions* buffer.
      completions-format 'one-column
      completions-detailed t ; Show more details in completion minibuffer (inspired by `marginalia')
      ;; Grouping of completions for Emacs 28
      completions-group t

      ;; Functionality of `indent-for-tab-command'. Make sure tab doesn't indent
      ;; when you want to perform completion
      tab-always-indent 'complete
      tab-first-completion 'word

      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t

      enable-recursive-minibuffers t   ; Allow minibuffer commands in minibuffer
      read-answer-short t           ; also check `use-short-answers' for Emacs28
      resize-mini-windows t         ; Not sure what this does
      ;; Truncates the default value part of the minibuffer prompt (often
      ;; "default ...") to something else
      minibuffer-default-prompt-format " [%s]")
(setq-default case-fold-search t)         ; For general regexp
;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Add prompt indicator to `completing-read-multiple'. We display
;; [CRM<separator>], e.g., [CRM,] if the separator is a comma. Taken from
;; https://github.com/minad/vertico
(defun crm-indicator (args)
  (cons (format "[completing-read-multiple: %s]  %s"
                (propertize
                 (replace-regexp-in-string
                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                  crm-separator)
                 'face 'error)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; TAB acts more like how it does in the shell
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;;;; Prescient
;;;;; Itself
;; Sorting and filtering of minibuffer candidates. The difference between
;; `orderless' and this package is that `orderless' filters but does not sort -
;; it leaves that up to the "candidate source and the completion UI."
;; Additionally, `orderless' has style "dispatchers," i.e., I can define
;; predicates for what filtering style to use for which token
(use-package prescient
  :disabled
  :demand
  :custom
  ;; NOTE 2024-02-03: Flex is chosen as a backup in case nothing in prescient is
  ;; matched, which only happens if I'm clueless about what I'm searching for.
  ;; We prefer this over adding the fuzzy matching in `prescient-filter-method'
  ;; because we don't want a bunch of random results included in the filtered
  ;; prescient results and cluttering it
  (completion-styles '(prescient flex))
  (prescient-filter-method
   '(literal initialism regexp))
  (prescient-sort-full-matches-first t)
  (prescient-history-length 200)
  (prescient-aggressive-file-save t)
  :config
  (prescient-persist-mode))

;;;;; Vertico-prescient
(use-package vertico-prescient
  :after (prescient vertico)
  :custom
  (vertico-prescient-enable-filtering t)
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil) ; Keep it up to vertico
  (vertico-prescient-completion-styles '(prescient flex))
  ;; See also `prescient--completion-recommended-overrides'
  (vertico-prescient-completion-category-overrides
   '((file (styles basic partial-completion))
     (eglot (styles prescient flex))))
  :init
  (vertico-prescient-mode))

;;;;; Corfu-prescient
(use-package corfu-prescient
  :after (prescient corfu)
  :custom
  (corfu-prescient-enable-filtering t)
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting t)
  (corfu-prescient-completion-styles '(prescient flex))
  ;; See also `prescient--completion-recommended-overrides'
  (corfu-prescient-completion-category-overrides
   '((file (styles basic partial-completion))
     (eglot (styles prescient flex))))
  :init
  (corfu-prescient-mode))

;;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-field-width 80)
  (marginalia-align-offset -2)          ; Two to the left
  :init
  (marginalia-mode))

(provide 'completion-general-rcp)
;;; completion-general-rcp.el ends here
