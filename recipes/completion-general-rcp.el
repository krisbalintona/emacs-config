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

(provide 'completion-general-rcp)
;;; completion-general-rcp.el ends here
