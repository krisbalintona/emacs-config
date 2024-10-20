;;; programming-general-rcp.el --- General programming stuff  -*- lexical-binding: t; -*-

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

;; Language-agnostic packages helpful or required for programming.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Electric
(use-package electric
  :ensure nil
  :hook (on-first-input . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-default-inhibit)
  (electric-quote-comment nil)
  (electric-quote-string nil)
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  (electric-quote-inhibit-functions nil))

;;;; Eldoc
(use-package eldoc
  :diminish
  :bind ( :map help-map
          ("\." . eldoc-doc-buffer))
  :custom
  (eldoc-print-after-edit nil)
  (eldoc-idle-delay 0.2)
  (eldoc-documentation-strategy
   'eldoc-documentation-compose-eagerly) ; Mash multiple sources together and display eagerly
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit) ; Also respects `max-mini-window-height'
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t))

;;;; Eldoc-box
(use-package eldoc-box
  :disabled                             ; Only intrusive
  :diminish eldoc-box-hover-mode
  ;; :hook (eldoc-mode . eldoc-box-hover-mode)
  :bind
  (([remap eldoc-doc-buffer] . eldoc-box-help-at-point)
   :map eglot-mode-map
   ([remap eldoc-box-help-at-point] . eldoc-box-eglot-help-at-point))
  :custom
  (eldoc-box-max-pixel-width 650)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-cleanup-interval 0.5)
  (eldoc-box-only-multi-line t)
  (eldoc-box-fringe-use-same-bg t)
  (eldoc-box-self-insert-command-list
   '(self-insert-command outshine-self-insert-command))
  :config
  ;; Workaround for many hyphen characters wrapping in an ugly way in
  ;; `eldoc-box' frame
  (defun kb/eglot--format-markup (markup)
    "Format MARKUP according to LSP's spec."
    (pcase-let ((`(,string ,mode)
                 (if (stringp markup) (list markup 'gfm-view-mode)
                   (list (plist-get markup :value)
                         (pcase (plist-get markup :kind)
                           ("markdown" 'gfm-view-mode)
                           ("plaintext" 'text-mode)
                           (_ major-mode))))))
      (with-temp-buffer
        (setq-local markdown-fontify-code-blocks-natively t)

        ;; In markdown, replace the horizontal rule, which is three hyphens in
        ;; the markup, with X number of hyphens-like characters, with X being
        ;; enough to cover the width of `eldoc-box-max-pixel-width'. We can't
        ;; simply replace with more hyphens since `gfm-view-mode' renders any
        ;; set of three hyphens as a horizontal rule
        (setq string (string-replace
                      "---"
                      (make-string (floor (/ eldoc-box-max-pixel-width (window-font-width))) ?⎺)
                      string))

        (insert string)
        (delete-trailing-whitespace) ; Also remove trailing whitespace while we're here
        (let ((inhibit-message t)
              (message-log-max nil)
              match)
          (ignore-errors (delay-mode-hooks (funcall mode)))
          (font-lock-ensure)
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (when (fboundp 'text-property-search-forward) ;; FIXME: use compat
              (while (setq match (text-property-search-forward 'invisible))
                (delete-region (prop-match-beginning match)
                               (prop-match-end match)))))
          (string-trim (buffer-string))))))
  (advice-add 'eglot--format-markup :override #'kb/eglot--format-markup))

;;;; Compile
(use-package compile
  :ensure nil
  :bind
  ("<f5>" . recompile)
  :custom
  (compilation-scroll-output 'first-error) ; Scroll with compile buffer
  (compilation-auto-jump-to-first-error 'if-location-known))

;;;; Consult
;; Counsel equivalent for default Emacs completion. It provides many useful
;; commands.
(use-package consult
  :bind
  ( :map goto-map                        ; Uses the `M-g' prefix
    ("e" . consult-compile-error)
    ("m" . consult-mark)
    ("M" . consult-global-mark)
    ("I" . consult-imenu-multi)
    :map search-map                      ; Uses the `M-s' prefix
    ("g" . consult-git-grep)
    ("G" . consult-grep)
    ("r" . consult-ripgrep)
    ("f" . consult-find)
    ("F" . consult-locate)
    :map comint-mode-map
    ([remap comint-history-isearch-backward-regexp]. consult-history)
    :map minibuffer-local-map
    ([remap next-matching-history-element] . consult-history)
    ([remap previous-matching-history-element]. consult-history))
  :config
  ;; Have line centered in previews. Make sure `recenter' is called after
  ;; `consult--maybe-recenter'
  (add-hook 'consult-after-jump-hook #'recenter)

  ;; Customize consult commands
  (consult-customize consult-buffer :group nil)
  
  ;; Additional `consult-buffer' sources (groups)
  (defvar kb/consult-buffer--dired-source
    (list :name     "Dired"
          :category 'buffer
          :narrow   ?d
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    'consult--buffer-state
          :action   'consult--buffer-action
          :items (lambda ()
                   (mapcar #'buffer-name
                           (seq-filter
                            (lambda (x)
                              (eq (buffer-local-value 'major-mode x) 'dired-mode))
                            (buffer-list))))))
  (add-to-list 'consult-buffer-sources #'kb/consult-buffer--dired-source 'append)

  (defvar kb/consult-buffer--info-source
    (list :name     "Info"
          :category 'buffer
          :narrow   ?i
          :face     'info-title-1
          :history  'buffer-name-history
          :state    'consult--buffer-state
          :action   'consult--buffer-action
          :items (lambda ()
                   (mapcar #'buffer-name
                           (seq-filter
                            (lambda (x)
                              (eq (buffer-local-value 'major-mode x) 'Info-mode))
                            (buffer-list))))))
  (add-to-list 'consult-buffer-sources #'kb/consult-buffer--info-source 'append)

  (defvar kb/consult-buffer--customize-source
    (list :name     "Customize"
          :category 'buffer
          :narrow   ?c
          :face     'custom-group-tag
          :history  'buffer-name-history
          :state    'consult--buffer-state
          :action   'consult--buffer-action
          :items (lambda ()
                   (mapcar #'buffer-name
                           (seq-filter
                            (lambda (x)
                              (eq (buffer-local-value 'major-mode x) 'Custom-mode))
                            (buffer-list))))))
  (add-to-list 'consult-buffer-sources #'kb/consult-buffer--customize-source 'append))

;;;; Embark
;; Allow an equivalent to ivy-actions to regular complete-read minibuffers (and
;; thus selectrum!)
(use-package embark
  :bind
  (("C-.". embark-act)
   ("C-h B". embark-bindings)
   :map vertico-map
   ("C-.". embark-act)
   ("C->". embark-become)
   :map embark-symbol-map
   ("R". raise-sexp))
  :custom
  ;; Embark Actions menu
  (prefix-help-command 'embark-prefix-help-command) ; Use completing read when typing ? after prefix key
  (embark-prompter 'embark-keymap-prompter) ; What interface do I want to use for Embark Actions?
  (embark-indicators                    ; How the Embark Actions menu appears
   '(embark-mixed-indicator
     embark-highlight-indicator
     ;; embark-isearch-highlight-indicator
     ;; embark-verbose-indicator
     ))
  (embark-mixed-indicator-delay 1.5)

  ;; Misc
  (embark-collect-live-initial-delay 0.8)
  (embark-collect-live-update-delay 0.5)
  :config
  (add-to-list 'embark-keymap-alist '(raise-sexp . embark-symbol-map)))

;;;; Embark-consult
;; Companion package for embark
(use-package embark-consult
  :demand
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  :hook (scratch-create-buffer . kb/scratch-buffer-setup)
  :bind
  ( :map krisb-open-keymap
    ("s". scratch))
  :preface
  (defun kb/scratch-buffer-setup ()
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

;;;; File or buffer utilities
;;;;; Autorevert
;; Automatically update buffers as files are externally modified
(use-package autorevert
  :ensure nil
  :hook (on-first-file . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t)
  (auto-revert-verbose t))

;;;;; Whitespace
;; Remove whitespace on save
(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style '(face empty indentation::space tab)))

;;;;; Sudo-edit
;; Utilities to edit files as root
(use-package sudo-edit
  :bind
  ( :map krisb-file-keymap
    ("U" . sudo-edit-find-file)
    ("u" . sudo-edit))
  :config
  (sudo-edit-indicator-mode))

;;;;; Hi-lock
(use-package hi-lock
  :hook (on-first-file . global-hi-lock-mode)
  :ensure nil
  :custom
  (hi-lock-file-patterns-policy
   '(lambda (_pattern) t)))

;;;;; Symbol-overlay
;; Mimics functionality of built-in hi-lock but with overlays instead of
;; font-lock. Usefully has `symbol-overlay-rename'. On highlighted regions, the
;; `symbol-overlay-map' is enabled
(use-package symbol-overlay
  :bind
  (([remap highlight-symbol-at-point] . symbol-overlay-put)
   ("M-s h M-n" . symbol-overlay-switch-forward)
   ("M-s h M-p" . symbol-overlay-switch-backward)
   ("M-s h <f7>" . symbol-overlay-mode)
   ("M-s h <f8>" . symbol-overlay-remove-all)))

;;;;; Re-builder
;; Interactively build regexps
(use-package re-builder
  :ensure nil
  :custom
  (reb-re-syntax 'read))

;;;; Modes
;;;;; Conf-mode
;; For Unix config files
(use-package conf-mode
  :ensure nil
  :mode ("\\.rs\\'" . conf-mode)
  :hook
  (conf-mode . outshine-mode))

;;;;; Vimrc-mode
;; For editing vim/nvim config files
(use-package vimrc-mode)

;;;; Aesthetics
;;;;; Prog-mode
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . goto-address-prog-mode)
         (window-setup . global-prettify-symbols-mode)
         (org-mode . (lambda () (setq-local prettify-symbols-compose-predicate 'kb/prettify-symbols-compose-p))))
  :init
  (defun kb/prettify-symbols-compose-p (start end _match)
    "Returns nil except on org-mode exceptions.
Returns nil if the character before and after MATCH isn't a punctuation,
with the exception of org-emphasis markers."
    (let ((before (char-to-string (or (char-before start) ? )))
          (after (char-to-string (or (char-after end) ? )))
          (org-emphasis-markers (mapcar #'car org-emphasis-alist)))
      (cond
       ((or (member before org-emphasis-markers)
            (member after org-emphasis-markers))
        t)
       ((or (string-match-p (rx (or (any punct))) before)
            (string-match-p (rx (or (any punct))) after))
        nil)
       (t t))))
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (add-to-list 'prettify-symbols-alist '("->" . ?➡))
              (add-to-list 'prettify-symbols-alist '("<-" . ?⬅))))
  (add-hook 'latex-mode-hook
            (lambda ()
              (add-to-list 'prettify-symbols-alist '("\\Dashv" . ?⫤))
              (add-to-list 'prettify-symbols-alist '("\\DashVDash" . ?⟚))
              (add-to-list 'prettify-symbols-alist '("\\dashVdash" . ?⊢))
              (delete '("--" . 8211 ) prettify-symbols-alist)
              (delete '("---" . 8212) prettify-symbols-alist)
              ;; For `lplfitch'. Slightly higher than `\vdots'. Using the
              ;; `\pline{\vdots}' results in the ellipses not being centered
              ;; on the line.
              (add-to-list 'prettify-symbols-alist '("\\ellipsesline" . ?⋮))
              ;; Circled numbers from the pifont package
              (add-to-list 'prettify-symbols-alist '("\\ding{192}" . ?①))
              (add-to-list 'prettify-symbols-alist '("\\ding{193}" . ?②))
              (add-to-list 'prettify-symbols-alist '("\\ding{194}" . ?③))
              (add-to-list 'prettify-symbols-alist '("\\ding{195}" . ?④))
              (add-to-list 'prettify-symbols-alist '("\\ding{196}" . ?⑤))
              (add-to-list 'prettify-symbols-alist '("\\ding{197}" . ?⑥))
              (add-to-list 'prettify-symbols-alist '("\\ding{198}" . ?⑦))
              (add-to-list 'prettify-symbols-alist '("\\ding{199}" . ?⑧))
              (add-to-list 'prettify-symbols-alist '("\\ding{200}" . ?⑨))
              (add-to-list 'prettify-symbols-alist '("\\ding{201}" . ?⑩))
              ;; Angle brackets for text (non-math)
              (add-to-list 'prettify-symbols-alist '("\\textlangle" . 10216))
              (add-to-list 'prettify-symbols-alist '("\\textrangle" . 10217))))
  (add-hook 'python-base-mode-hook
            (lambda ()
              (add-to-list 'prettify-symbols-alist '("->" . ?»))
              (add-to-list 'prettify-symbols-alist '("lambda" . ?λ)))))

;;;;; Hl-line
(use-package hl-line
  :ensure nil
  :hook
  ((prog-mode conf-mode) . hl-line-mode))

;;;;; Hl-todo
;; OPTIMIZE 2023-07-14: Also consider synergy with
;; https://codeberg.org/ideasman42/emacs-prog-face-refine
(use-package hl-todo
  :hook (on-first-buffer . global-hl-todo-mode)
  :bind
  ( :map hl-todo-mode-map
    ("M-s t n" . hl-todo-next)
    ("M-s t p" . hl-todo-previous)
    ("M-s t o" . hl-todo-occur))
  :custom
  (hl-todo-include-modes '(prog-mode conf-mode text-mode))
  (hl-todo-text-modes '(markdown-mode org-mode text-mode))
  (hl-todo-exclude-modes nil)
  ;; TODO 2022-02-07: Change `kb-comment' such that I am able to leverage
  ;; hl-todo's punctuation highlighting.
  (hl-todo-require-punctuation t)
  (hl-todo-highlight-punctuation ": ")
  (hl-todo-keyword-faces
   '(("TODO" . "orange")
     ("HACK" error bold)
     ("NOTE" . "cornflower blue")
     ("REVIEW" . "orchid")
     ("FIXME" error bold)
     ("OPTIMIZE" . "SandyBrown")))
  :config
  (with-eval-after-load 'alt-comment-dwim
    ;; Make sure to have all words in `alt-comment-dwim-keywords-coding' and
    ;; `alt-comment-dwim-keywords-writing' in this list, otherwise those words will not
    ;; appear in any calls to `alt-comment-dwim-dwim'.
    (setopt hl-todo-keyword-faces alt-comment-dwim-keyword-faces)))

;;;;; Ansi-color
;; Apply ANSI terminal color escape codes.
;; <http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html>
(use-package ansi-color
  :ensure nil
  :autoload endless/colorize-compilation
  :hook (compilation-filter . endless/colorize-compilation)
  :config
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

;;;;; Fancy-compilation
;; Better compilation buffers. Has support color output,progress updates on a
;; single line (as used by many build systems),scrolling behavior similar to
;; most terminals,and optionally use foreground & background independent of
;; theme colors.
(use-package fancy-compilation
  :after compile
  :demand
  :custom
  (fancy-compilation-override-colors nil)
  ;; Briefer text
  (fancy-compilation-quiet-prelude t)
  (fancy-compilation-quiet-prolog t)
  :config
  (fancy-compilation-mode 1))

;;;;; Indent-bars
;; Show indicator for indentation levels (like in VS Code)
(use-package indent-bars
  :disabled t                           ; FIXME 2023-08-18: Causes errors I think...
  ;; OPTIMIZE 2023-08-15: Have to add to `after-init-hook' because of issues
  ;; with daemon
  :hook ((kb/themes . indent-bars-reset)
         ((prog-mode conf-mode) . indent-bars-mode))
  :custom
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.25)
  (indent-bars-pad-frac 0.25)
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-display-on-blank-lines t))

;;;;; Rainbow-mode
;; Colorify color codes
(use-package rainbow-mode
  :diminish
  :hook
  ((prog-mode conf-mode help-mode) . rainbow-mode))

;;;;; Highlight-quoted
;; Make (lisp) quotes and quoted symbols easier to distinguish from free variables by highlighting
;; them
(use-package highlight-quoted
  :disabled t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

;;;;; Paren
;; Highlight matching delimiters
(use-package paren
  :ensure nil
  :hook (on-first-buffer . show-paren-mode)
  :custom
  (show-paren-context-when-offscreen 'overlay))

;;;;; Display-fill-column-indicator
(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-character ?│)
  :custom-face
  (fill-column-indicator ((t (:inherit line-number)))))

;;;;; Visual-wrap
;; Visually indent lines wrapped visually! This makes long-lines in lists
;; properly indented!
;; NOTE 2024-10-09: This package is the same as the more often referred to
;; `adaptive-wrap-prefix-mode'.
(use-package visual-wrap
  :ensure nil
  :demand t
  :config
  (global-visual-wrap-prefix-mode 1))

;;;; Other
;;;;; Lorem-ipsum
;; Sample text
(use-package lorem-ipsum
  :config
  (setq-default lorem-ipsum-list-bullet "- "))

(provide 'programming-general-rcp)
;;; programming-general-rcp.el ends here
