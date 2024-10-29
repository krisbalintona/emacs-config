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
    ("m" . consult-mark)
    ("M" . consult-global-mark)
    ("I" . consult-imenu-multi)
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
  (consult-customize consult-buffer :group nil))

;;;; File or buffer utilities
;;;;; Whitespace
;; Remove whitespace on save
(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style '(face empty indentation::space tab)))

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

(provide 'programming-general-rcp)
;;; programming-general-rcp.el ends here
