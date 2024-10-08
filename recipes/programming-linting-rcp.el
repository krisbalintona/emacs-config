;;; programming-linting-rcp.el --- Linting           -*- lexical-binding: t; -*-

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

;; Everything to do with checking syntax and foreseeing errors.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'personal-variables-rcp)

;;;; Flymake
(use-package flymake
  :diminish
  :hook
  ((prog-mode org-mode) . flymake-mode)
  :bind
  ( :map flymake-mode-map
    ("M-n" . flymake-goto-next-error)
    ("M-p". flymake-goto-prev-error))
  :custom
  (elisp-flymake-byte-compile-load-path (append '("./") load-path)) ; Recognizes files I know about
  (flymake-wrap-around nil)
  (flymake-fringe-indicator-position nil) ; Disable fringe indicators
  (flymake-show-diagnostics-at-end-of-line nil) ; I enable this selectively via a hook
  (flymake-mode-line-format
   '(flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format     ; Remove surrounding brackets
   ;; NOTE 2024-02-12: Need to have first and last elements be strings!
   ;; Otherwise a counter might be hidden
   '(""
     flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter
     ""))
  (flymake-suppress-zero-counters t))

;;;; Flymake-collection
(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup)
  :custom
  (flymake-collection-hook-inherit-config t)
  (flymake-collection-hook-ignore-modes nil)
  :init
  ;; NOTE 2024-10-05: Set `flymake-collection-config' immediately when the
  ;; package loads, so the first invocation of `flymake-collection-hook-setup'
  ;; uses my configured value.
  ;; NOTE 2024-10-05: I configure vale to use proselint to my liking, so I
  ;; disable the proselint checker. One reason that motivates this decision is
  ;; vale's performance compared to proselint (see
  ;; https://github.com/errata-ai/vale?tab=readme-ov-file#benchmarks).
  (setf (alist-get 'org-mode flymake-collection-config)
        '((flymake-collection-vale
           :depth -20)
          (flymake-collection-proselint
           :depth -1
           :disabled t))
        (alist-get 'markdown-mode flymake-collection-config)
        '((flymake-collection-markdownlint
           :depth -50)
          (flymake-collection-vale
           :depth -20)
          (flymake-collection-proselint
           :disabled t
           :depth -1))
        (alist-get 'notmuch-message-mode flymake-collection-config)
        '((flymake-collection-vale
           :depth -20)
          (flymake-collection-proselint
           :depth -1
           :disabled t)))
  :config
  (require 'flymake-collection-define)
  ;; Customize the vale checker to my liking
  (flymake-collection-define-enumerate
    flymake-collection-vale
    "My version of `flymake-collection''s vale checker."
    :title "vale"
    :pre-let ((vale-exec (executable-find "vale")))
    :pre-check (unless vale-exec
                 (error "Cannot find vale executable"))
    :write-type 'pipe
    :command `(,vale-exec
               ,@(let* ((file-name (buffer-file-name flymake-collection-source))
                        (extension
                         ;; Sometimes we specify the extension based on the
                         ;; major mode because we editing buffers not visiting a
                         ;; file
                         (cond
                          ((equal major-mode 'org-mode) "org")
                          ((derived-mode-p 'markdown-mode) "md")
                          ((derived-mode-p 'message-mode)
                           ;; I define a custom ".email" extension in my
                           ;; .vale.ini so that the rules I want are run during
                           ;; email composition
                           "email")
                          (file-name
                           (file-name-extension file-name)))))
                   (when extension
                     (list (concat "--ext=." extension))))
               "--output=JSON")
    :generator
    (cdaar
     (flymake-collection-parse-json
      (buffer-substring-no-properties
       (point-min) (point-max))))
    :enumerate-parser
    (let-alist it
      `(,flymake-collection-source
        ,@(with-current-buffer flymake-collection-source
            (save-excursion
              (goto-char (point-min))
              (unless (and (eq .Line 1)
                           (not (bolp)))
                (forward-line (1- .Line)))
              (list (+ (point) (1- (car .Span)))
                    (+ (point) (cadr .Span)))))
        ,(pcase .Severity
           ("suggestion" :note)
           ("warning" :warning)
           ((or "error" _) :error))
        ,(concat (propertize (concat "[" .Check "]") 'face 'flymake-collection-diag-id) " "
                 .Message)))))

;;;; Flymake-flycheck
;; For extending flycheck checkers into flymake. This allows flymake to use
;; flycheck backends; check readme on how to do so. I use this when only
;; flycheck is available (e.g. `lsp-mode'), otherwise I try to rely on
;; `flymake-collection'.
(use-package flymake-flycheck
  ;; As the readme warns, "Flycheck UI packages will have no idea of what the
  ;; checkers are doing, because they are run without flycheck's coordination."
  :hook (flycheck-mode . kb/enable-flymake-flycheck)
  :init
  (defun kb/enable-flymake-flycheck ()
    (when flycheck-mode (flycheck-mode -1))
    (flymake-mode 1)
    ;; Existing flymake backends take precedence over the flycheck ones here.
    ;; Reverse order of the append if this isn't desired
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        (flymake-flycheck-all-chained-diagnostic-functions))))
  :config
  ;; Disable flycheck checkers for which we have flymake equivalents
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)))))

;;;; Package-lint-flymake
(use-package package-lint-flymake
  :commands package-lint-flymake
  :hook (emacs-lisp-mode . kb/package-lint-flymake-setup)
  :config
  (defun kb/package-lint-flymake-setup ()
    (unless (string-match-p (expand-file-name "recipes/" user-emacs-directory) default-directory)
      (add-hook 'flymake-diagnostic-functions #'package-lint-flymake nil t))))

(provide 'programming-linting-rcp)
;;; programming-linting-rcp.el ends here
