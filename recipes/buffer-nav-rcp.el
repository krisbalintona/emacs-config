;;; buffer-nav-rcp.el --- Navigating efficiently within buffers  -*- lexical-binding: t; -*-

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

;; Configuration of packages whose main functionality is to navigate buffers.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Isearch
;; Incremental search
(use-package isearch
  :ensure nil
  :custom
  (isearch-repeat-on-direction-change t)
  (isearch-allow-scroll t)
  (isearch-allow-motion t)
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no)
  ;; Make regular Isearch interpret the empty space as a regular expression that
  ;; matches any character between the words you give it. Learned from
  ;; Protesilaos. Also be aware of `isearch-toggle-lax-whitespace'
  (isearch-lax-whitespace t)
  (search-whitespace-regexp ".*?"))

;;;; Imenu
(use-package imenu
  :ensure nil
  :custom
  (org-imenu-depth 7)                   ; Show more than just 2 levels...
  (imenu-auto-rescan t)
  (use-package-enable-imenu-support t))

;;;; Imenu-list
;; Side buffer with imenu items
(use-package imenu-list
  :after imenu
  :general (kb/nav-keys
             "I" '(imenu-list :wk "Imenu list"))
  :hook (imenu-list-major-mode . visual-line-mode))

;;;; Occur
;; Narrow current buffer to lines which match a regexp
(use-package occur
  :ensure nil
  :gfhook 'visual-line-mode
  :general (kb/nav-keys
             "o" '(occur :wk "Occur")))

(provide 'buffer-nav-rcp)
;;;; Puni
;; Major-mode agnostic structural editing, faithful to built-ins
(use-package puni
  :defines 'kb/puni-global-mode
  :general
  ;; See `puni-mode-map'
  ;; (;; [remap transpose-sexps] 'puni-transpose
  ;;  [remap forward-word] 'toki-forward-word
  ;;  [remap backward-word] 'toki-backward-word)
  ;; From `toki-editing'
  ;; (:keymaps 'text-mode-map
  ;;           [remap puni-forward-kill-word] 'toki-forward-delete-word
  ;;           [remap puni-backward-kill-word] 'toki-backward-delete-word)
  :custom
  (puni-confirm-when-delete-unbalanced-active-region t)
  :init
  (defvar kb/puni-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-d") 'puni-forward-kill-word)
      (define-key map (kbd "M-DEL") 'puni-backward-kill-word)
      (define-key map [remap kill-line] 'kb/puni-kill-line)
      (define-key map [remap backward-sexp] 'puni-backward-sexp)
      (define-key map [remap forward-sexp] 'puni-forward-sexp)
      (define-key map [remap beginning-of-defun] 'puni-beginning-of-sexp)
      (define-key map [remap end-of-defun] 'puni-end-of-sexp)
      (define-key map [remap backward-list] 'puni-backward-sexp-or-up-list)
      (define-key map [remap forward-list] 'puni-forward-sexp-or-up-list)
      (define-key map (kbd "C-M-9") 'puni-syntactic-backward-punct)
      (define-key map (kbd "C-M-0") 'puni-syntactic-forward-punct)
      (define-key map (kbd "C-M-r") 'puni-raise)
      (define-key map (kbd "C-M-=") 'puni-splice)
      (define-key map (kbd "C-M-S-o") 'puni-split)
      (define-key map (kbd "C-M-[") 'puni-slurp-backward)
      (define-key map (kbd "C-M-]") 'puni-slurp-forward)
      (define-key map (kbd "C-M-{") 'puni-barf-backward)
      (define-key map (kbd "C-M-}") 'puni-barf-forward)
      map)
    "My own Puni keymap.")
  (defun kb/puni-kill-line (arg)
    "A wrapper around `puni-kill-line'.
Call `org-kill-line' instead when in org-mode. Passes ARG to
command."
    (interactive "P")
    (if (derived-mode-p 'org-mode)
        (org-kill-line arg)
      (puni-kill-line arg)))

  (define-minor-mode kb/puni-mode
    "Enable keybindings for Puni commands."
    :keymap kb/puni-mode-map)
  (define-globalized-minor-mode kb/puni-global-mode
    kb/puni-mode
    (lambda () (kb/puni-mode 1)))
  (kb/puni-global-mode))

;;;; Avy
;; Quickly jump to any character
(use-package avy
  :commands kb/avy-goto-parens
  :general
  ("C-;" 'avy-goto-char-timer)
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
            "C-:" 'kb/avy-goto-parens)
  :custom
  (avy-all-windows nil)                 ; Scope
  (avy-case-fold-search nil)
  (avy-single-candidate-jump t)
  (avy-timeout-seconds 0.3)
  (avy-style 'at-full)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (avy-dispatch-alist ; Avy actions (first narrow so letter combinations appear)
   '((?x . avy-action-kill-move)
     (?X . avy-action-kill-stay)
     (?t . avy-action-teleport)
     (?m . avy-action-mark)
     (?n . avy-action-copy)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-line)
     (?i . avy-action-ispell)
     (?z . avy-action-zap-to-char)
     ;; New, custom actions
     (?. . avy-action-embark)
     (? . avy-action-help)
     (? . avy-action-define)))
  (avy-orders-alist
   '((avy-goto-char . kb/avy-order-farthest)
     (avy-goto-char-2 . kb/avy-order-farthest)
     (avy-goto-word-0 . kb/avy-order-farthest)
     (avy-goto-word-1 . kb/avy-order-farthest)
     (avy-goto-char-timer . kb/avy-order-farthest)
     (kb/avy-goto-parens . kb/avy-order-farthest)))
  :config
  (defun kb/avy-order-farthest (x)
    (- (abs (- (if (numberp (car x))
                   (car x)
                 (caar x))
               (point)))))

  ;; Taken from the avy wiki
  (defun kb/avy-goto-parens ()
    "Go to an open or close parens."
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump (rx (+ (or (literal "(") (literal ")")))))))

  ;; Additional avy dispatch actions. Most are inspired or taken from
  ;; https://karthinks.com/software/avy-can-do-anything/

  ;; Embark
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Helpful
  (defun avy-action-help (pt)
    (save-excursion
      (goto-char pt)
      (if (featurep 'helpful)
          (helpful-at-point)
        (describe-symbol (symbol-at-point))))
    (when (featurep 'helpful)
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Dictionary
  (defun avy-action-define (pt)
    (require 'checking-words-rcp)
    (save-excursion
      (goto-char pt)
      (kb/dictionary-at-point))
    ;; If with `universal-arg', don't switch to help buffer
    (when current-prefix-arg
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Faces
  (when (featurep 'modus-themes)
    ;; Don't bold so text isn't shifted much
    (set-face-attribute 'avy-lead-face nil :inherit 'modus-themes-reset-soft)
    (set-face-attribute 'avy-lead-face-0 nil :inherit 'modus-themes-reset-soft)
    (set-face-attribute 'avy-lead-face-1 nil :inherit 'modus-themes-reset-soft)
    (set-face-attribute 'avy-lead-face-2 nil :inherit 'modus-themes-reset-soft)))

;;;; Goto-last-change
(use-package goto-chg
  :general ("H-(" 'goto-last-change
            "H-)" 'goto-last-change-reverse))

;;; buffer-nav-rcp.el ends here
