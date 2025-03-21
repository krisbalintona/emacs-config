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
  (use-package-enable-imenu-support t)
  (imenu-flatten 'group))

;;;; Puni
;; Major-mode agnostic structural editing, faithful to built-ins
(use-package puni
  :commands kb/puni-global-mode
  :hook
  (on-first-input . kb/puni-global-mode)
  ;; :general
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
  :config
  (defvar kb/puni-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-d") 'puni-forward-kill-word)
      (define-key map (kbd "M-DEL") 'puni-backward-kill-word)
      (define-key map [remap kill-line] 'puni-kill-line)
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

  (define-minor-mode kb/puni-mode
    "Enable keybindings for Puni commands."
    :keymap kb/puni-mode-map)
  (define-globalized-minor-mode kb/puni-global-mode
    kb/puni-mode
    (lambda () (kb/puni-mode 1))))

;;;; Avy
;; Quickly jump to any character
(use-package avy
  :commands kb/avy-transient-menu
  :bind
  ("C-;" . kb/avy-transient-menu)
  :custom
  (avy-all-windows t)                   ; Scope
  (avy-case-fold-search nil)
  (avy-single-candidate-jump t)
  (avy-timeout-seconds 0.3)
  (avy-style 'at-full)
  (avy-keys '(?a ?w ?r ?u ?i ?o ?p))
  (avy-dispatch-alist ; Avy actions (first narrow so letter combinations appear)
   '((?k . avy-action-kill-stay)
     (?K . avy-action-kill-move)
     (?t . avy-action-teleport)
     (?m . avy-action-mark)
     (?y . avy-action-yank)
     (?z . avy-action-zap-to-char)
     (?. . kb/avy-action-embark)
     (?h . kb/avy-action-help)
     (?d . kb/avy-action-define)
     (?e . kb/avy-action-eval)))
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
  (defun kb/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Helpful
  (defun kb/avy-action-help (pt)
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
  (defun kb/avy-action-define (pt)
    (require 'checking-words-rcp)
    (save-excursion
      (goto-char pt)
      (kb/dictionary-at-point))
    ;; If with `universal-arg', don't switch to help buffer
    (when current-prefix-arg
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Evaluation
  (defun kb/avy-action-eval (pt)
    (save-excursion
      (goto-char pt)
      (if (fboundp 'eros-eval-last-sexp)
          (call-interactively 'eros-eval-last-sexp)
        (call-interactively 'eval-last-sexp)))
    t)

  ;; Faces
  (when (featurep 'modus-themes)
    ;; Don't bold so text isn't shifted much
    (set-face-attribute 'avy-lead-face nil :inherit 'modus-themes-reset-soft)
    (set-face-attribute 'avy-lead-face-0 nil :inherit 'modus-themes-reset-soft)
    (set-face-attribute 'avy-lead-face-1 nil :inherit 'modus-themes-reset-soft)
    (set-face-attribute 'avy-lead-face-2 nil :inherit 'modus-themes-reset-soft))

  ;; Transient map
  (transient-define-prefix kb/avy-transient-menu ()
    "Transient menu for several avy commands."
    [("l" "Line" avy-goto-line)
     ("c" "Character" avy-goto-char-timer)
     ("s" "Symbol" avy-goto-symbol-1)
     ("p" "Parens" kb/avy-goto-parens :if-mode emacs-lisp-mode)]))

;;;; Goto-last-change
(use-package goto-chg
  :bind
  (("C-M-s-(" . goto-last-change)
   ("C-M-s-)" . goto-last-change-reverse)))

(provide 'buffer-nav-rcp)
;;; buffer-nav-rcp.el ends here
