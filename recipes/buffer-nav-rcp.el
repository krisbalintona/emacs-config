;;; buffer-nav-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Configuration of packages whose main functionality is to navigate buffers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Puni
;; Major-mode agnostic structural editing, faithful to built-ins
(use-package puni
  :general
  ;; See `puni-mode-map'
  ("M-d" 'puni-forward-kill-word
   "M-DEL" 'puni-backward-kill-word
   "C-k" 'kb/puni-smart-kill-line
   "C-M-f" 'puni-forward-sexp
   "C-M-b" 'puni-backward-sexp
   "C-M-a" 'puni-beginning-of-sexp
   "C-M-e" 'puni-end-of-sexp
   "C-M-n" 'puni-forward-sexp-or-up-list
   "C-M-p" 'puni-backward-sexp-or-up-list
   ;; My additional keybinds or rebindings
   "C-M-9" 'puni-syntactic-backward-punct
   "C-M-0" 'puni-syntactic-forward-punct
   "C-M-r" 'puni-raise
   "C-M-S-m" 'puni-split
   "C-M-m" 'puni-splice
   "C-M-]" 'puni-slurp-forward
   "C-M-}" 'puni-barf-forward
   "C-M-[" 'puni-slurp-backward
   "C-M-{" 'puni-barf-backward
   "C-=" 'puni-expand-region
   [remap forward-word] 'toki-forward-word
   [remap backward-word] 'toki-backward-word)
  ;; From `toki-editing'
  (:keymaps 'text-mode-map
   [remap puni-forward-kill-word] 'toki-forward-delete-word
   [remap puni-backward-kill-word] 'toki-backward-delete-word)
  :custom
  (puni-confirm-when-delete-unbalanced-active-region t)
  :config
  ;; Taken from https://github.com/AmaiKinono/puni/wiki/Useful-commands. Also
  ;; made to retain the default prefix argument behavior
  (defun kb/puni-smart-kill-line (&optional n)
    "Kill a line forward while keeping expressions balanced.
If nothing can be deleted, kill backward. If still nothing can be
deleted, kill the pairs around point."
    (interactive "P")
    (let ((bounds (puni-bounds-of-list-around-point)))
      (if (eq (car bounds) (cdr bounds))
          (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
            (puni-delete-region (car sexp-bounds) (cdr sexp-bounds) 'kill))
        (if (eq (point) (cdr bounds))
            (puni-backward-kill-line n)
          (puni-kill-line n)))))

  ;; The following are movement and editing related commands I found interesting
  ;; from Toki's `toki-editing' module from his personal config:
  ;; /home/krisbalintona/emacs-repos/toki-emacs/site-lisp/toki-editing.el. I've
  ;; also added prefix argument support for some many of theses commands

  ;; Errors (ancillary)
  (defun toki/bob-error ()
    "Signal an error if point is at the beginning of buffer."
    (when (bobp)
      (signal 'beginning-of-buffer nil)))

  (defun toki/eob-error ()
    "Signal an error if point is and the end of buffer."
    (when (eobp)
      (signal 'end-of-buffer nil)))

  ;; Syntax (not commands)
  (defun toki/forward-block ()
    "Go forward a block.
Return the point if success.

A block is a continuous region with the same syntax, which
contains no more than 1 word.  See the implementation for
details."
    (unless (eobp)
      ;; A word may actually end at a position where the syntax on both sides are
      ;; "word", e.g., when subword-mode is enabled.
      (let ((word-end (save-excursion (when (forward-word) (point)))))
        (puni--forward-same-syntax word-end))))

  (defun toki/backward-block ()
    "Backward version of `toki/forward-block'."
    (unless (bobp)
      (let ((word-beg (save-excursion (when (forward-word -1) (point)))))
        (puni--backward-same-syntax word-beg))))

  ;; Word movement and deletion
  (defun toki-forward-word ()
    "A finer version of `forward-word'.
If there's *only one* non-word char between point and next word,
move after it.  Then jump forward by a block.  A block is a
continuous region with the same syntax, like a word, a bunch of
whitespaces/punctuations, etc.

This doesn't fly over most punctuations, while `forward-word'
does."
    (interactive)
    (toki/eob-error)
    (when (eq (puni--syntax-char-after (1+ (point))) ?w)
      (forward-char))
    (toki/forward-block))

  (defun toki-forward-delete-word ()
    "Delete word forward while keeping expressions balanced."
    (interactive)
    (if (use-region-p)
        (puni-delete-active-region)
      (puni-soft-delete-by-move #'toki-forward-word nil nil nil 'jump-and-reverse-delete)))

  (defun toki-backward-word ()
    "Backward version of `toki-forward-word'."
    (interactive)
    (toki/bob-error)
    (when (eq (puni--syntax-char-after (- (point) 2)) ?w)
      (backward-char))
    (toki/backward-block))

  (defun toki-backward-delete-word ()
    "Delete word backward while keeping expressions balanced."
    (interactive)
    (if (use-region-p)
        (puni-delete-active-region)
      (puni-soft-delete-by-move #'toki-backward-word nil nil nil 'jump-and-reverse-delete))))

;;; Avy
;; Quickly jump to any character
(use-package avy
  :commands kb/avy-goto-parens
  :general
  ;; Also consider `avy-goto-char-timer'
  ("C-;" 'avy-goto-word-or-subword-1
   "C-:" 'avy-goto-char-2)
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   "C-:" 'kb/avy-goto-parens)
  :custom
  (avy-all-windows nil)                 ; Scope
  (avy-case-fold-search nil)
  (avy-single-candidate-jump nil)
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
    (goto-char pt)
    (kb/dictionary-at-point)
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
;;; Link-hint
;; Open links quickly
(use-package link-hint
  :general (:prefix "C-c l"
            "o" 'link-hint-open-link
            "c" 'link-hint-copy-link))

;;; Imenu
(use-package imenu
  :custom
  (org-imenu-depth 7)                   ; Show more than just 2 levels...
  (imenu-auto-rescan t)
  )

;;; Imenu-list
;; Side buffer with imenu items
(use-package imenu-list
  :after imenu
  :general (kb/nav-keys
             "I" '(imenu-list :wk "Imenu list"))
  :hook (imenu-list-major-mode . visual-line-mode)
  )

;;; Occur
;; Narrow current buffer to lines which match a regexp
(use-package occur
  :straight nil
  :gfhook 'visual-line-mode
  :general (kb/nav-keys
             "o" '(occur :wk "Occur"))
  )

;;; buffer-nav-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffer-nav-rcp)
