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
  (puni-global-mode)
  :config
  (defvar kb/puni-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-d") 'puni-forward-kill-word)
      (define-key map (kbd "M-DEL") 'puni-backward-kill-word)
      (define-key map (kbd "C-k") 'kb/puni-smart-kill-line)
      (define-key map (kbd "C-M-f") 'puni-forward-sexp)
      (define-key map (kbd "C-M-b") 'puni-backward-sexp)
      (define-key map (kbd "C-M-a") 'puni-beginning-of-sexp)
      (define-key map (kbd "C-M-e") 'puni-end-of-sexp)
      (define-key map (kbd "C-M-e") 'puni-end-of-sexp)
      (define-key map (kbd "C-M-n") 'puni-forward-sexp-or-up-list)
      (define-key map (kbd "C-M-p") 'puni-backward-sexp-or-up-list)
      (define-key map (kbd "C-M-9") 'puni-syntactic-backward-punct)
      (define-key map (kbd "C-M-0") 'puni-syntactic-forward-punct)
      (define-key map (kbd "C-M-r") 'puni-raise)
      (define-key map (kbd "C-M-m") 'puni-splice)
      (define-key map (kbd "C-M-S-m") 'puni-split)
      (define-key map (kbd "C-M-[") 'puni-slurp-backward)
      (define-key map (kbd "C-M-]") 'puni-slurp-forward)
      (define-key map (kbd "C-M-{") 'puni-barf-backward)
      (define-key map (kbd "C-M-}") 'puni-barf-forward)
      map)
    "My own Puni keymap.")
  (define-minor-mode puni-mode
    "Enable keybindings for Puni commands."
    :keymap kb/puni-mode-map)

  ;; Taken from https://github.com/AmaiKinono/puni/wiki/Useful-commands. Also
  ;; made to retain the typical prefix argument behavior of built-in
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

;;; Imenu
(use-package imenu
  :elpaca nil
  :custom
  (org-imenu-depth 7)                   ; Show more than just 2 levels...
  (imenu-auto-rescan t)
  (use-package-enable-imenu-support t))

;;; Imenu-list
;; Side buffer with imenu items
(use-package imenu-list
  :after imenu
  :general (kb/nav-keys
             "I" '(imenu-list :wk "Imenu list"))
  :hook (imenu-list-major-mode . visual-line-mode))

;;; Occur
;; Narrow current buffer to lines which match a regexp
(use-package occur
  :elpaca nil
  :gfhook 'visual-line-mode
  :general (kb/nav-keys
             "o" '(occur :wk "Occur")))

;;; buffer-nav-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffer-nav-rcp)
