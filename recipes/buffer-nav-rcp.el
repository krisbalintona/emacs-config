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
  ("M-d" 'puni-forward-kill-word
   "M-DEL" 'puni-backward-kill-word
   "C-k" 'puni-kill-line
   "C-M-f" 'puni-forward-sexp
   "C-M-b" 'puni-backward-sexp
   "C-M-a" 'puni-beginning-of-sexp
   "C-M-e" 'puni-end-of-sexp
   "C-M-n" 'puni-forward-sexp-or-up-list
   "C-M-p" 'puni-backward-sexp-or-up-list
   ;; My additional keybinds
   "C-M-9" 'puni-syntactic-backward-punct
   "C-M-0" 'puni-syntactic-forward-punct
   "C-M-r" 'puni-raise
   "C-=" 'puni-expand-region)
  :custom
  (puni-confirm-when-delete-unbalanced-active-region t))

;;; Avy
;; Quickly jump to any character
(use-package avy
  :chords (("//" .  avy-goto-word-1)
           ("''" . avy-goto-char))
  :custom
  (avy-all-windows nil)                 ; Scope
  (avy-keys                             ; Used for letter combinations
   '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
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
     (?H . avy-action-helpful)
     (?D . avy-action-define)
     ))
  :init
  ;; Additional avy actions. Inspired or taken from
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
  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  ;; Dictionary
  (defun dictionary-search-dwim (&optional arg)
    "Search for definition of word at point. If region is active,
search for contents of region instead. If called with a prefix
argument, query for word to search."
    (interactive "P")
    (if arg
        (dictionary-search nil)
      (if (use-region-p)
          (dictionary-search (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
        (if (thing-at-point 'word)
            (dictionary-lookup-definition)
          (dictionary-search-dwim '(4))))))
  (defun avy-action-define (pt)
    (goto-char pt)
    (dictionary-search-dwim)
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  )

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

;;; Goto-line-preview
;; Preview line before you jump to it with `goto-line'
(use-package goto-line-preview
  :general ([remap goto-line] 'goto-line-preview)
  )

;;; buffer-nav-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffer-nav-rcp)
