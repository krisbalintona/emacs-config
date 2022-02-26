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

;;; Avy
;; Quickly jump to any character
(use-package avy
  :general (:states '(normal visual)
                    "'" 'avy-goto-word-1
                    "\"" 'avy-goto-char)
  :custom
  (avy-all-windows nil)                 ; Scope
  (avy-keys                             ; Used for letter combinations
   '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   )
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

;;; Ace-link
;; Open links easily
(use-package ace-link
  :general (:keymaps '(Info-mode-map helpful-mode-map help-mode-map woman-mode-map eww-mode-map compilation-mode-map mu4e-view-mode-map custom-mode-map org-mode-map)
                     "M-'" 'ace-link :wk "Ace-link")
  )

;;; Better-jumper
;; Accompanies `evil-jumper' very well. Some of the smart stuff is taken from
;; https://www.reddit.com/r/emacs/comments/ntnhkc/how_i_jump_around_emacs_with_betterjumper/
(use-package better-jumper
  :after (evil consult)
  :general ("H-i" 'better-jumper-jump-backward
            "H-o" 'better-jumper-jump-forward
            "H-p" 'better-jumper-jump-toggle)
  :custom
  ;; This is THE key to avoiding conflict with evils' jumping functionality
  (better-jumper-use-evil-jump-advice nil)
  ;; (better-jumper-use-evil-jump-advice t) ; Add evil-jump jumps

  (better-jumper-max-length 200)
  (better-jumper-add-jump-behavior 'append)

  (better-jumper-context 'window)
  (better-jumper-use-savehist t)
  (better-jumper-buffer-savehist-size 50)
  :init
  ;; Toggle between two between current point and last better-jumper set point
  ;; (inspired by `evil-jump-backward-swap').
  (evil-define-motion better-jumper-jump-toggle (count)
    (better-jumper-jump-backward 1)
    (better-jumper-set-jump (point))
    )
  :init
  (better-jumper-mode)
  :config
  ;; Set a jump point using `better-jumper-set-jump'
  (general-advice-add '(evil-first-non-blank evil-end-of-visual-line
                                             evil-org-beginning-of-line evil-org-end-of-line
                                             org-beginning-of-line org-end-of-line
                                             evil-goto-first-line evil-goto-line evil-goto-mark evil-goto-definition
                                             evil-search-next evil-search-previous
                                             evilmi-jump-items
                                             back-to-indentation end-of-visual-line
                                             consult-line consult-outline consult-ripgrep consult-imenu
                                             evil-search-previous evil-search-next
                                             lispyville-previous-opening lispyville-next-closing
                                             lispyville-next-opening lispyville-previous-closing
                                             lispyville-backward-sexp lispyville-forward-sexp
                                             lispyville-backward-up-list lispyville-up-list
                                             )
                      :before 'better-jumper-set-jump)

  ;; Specifically for ace-jump
  (general-add-hook '(ace-jump-mode-before-jump-hook ace-jump-mode-end-hook) 'better-jumper-set-jump)
  )

;;; Smartparens
;; Auto pairing parentheses
(use-package smartparens
  :gfhook 'show-smartparens-mode        ; Subtly highlight matching parentheses
  :general (:keymaps 'prog-mode-map
                     :states '(visual normal motion)
                     [remap evil-forward-sentence-begin] 'sp-forward-sexp
                     [remap evil-backward-sentence-begin] 'sp-backward-sexp)
  :custom
  (sp-show-pair-from-inside t)
  (sp-ignore-modes-list
   '(minibuffer-mode minibuffer-inactive-mode
                     ))
  (sp-autoskip-closing-pair 'always-end)
  ;; Read `sp-delete-pair' docstring
  (sp-autodelete-pair t)
  (sp-autodelete-opening-pair t)
  (sp-autodelete-closing-pair t)
  :init
  (smartparens-global-mode)
  :config
  ;; For python
  (require 'smartparens-python)

  ;; Helper functions
  (defun kb/sp-point-before-letter-digit-p (_id action _context)
    "Return t if point is followed by any digit or alphanumeric character, nil
otherwise. This predicate is only tested on \"insert\" action."
    (when (eq action 'insert)
      (sp--looking-at-p "[a-z0-9A-Z]")))
  (defun kb/sp-point-adjacent-paren-p (_id action _context)
    "Return t if point is next to a parenthesis, nil otherwise. This predicate
is only tested on \"insert\" action."
    (when (eq action 'insert)
      (sp--looking-at-p "(\\|)")))
  (defun kb/sp-point-before-closing-paren-p (_id action _context)
    "Return t if point is before a closing parenthesis, nil otherwise. This predicate
is only tested on \"insert\" action."
    (when (eq action 'insert)
      (sp--looking-at-p "(")))

  ;; Global
  (sp-pair "(" ")" :actions '(insert autoskip navigate))
  (sp-pair "\"" "\""
           :actions '(insert autoskip navigate escape)
           :unless '(sp-in-string-quotes-p kb/sp-point-before-letter-digit-p sp-point-before-same-p)
           :post-handlers '(sp-escape-wrapped-region sp-escape-quotes-after-insert))

  ;; Emacs-lisp-mode
  (sp-with-modes 'emacs-lisp-mode
    (sp-local-pair "<" ">" :actions '(insert autoskip navigate))
    (sp-local-pair "(" ")"
                   :actions '(insert autoskip navigate)
                   :unless '(kb/sp-point-before-letter-digit-p kb/sp-point-before-closing-paren-p))
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "'" "'"
                   :actions '(insert autoskip navgiate)
                   :when '(sp-in-string-p))
    (sp-local-pair "`" "'"
                   :actions '(insert autoskip navigate)
                   :when '(sp-in-comment-p sp-in-string-p))
    )

  ;; Sh-mode
  (sp-local-pair 'sh-mode "`" "`"
                 :unless '(sp-point-before-word-p sp-point-before-same-p))
  )

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
