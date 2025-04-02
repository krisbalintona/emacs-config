;;; Puni
;; Major-mode agnostic structural editing, faithful to built-ins
(use-package puni
  :custom
  (puni-confirm-when-delete-unbalanced-active-region t)
  :config
  (puni-global-mode 1)

  ;; Replace the default mode map with my own version
  (defvar-keymap krisb-puni-mode-map
    :doc "Keymap used for `puni-mode'.")
  (bind-keys :map krisb-puni-mode-map
             ;; ("DEL" . puni-backward-delete-char)
             ;; ("C-d" . puni-forward-delete-char)
             ;; ("C-S-k" . puni-backward-kill-line)
             ;; ("C-c DEL" . puni-force-delete)
             ;; ("C-w" . puni-kill-region)
             ([remap kill-word] . puni-forward-kill-word)
             ([remap backward-kill-word] . puni-backward-kill-word)
             ([remap kill-line] . puni-kill-line)
             ([remap forward-sexp] . puni-forward-sexp)
             ([remap backward-sexp] . puni-backward-sexp)
             ([remap beginning-of-defun] . puni-beginning-of-sexp)
             ([remap end-of-defun] . puni-end-of-sexp)
             ([remap backward-list] . puni-backward-sexp-or-up-list)
             ([remap forward-list] . puni-forward-sexp-or-up-list)
             ("C-M-(" . puni-syntactic-backward-punct)
             ("C-M-)" . puni-syntactic-forward-punct)
             ("C-M-r" . puni-raise)
             ("C-M-=" . puni-splice)
             ("C-M-S-o" . puni-split)
             ("C-M-[" . puni-slurp-backward)
             ("C-M-]" . puni-slurp-forward)
             ("C-M-{" . puni-barf-backward)
             ("C-M-}" . puni-barf-forward))
  (setq puni-mode-map krisb-puni-mode-map))

;;; God-mode
(use-package god-mode
  ;; 2025-04-01: I find the need for a toggle key to defeat the purpose of the
  ;; package: you can move around quickly, but when it comes to editing, you
  ;; have to enter a "new mode"...  Why not just do modal editing?
  :disabled t
  :bind ("<escape>" . god-local-mode)
  :custom
  (god-mode-enable-function-key-translation nil)
  (god-exempt-major-modes nil)
  (god-exempt-predicates nil))

;;; Meow
(use-package meow
  ;; 2025-04-01: Best modal editing scheme, as far as I can tell. But I don't
  ;; want a modal editing package
  :disabled t
  :config
  (meow-global-mode 1)

  ;; Set up QWERTY
  (defun krisb-alternative-editing-schemes-setup ()
    "Set up QWERTY bindings with meow."
    (setopt meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
            meow--kbd-splice-sexp "C-M-="
            meow--kbd-forward-slurp "C-M-]"
            meow--kbd-backward-slurp "C-M-]"
            meow--kbd-forward-barf "C-M-}"
            meow--kbd-forward-barf "C-M-{"
            meow--kbd-split-sexp "C-M-S-o")
    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)
     ;; Bespoke bindings
     '(")" . puni-syntactic-forward-punct)
     '("(" . puni-syntactic-backward-punct)
     '("}" . meow-forward-slurp)
     '("{" . meow-backward-slurp)
     '("C-}" . meow-forward-barf)
     '("C-{" . meow-backward-barf)
     '("C-=" . meow-splice-sexp)
     '("C-o" . meow-split-sexp)))
  (krisb-alternative-editing-schemes-setup))

;;; Boon
(use-package boon
  :disabled t                ; 2025-04-02: Ergonomic but... what is the benefit?
  :config
  (require 'boon-qwerty)
  (boon-mode 1))

;;; Provide
(provide 'krisb-alternative-editing-schemes)
