;;; keybinds-general-rcp.el --- Keybind infrastructure  -*- lexical-binding: t; -*-

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

;; General.el setup and very broad keybindings.

;;; Code:
(require 'use-package-rcp)

;;; General itself
;; Leader key capabilities and more convenient key definitions and bindings.
(use-package general
  :demand
  :config
  (general-auto-unbind-keys))       ; Overwrite keybinds without returning error
(elpaca-wait)

;;; Leader keys
(general-create-definer kb/note-keys    ; For all note-taking needs
  :prefix "C-c n")
(general-create-definer kb/lsp-keys     ; For all lsp-related commands
  :keymaps 'lsp-mode-map
  :prefix "C-c l")
(general-create-definer kb/file-keys    ; File-related
  :prefix "C-c f")
(general-create-definer kb/yank-keys    ; Yanking
  :prefix "C-c i")
(general-create-definer kb/open-keys    ; Open certain things
  :prefix "C-c o")
;; Toggles. Use Hyper key if on Linux, and C-M- if in WSL2 or Windows natively
(if (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
    (general-create-definer kb/toggle-keys :prefix "C-M-S-t")
  (general-create-definer kb/toggle-keys :prefix "H-t"))

;;; Key-chords
(use-package use-package-chords
  :demand
  :init (key-chord-mode))

;;; Which-key
;; Show keybind tooltips
(use-package which-key
  :custom
  ;; These variables should be set before which-key-mode is activated
  (which-key-idle-delay 1.6)
  (which-key-idle-secondary-delay 1) ; Delay after which-key has already been shown
  (which-key-show-early-on-C-h t)    ; Show which-key help immediately
  (which-key-add-column-padding 0)
  (which-key-max-display-columns nil)
  ;; (which-key-show-transient-maps t) ; Necessary so show embark keybinds with which-key
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'right)
  (which-key-side-window-max-width 0.35)
  (which-key-lighter "")
  :init
  (which-key-mode)

  ;; Don't display C-u, digit, and other numeric keypad bindings
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
        which-key-replacement-alist))
;; (elpaca-wait)

;;; Meow
;; Trying out an alternative modal editing package
(use-package meow
  :disabled t                       ; I think I'm sticking with vanilla bindings
  :hook (after-init . meow-global-mode)
  :config
  (defun meow-setup ()
    ;; (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
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
     '("<escape>" . ignore)))
  (meow-setup))

(provide 'keybinds-general-rcp)
;;; keybinds-general-rcp.el ends here
