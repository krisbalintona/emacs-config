;;; programming-elisp-rcp.el --- Emacs-lisp          -*- lexical-binding: t; -*-

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

;; These are packages that are helpful for programming or working in elisp.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'buffers-and-windows-rcp)

;;;; Rainbow-delimiters
;; Highlight matching delimiters (e.g. parenthesis)
(use-package rainbow-delimiters
  :disabled          ; I think with my experience, I now favor `paren-face-mode'
  :hook
  ((emacs-lisp-mode lisp-interaction-mode inferior-emacs-lisp-mode) . rainbow-delimiters-mode))

;;;; Help-find
;; Provides `help-find-function' and `help-find-keybinding'
(use-package help-find
  :bind
  ( :map help-map
    ("M-f f" . help-find-function)
    ("M-f r" . help-find-keybinding)))

;;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
  :disabled                          ; Trying out built-in `help' functionality
  :hook
  (helpful-mode . visual-line-mode)
  :bind
  (([remap describe-function] . helpful-function)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-key] . helpful-key)
   ([remap apropos-command] . helpful-command))
  :chords
  ( :map helpful-mode-map
    ("jj" . helpful-at-point)))

;;;; Suggest
;; Query `suggest' for elisp coding suggestions!
(use-package suggest
  :bind
  ( :map krisb-open-keymap
    ("S" . suggest))
  :custom
  (suggest-insert-example-on-start nil))

(provide 'programming-elisp-rcp)
;;; programming-elisp-rcp.el ends here
