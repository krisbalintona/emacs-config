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

(provide 'programming-linting-rcp)
;;; programming-linting-rcp.el ends here
