;;; template-rcp.el --- Templating in Emacs          -*- lexical-binding: t; -*-

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

;; Packages related to template expansion.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Yasnippet
;; Template-expansion system (doesn't include templates)
(use-package yasnippet
  :demand
  :diminish yas-minor-mode
  :custom
  (yas-alias-to-yas/prefix-p nil)
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines nil)
  (yas-inhibit-overlay-modification-protection nil)
  (yas-snippet-revival t)
  (yas-triggers-in-field nil)
  (yas-choose-tables-first nil)         ; Fewer things to select!
  :config
  (yas-global-mode 1))

;;;; Consult-yasnippet
(use-package consult-yasnippet
  :after yasnippet
  :general
  ([remap yas-insert-snippet] 'consult-yasnippet
   [remap yas-visit-snippet-file] 'consult-yasnippet-visit-snippet-file))

;;;; Tempel
;; Small and simple snippet/template system compatible with corfu.
(use-package tempel
  :disabled                             ; Migrate to yasnippet
  :general
  ("M-+" 'tempel-complete               ; List all available templates
   "M-*" 'tempel-insert)                ; Insert typed template
  (:keymaps 'tempel-map
            "C-M-c" 'tempel-done)
  :custom
  (tempel-file (no-littering-expand-var-file-name "tempel-templates")))

(provide 'template-rcp)
;;; template-rcp.el ends here
