;;; programming-ide-base-rcp.el --- Emacs makes a good IDE  -*- lexical-binding: t; -*-

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

;; IDE features in Emacs.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Devdocs
(use-package devdocs
  :bind
  ( :map prog-mode-map
    ("C-c D" . devdocs-lookup))
  :hook ((python-base-mode
          . (lambda () (setq-local devdocs-current-docs '("python~3.9"))))
         (c-ts-base-mode
          . (lambda () (setq-local devdocs-current-docs '("c"))))
         (lua-mode
          . (lambda () (setq-local devdocs-current-docs '("lua~5.3"))))
         (latex-mode
          . (lambda () (setq-local devdocs-current-docs '("latex")))))
  :custom
  (devdocs-window-select t))

;;;; Dash-docs
;; Viewing of documentation via browser.
(use-package dash-docs
  :hook ((python-base-mode . (lambda () (setq-local dash-docs-common-docsets '("Python 3"))))
         (haskell-mode . (lambda () (setq-local dash-docs-common-docsets '("Haskell"))))
         (js2-mode . (lambda () (setq-local dash-docs-common-docsets '("JavaScript"))))
         (lua-mode . (lambda () (setq-local dash-docs-common-docsets '("Lua"))))
         (LaTeX-mode . (lambda () (setq-local dash-docs-common-docsets '("LaTeX")))))
  :custom
  (dash-docs-docsets-path (expand-file-name "dash-docs-docsets" no-littering-var-directory))
  (dash-docs-browser-func 'eww)

  (dash-docs-enable-debugging nil) ; Get rid of annoying messages when searching
  (dash-docs-min-length 2)
  (dash-enable-fontlock t))

;;;;; Dash-docs-completing-read
;; My own interface for accessing docsets via `completing-read'.
(use-package dash-docs-completing-read
  :disabled                   ; REVIEW 2024-10-17: Check what I dod this with...
  :ensure nil
  :after dash-docs
  :bind
  ( :map krisb-lsp-keymap
    ("Di" . dash-docs-install-docset)
    ("Dl" . dash-docs-completing-read-at-point)
    ("DL" . dash-docs-completing-read)))

;;;; Hideshow
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook ((prog-mode conf-mode) . hs-minor-mode)
  :bind (("C-M-s-<tab>" . kb/hs-cycle)
         ("C-M-S-s-<iso-lefttab>" . kb/hs-global-cycle))
  :custom
  (hs-set-up-overlay 'kb/hideshow-display)
  (hs-isearch-open t)
  (hs-allow-nesting t)
  :config
  (defun kb/hideshow-display (ov)
    "Text to replace hidden text with."
    (overlay-put ov 'display
                 (propertize (format " [%d lines elided…] "
                                     (1- (count-lines (overlay-start ov)
                                                      (overlay-end ov))))
                             'face '(:weight light :inherit shadow))))

  ;; Taken from karthink's config. Also read
  ;; https://karthinks.com/software/simple-folding-with-hideshow/
  (defun kb/hs-cycle (&optional level)
    (interactive "p")
    (save-excursion
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;;TODO: Fix this case. `hs-show-block' needs to be called twice to
             ;;open all folds of the parent block.
             (hs-show-block)
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  ;; Taken from karthink's config. Also read
  ;; https://karthinks.com/software/simple-folding-with-hideshow/
  (defun kb/hs-global-cycle ()
    "Cycle all blocks in buffer."
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all)))))

(provide 'programming-ide-base-rcp)
;;; programming-ide-base-rcp.el ends here
