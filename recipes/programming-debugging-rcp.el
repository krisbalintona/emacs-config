;;; programming-debugging-rcp.el --- Debugging programs  -*- lexical-binding: t; -*-

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

;; All configuration related to debugging.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Gud
(use-package gud
  :ensure nil
  :custom
  (gud-highlight-current-line t))

;;;; Realgud
(use-package realgud
  :hook (realgud-srcbuf-mode . tool-bar-mode)
  :custom
  (realgud-window-split-orientation 'horizontal)
  (realgud-short-key-on-tracing? t))

;;;; Dap-mode
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :bind
  ( :map lsp-mode-map
    ("<f6> d" . dap-debug)
    ("<f6> l" . dap-debug-last)
    ("<f6> h" . dap-hydra)
    ("<f6> q" . dap-disconnect)
    ("<f6> r" . dap-ui-repl)

    ("<f6> b" . :ignore)
    ("<f6> bt" . dap-breakpoint-toggle)
    ("<f6> ba" . dap-breakpoint-toggle)
    ("<f6> bd" . dap-breakpoint-delete)
    ("<f6> bD" . dap-breakpoint-delete-all)
    ("<f6> bl" . dap-breakpoint-log-message)
    ("<f6> bc" . dap-breakpoint-condition)

    ("<f6> e" . :ignore)
    ("<f6> ea" . dap-ui-expressions-add)
    ("<f6> er" . dap-ui-expressions-remove)

    ("C-M-s-c" . dap-debug-last)
    ("C-M-s-C" . dap-debug-recent))
  :custom
  (dap-debug-compilation-keep t)        ; Keep output window in success?
  (dap-debug-restart-keep-session nil)  ; Delete previous sessions

  (dap-auto-configure-features '(;; sessions
                                 locals
                                 breakpoints
                                 expressions
                                 controls
                                 tooltip
                                 ))

  ;; Dap-ui window configurations
  (dap-ui-buffer-configurations
   `((,dap-ui--sessions-buffer
      (side . right)
      (slot . 2)
      (window-width . 0.12))
     (,dap-ui--breakpoints-buffer
      (side . right)
      (slot . 3)
      (window-width . 0.12))
     (,dap-ui--locals-buffer
      (side . left)
      (slot . 1)
      (window-width . 0.15))
     (,dap-ui--expressions-buffer
      (side . left)
      (slot . 2)
      (window-width . 0.15))
     (,dap-ui--repl-buffer
      (side . bottom)
      (slot . 2)
      (window-height . 0.12)
      (window-parameters . ((mode-line-format . none))))
     )))

;;;; Dape
;; Dap-mode but without LSP-mode
(use-package dape
  :diminish dape-breakpoint-global-mode
  :custom
  (dape-key-prefix nil)                 ; I make my own binding
  (dape-buffer-window-arrangement 'right)
  (dape-stepping-granularity 'instruction)
  (dape-info-variable-table-aligned t)
  :config
  (dape-breakpoint-global-mode 1)

  (bind-key "C-c d" dape-global-map 'prog-mode-map)

  ;; Kill created compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  (defun kb/dape--save-on-start ()
    "Save buffers on startup."
    (save-some-buffers nil t))
  (add-hook 'dape-on-start-hooks 'kb/dape--save-on-start)

  ;; To display info and repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl))

(provide 'programming-debugging-rcp)
;;; programming-debugging-rcp.el ends here
