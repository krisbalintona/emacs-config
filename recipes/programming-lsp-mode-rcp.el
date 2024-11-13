;;; programming-lsp-mode-rcp.el --- LSP-mode         -*- lexical-binding: t; -*-

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

;; All configuration related to lsp-mode.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Lsp-mode
;; Use the language server protocol as a backend for Emacs.
(use-package lsp-mode
  :disabled
  :diminish ((lsp-mode . "LSP")
             (lsp-lens-mode . "Lens"))
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . lsp-lens-mode))
  :custom
  ;; core
  (lsp-keymap-prefix "C-M-s-l")             ; Also have this be a prefix
  (lsp-keep-workspace-alive nil)
  (lsp-auto-guess-root nil)
  (lsp-log-io nil)                  ; If set to true can cause a performance hit
  (lsp-idle-delay 0.05)
  (lsp-enable-xref t)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 5000) ; Increase number of watched files until prompt emerges
  (lsp-enable-folding t)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-links t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting t)
  (lsp-symbol-highlighting-skip-current t) ; When highlighting, don't highlight symbol at point?
  (lsp-enable-text-document-color t)

  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-provider :none) ; Don't use company
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-enable-snippet t)
  (lsp-completion-show-kind t)

  ;; headerline
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't fontify headline breadcrumb text (janky fix)
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)
  (lsp-headerline-breadcrumb-icons-enable t)

  ;; modeline
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-diagnostics-scope :file)
  (lsp-modeline-code-actions-segments '(count icon))
  (lsp-modeline-workspace-status-enable nil)

  ;; lens
  (lsp-lens-enable t)

  ;; semantic
  (lsp-semantic-tokens-enable t)
  :custom-face
  ;; (lsp-face-highlight-read ((t (:inherit nil :box (:line-width -1 :style nil)))))
  ;; (lsp-face-highlight-write ((t (:inherit nil :box (:line-width -1 :style nil)))))
  (lsp-face-highlight-textual ((t (:inherit nil :box (:line-width -1 :style nil))))))

;;;; Lsp-ui
;; Fancy frame and sideline overlay which shows useful information about what's
;; on the point.
(use-package lsp-ui
  :requires lsp
  :hook
  ((lsp-mode . lsp-ui-mode)
   (lsp-ui-imenu-mode . hide-mode-line-mode)
   (lsp-ui-mode . (lambda ()
                    (when (bound-and-true-p eldoc-box-hover-mode)
                      (eldoc-box-hover-mode -1)))))
  :bind
  ( :map lsp-ui-mode-map
    ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
    ([remap xref-find-references] . lsp-ui-peek-find-references)
    ([remap imenu-list] . lsp-ui-imenu))
  :custom
  ;; Lsp-ui-peek - Peek in a child frame
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-peek-show-directory t)

  ;; Lsp-ui-sideline - Info at the side
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics t)  ; Show diagnostics messages in sideline?
  (lsp-ui-sideline-show-hover nil)      ; Show hover messages in sideline?
  (lsp-ui-sideline-show-code-actions nil) ; Show code actions in sideline?
  ;; When set to 'line' the information will be updated when user changes
  ;; current line otherwise the information will be updated when user changes
  ;; current point
  (lsp-ui-sideline-update-mode 'point)
  (lsp-ui-sideline-delay 0.2)          ; Seconds to wait before showing sideline

  ;; Lsp-eldoc - Info in the echo area
  (lsp-eldoc-hook '(lsp-hover))
  (lsp-eldoc-enable-hover t)            ; Show eldoc info when hovering?
  (lsp-eldoc-render-all nil)            ; Take as much space as needed?

  ;; Lsp-ui-doc - Show documentation
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-show-with-cursor t)       ; Point hover (alongside cursor!)
  (lsp-ui-doc-show-with-mouse nil)      ; Mouse hover (alongside cursor!)
  ;; Appearance
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-max-height 10)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-text-scale-level -1)
  (lsp-ui-doc-use-webkit nil)

  ;; Lsp-ui-imenu - Imenu integration
  (lsp-ui-imenu-window-width 70)
  (lsp-ui-imenu-auto-refresh 'after-save) ; Auto refresh
  (lsp-ui-imenu-auto-refresh-delay 1.0)   ; Variable doesn't exist?
  )

;;;; Ancillary
;;;;; Consult-lsp
(use-package consult-lsp
  :after lsp-mode
  :bind
  ( :map lsp-mode-map
    ([remap consult-flycheck] . consult-lsp-diagnostics)))

(provide 'programming-lsp-mode-rcp)
;;; programming-lsp-mode-rcp.el ends here
