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
  :diminish ((lsp-mode . "LSP")
             (lsp-lens-mode . "Lens"))
  :gfhook
  'lsp-enable-which-key-integration
  'lsp-lens-mode
  :general
  (:keymaps 'lsp-mode-map
            :states 'insert
            "<tab>" 'indent-for-tab-command)
  (kb/lsp-keys ; Remap all the keys from `lsp-command-map'
    "w" '(ignore t :wk "Workspaces")
    "wD" '(lsp-disconnect :wk "disconnect")
    "wd" '(lsp-describe-session :wk "describe session")
    "wq" '(lsp-workspace-shutdown :wk "shutdown server")
    "wr" '(lsp-workspace-restart :wk "restart server")
    "ws" '(lsp :wk "start server")

    "=" '(ignore t :wk "Formatting")
    "==" '(lsp-format-buffer :wk "format buffer")
    "=r" '(lsp-format-region :wk "format region")

    "F" '(ignore t :wk "Folders")
    "Fa" '(lsp-workspace-folders-add :wk "add folder")
    "Fb" '(lsp-workspace-blacklist-remove :wk "un-blacklist folder")
    "Fr" '(lsp-workspace-folders-remove :wk "remove folder")

    "T" '(ignore t :wk "Toggles")
    "TD" '(lsp-modeline-diagnostics-mode :wk "toggle modeline diagnostics")
    "TL" '(lsp-toggle-trace-io :wk "toggle log io")
    "TS" '(lsp-ui-sideline-mode :wk "toggle sideline")
    "TT" '(lsp-treemacs-sync-mode :wk "toggle treemacs integration")
    "Ta" '(lsp-modeline-code-actions-mode :wk "toggle modeline code actions")
    "Tb" '(lsp-headerline-breadcrumb-mode :wk "toggle breadcrumb")
    "Td" '(lsp-ui-doc-mode :wk "toggle documentation popup")
    "Tf" '(lsp-toggle-on-type-formatting :wk "toggle on type formatting")
    "Th" '(lsp-toggle-symbol-highlight :wk "toggle highlighting")
    "Tl" '(lsp-lens-mode :wk "toggle lenses")
    "Ts" '(lsp-toggle-signature-auto-activate :wk "toggle signature")

    "g" '(ignore t :wk "Gotos")
    "ga" '(xref-find-apropos :wk "find symbol in workspace")
    "gd" '(lsp-find-declaration :wk "find declarations")
    "ge" '(lsp-treemacs-errors-list :wk "show errors")
    "gg" '(lsp-find-definition :wk "find definitions")
    "gh" '(lsp-treemacs-call-hierarchy :wk "call hierarchy")
    "gi" '(lsp-find-implementation :wk "find implementations")
    "gr" '(lsp-find-references :wk "find references")
    "gt" '(lsp-find-type-definition :wk "find type definition")

    "h" '(ignore t :wk "Help")
    "hg" '(lsp-ui-doc-glance :wk "glance symbol")
    "hh" '(lsp-describe-thing-at-point :wk "describe symbol at point")
    "hs" '(lsp-signature-activate :wk "signature help")

    "r" '(ignore t :wk "Refactoring")
    "ro" '(lsp-organize-imports :wk "organize imports")
    "rr" '(lsp-rename :wk "rename")

    "a" '(ignore t :wk "Actions")
    "aa" '(lsp-execute-code-action :wk "code actions")
    "ah" '(lsp-document-highlight :wk "highlight symbol")
    "al" '(lsp-avy-lens :wk "lens")

    "p" '(ignore t :wk "Peeks")
    "Gg" '(lsp-ui-peek-find-definitions :wk "peek definitions")
    "Gi" '(lsp-ui-peek-find-implementation :wk "peek implementations")
    "Gr" '(lsp-ui-peek-find-references :wk "peek references")
    "Gs" '(lsp-ui-peek-find-workspace-symbol :wk "peek workspace symbol")
    )
  :custom
  ;; core
  (lsp-keymap-prefix "H-l")             ; Also have this be a prefix
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
  :ghook 'lsp-mode-hook
  :hook (lsp-ui-imenu-mode . hide-mode-line-mode)
  :gfhook
  '(lambda ()
     (when (bound-and-true-p eldoc-box-hover-mode)
       (eldoc-box-hover-mode -1)))
  :general (:keymaps 'lsp-ui-mode-map
                     [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
                     [remap xref-find-references] #'lsp-ui-peek-find-references
                     [remap imenu-list] #'lsp-ui-imenu)
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
  :hook (lsp-mode . (lambda () ; Need to do it this way since adding to `lsp-mode-map' doesn't work
                      (general-define-key
                       :keymaps 'local
                       :states '(normal visual)
                       "gs" '(consult-lsp-symbols :wk "Consult lsp symbols regexp")
                       "gf" '(consult-lsp-file-symbols :wk "Consult lsp file symbols list")
                       )))
  :general
  (:keymaps 'lsp-mode-map
            [remap consult-flycheck] '(consult-lsp-diagnostics :wk "Consult lsp diagnostics")))

;;;;; Lsp-treemacs
;; Treemacs-like buffer that shows files, errors, symbol hierarchy, etc.
(use-package lsp-treemacs
  :requires treemacs
  :after lsp
  :hook ((lsp-mode . lsp-treemacs-sync-mode)
         (lsp-treemacs-generic-mode . hide-mode-line-mode)
         (lsp-treemacs-error-list-mode . hide-mode-line-mode)
         (lsp-treemacs-deps-list-mode . hide-mode-line-mode)
         )
  :general
  (kb/lsp-keys
    "Ft" '(lsp-treemacs-symbols :wk "Lsp-treemacs"))
  (:keymaps 'lsp-treemacs-error-list-mode-map
            :states 'normal
            "x" 'lsp-treemacs-quick-fix))

(provide 'programming-lsp-mode-rcp)
;;; programming-lsp-mode-rcp.el ends here
