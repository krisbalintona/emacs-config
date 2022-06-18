;;; programming-lsp-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All configuration related to LSP-mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Lsp-mode
;; Use the language server protocol as a backend for Emacs.
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; Lsp-mode only when buffer is visible
  :gfhook
  'lsp-enable-which-key-integration
  'lsp-headerline-breadcrumb-mode
  'lsp-lens-mode
  '(lambda ()
     ;; Change `corfu' settings for LSP buffers
     (general-define-key ; Insert special separator character
      :keymaps 'local
      :states 'insert
      "C-SPC" '(lambda () (interactive) (insert corfu-separator)))
     (setq-local corfu-auto t
                 corfu-auto-delay 0.8
                 corfu-auto-prefix 1
                 corfu-quit-at-boundary t
                 corfu-separator ?·
                 orderless-component-separator "·")
     ;; Force re-enable `corfu-mode' in order for it to be
     ;; aware of the local change to `corfu-auto'
     (corfu-mode 1))
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
  (lsp-keymap-prefix "H-l")             ; Also have this be a prefix
  (lsp-auto-guess-root nil)
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't fontify headline breadcrumb text (janky fix)
  (lsp-modeline-diagnostics-scope :file)
  (lsp-modeline-code-actions-segments '(count icon name))
  (lsp-enable-file-watchers t) ; Affects performance. Enable if I do actual programming
  (lsp-file-watch-threshold 5000) ; Increase number of watched files until prompt emerges
  (lsp-log-io nil)                ; If set to true can cause a performance hit

  ;; Semantic tokens - font locking based on word's role
  (lsp-semantic-tokens-enable t)

  ;; When you hover over a symbol
  (lsp-enable-symbol-highlighting t)
  (lsp-symbol-highlighting-skip-current nil) ; When highlighting, don't highlight symbol at point?
  )

;;; Lsp-ui
;; Fancy frame and sideline overlay which shows useful information about what's
;; on the point.
(use-package lsp-ui
  :ghook 'lsp-mode-hook
  :hook ((lsp-ui-imenu-mode . hide-mode-line-mode)
         (lsp-mode . lsp-ui-mode))
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
  (lsp-ui-sideline-enable t)
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
  (lsp-eldoc-enable-hover nil)          ; Show eldoc info when hovering?
  (lsp-eldoc-render-all t)              ; Take as much space as needed?

  ;; Lsp-ui-doc - Show documentation
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-show-with-cursor t)       ; Point hover (alongside cursor!)
  (lsp-ui-doc-show-with-mouse nil)      ; Mouse hover (alongside cursor!)
  ;; Appearance
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-position 'top)
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

;;; Dap-mode
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :general
  (:keymaps 'lsp-mode-map
            :prefix "H-d"
            "d" '(dap-debug :wk "Debug")
            "l" '(dap-debug-last :wk "Debug last")
            "h" '(dap-hydra :wk "Hydra")
            "q" '(dap-disconnect :wk "Quit")
            "r" '(dap-ui-repl :wk "REPL")

            "b" '(:ignore t :wk "Breakpoints")
            "bt" '(dap-breakpoint-toggle :wk "Toggle breakpoint")
            "ba" '(dap-breakpoint-toggle :wk "Add breakpoint")
            "bd" '(dap-breakpoint-delete :wk "Delete breakpoint")
            "bD" '(dap-breakpoint-delete-all :wk "Delete all breakpoints")
            "bl" '(dap-breakpoint-log-message :wk "Breakpoint log")
            "bc" '(dap-breakpoint-condition :wk "Breakpoint condition")

            "e" '(:ignore t :wk "Expressions")
            "ea" '(dap-ui-expressions-add :wk "Expression add")
            "er" '(dap-ui-expressions-remove :wk "Expression add")
            )
  (:keymaps 'lsp-mode-map
            "H-c" 'dap-debug-last
            "H-C" 'dap-debug-recent
            )
  (:keymaps 'lsp-mode-map
            :prefix "H-d"
            "d" '(dap-debug :wk "Debug")
            "l" '(dap-debug-last :wk "Debug last")
            "h" '(dap-hydra :wk "Hydra")
            "q" '(dap-disconnect :wk "Quit")
            "r" '(dap-ui-repl :wk "REPL")

            "b" '(:ignore t :wk "Breakpoints")
            "bt" '(dap-breakpoint-toggle :wk "Toggle breakpoint")
            "ba" '(dap-breakpoint-toggle :wk "Add breakpoint")
            "bd" '(dap-breakpoint-delete :wk "Delete breakpoint")
            "bD" '(dap-breakpoint-delete-all :wk "Delete all breakpoints")
            "bl" '(dap-breakpoint-log-message :wk "Breakpoint log")
            "bc" '(dap-breakpoint-condition :wk "Breakpoint condition")

            "e" '(:ignore t :wk "Expressions")
            "ea" '(dap-ui-expressions-add :wk "Expression add")
            "er" '(dap-ui-expressions-remove :wk "Expression add"))
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
     ))
  )

;;; Lsp-bridge
;; Asynchronous LSP client
(use-package lsp-bridge
  :straight (lsp-bridge :type git
                        :host github
                        :repo "manateelazycat/lsp-bridge"
                        :files (:defaults "*.py" "langserver"))
  :after lsp-mode
  :gfhook '(lambda ()                        ; For Xref support
             (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t))
  :init
  (defun kb/global-lsp-bridge-mode ()
    "My own, non-scuffed version of the command."
    (interactive)
    (dolist (hook lsp-bridge-default-mode-hooks)
      (add-hook hook (lambda () (lsp-bridge-mode 1))))
    (setq lsp-bridge-diagnostics-timer
          (run-with-idle-timer lsp-bridge-diagnostics-fetch-idle t #'lsp-bridge-diagnostics-fetch)))
  :config
  (kb/global-lsp-bridge-mode)

  ;; Enable extension
  (require 'lsp-bridge-jdtls) ; Provide Java third-party library jump and -data directory support, optional
  (require 'lsp-bridge-icon)  ; Show icons for completion items

  ;; For corfu users with HiDPI screen
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3)))))))

;;; Ancillary
;;;; Consult-lsp
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

;;;; Lsp-treemacs
;; Treemacs-like buffer that shows files, errors, symbol hierarchy, etc.
(use-package lsp-treemacs
  :after treemacs
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
            "x" 'lsp-treemacs-quick-fix)
  )

;;; programming-lsp-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-lsp-rcp)
