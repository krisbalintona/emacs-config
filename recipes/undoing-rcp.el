;;; undoing-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for undoing and redoing mistakes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Undo-tree
;; Probably the first thing you'd miss is undo and redo, which requires an extra
;; package to work like it does in kakoune (and almost every other editor).
(use-package undo-tree
  :disabled t
  :general (:keymaps 'undo-tree-visualizer-mode-map
                     "h" 'undo-tree-visualize-switch-branch-left
                     "j" 'undo-tree-visualize-redo
                     "k" 'undo-tree-visualize-undo
                     "l" 'undo-tree-visualize-switch-branch-right)
  :custom
  (evil-undo-system 'undo-tree)
  :init
  (global-undo-tree-mode))

;;; Undo-fu
;; Easy and simple undoing
(use-package undo-fu
  :if (not (featurep 'undo-tree))       ; Only when undo-tree isn't active
  :custom
  (evil-undo-system 'undo-fu)
  ;; Store more undo history to prevent loss of data
  (undo-strong-limit 3000000)
  (undo-outer-limit 3000000))

;;; Undo-fu-session
;; Keep undo history across sessions
(use-package undo-fu-session
  :after undo-fu                        ; Depends on undo-fu
  :custom
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-directory (no-littering-expand-var-file-name "undo-fu-session/"))
  (undo-fu-session-file-limit 15000)
  :init
  (global-undo-fu-session-mode))

;;; undoing-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'undoing-rcp)
