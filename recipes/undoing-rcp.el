;;; undoing-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for undoing and redoing mistakes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Undo-fu
;; Easy and simple undoing
(use-package undo-fu
  :demand t
  :general ([remap undo] 'undo-fu-only-undo
            [remap undo-redo] 'undo-fu-only-redo)
  :custom
  ;; Store more undo history to prevent loss of data
  (undo-limit (* 100 1024))
  (undo-strong-limit 3000000)
  (undo-outer-limit 3000000)
  (evil-undo-system 'undo-fu)
  )

;;; Undo-fu-session
;; Keep undo history across sessions
(use-package undo-fu-session
  :requires undo-fu
  :hook (after-init . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-directory (concat no-littering-var-directory "undo-fu-session/"))
  (undo-fu-session-file-limit 15000)
  )

;;; undoing-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'undoing-rcp)
