;;; undoing-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for undoing and redoing mistakes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Undo-fu
;; Easy and simple undoing
(use-package undo-fu
  :custom
  ;; Store more undo history to prevent loss of data
  (undo-limit (* 100 1024))
  (undo-strong-limit 3000000)
  (undo-outer-limit 3000000)
  :general ([remap undo] 'undo-fu-only-undo
            [remap redo] 'undo-fu-only-redo)
  )

;;;; Undo-fu-session
;; Keep undo history across sessions
(use-package undo-fu-session
  :requires undo-fu
  :ghook ('after-init-hook 'global-undo-fu-session-mode)
  :custom
  (undo-fu-session-directory (concat user-emacs-directory "undo-fu-session/"))
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-directory (concat no-littering-var-directory "undo-fu-session/"))
  (undo-fu-session-file-limit 15000)
  )

;;; undoing-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'undoing-rcp)
