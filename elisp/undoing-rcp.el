;;; undoing-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for undoing mistakes and redoing them
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Undo-fu
;; Easy and simple undoing
(use-package undo-fu
  :hook (after-init . undo-fu-mode)
  :custom
  ;; Store more undo history to prevent loss of data
  (undo-limit (* 100 1024))
  (undo-strong-limit 3000000)
  (undo-outer-limit 3000000)
  :config
  (general-define-key
   [remap undo] 'undo-fu-only-undo
   [remap redo] 'undo-fu-only-redo
   )
  )

;;;; Undo-fu-session
;; Keep undo history across sessions
(use-package undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-directory (concat user-emacs-directory "undo-fu-session/"))
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-directory (concat no-littering-var-directory "undo-fu-session/"))
  (undo-fu-session-file-limit 15000)
  )

;;; undoing-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'undoing-rcp)
