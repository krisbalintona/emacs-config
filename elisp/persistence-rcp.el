;;; persistence-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages relevant to saving and loading information across Emacs sessions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Savehist
;; Make history of certain things (e.g. minibuffer) persistent across sessions
(use-package savehist
  :ghook 'after-init-hook
  :custom
  (history-length 10000)
  (history-delete-duplicates t)
  (savehist-autosave-interval 30)
  (savehist-save-minibuffer-history t)
  :config
  (add-to-list 'savehist-additional-variables 'recentf-list) ; Save recent files
  (add-to-list 'savehist-additional-variables 'kill-ring)    ; Save kill ring
  )

;;;; Recentf
;; Enable logging of recent files
(use-package recentf
  :ghook 'after-init-hook
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  )

;;;; Saveplace
;; Save and restore the point's location in files
(use-package saveplace
  :ghook ('after-init-hook 'save-place-mode)
  :custom
  (save-place-forget-unreadable-files t)
  )


;;; persistence-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'persistence-rcp)
