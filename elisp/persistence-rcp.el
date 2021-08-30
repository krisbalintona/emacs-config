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
  :straight nil
  :ghook 'after-init-hook
  :custom
  (savehist-autosave-interval 300)
  :config
  (add-to-list 'savehist-additional-variables 'recentf-list) ; Save recent files
  (add-to-list 'savehist-additional-variables 'kill-ring) ; Save kill ring
  )

;;;; Recentf
;; Enable logging of recent files
(use-package recentf
  :straight nil
  :ghook 'after-init-hook
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  )

;;;; Save-place-mode
;; Save and restore the point's location in files
(use-package saveplace
  :straight nil
  :ghook ('after-init-hook 'save-place-mode)
  )

;;; persistence-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'persistence-rcp)
