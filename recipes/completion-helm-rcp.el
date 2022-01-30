;;; completion-helm-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Helm completion framework and its cousin packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Helm
;; Feature-rich version of Ivy completion
;; NOTE: With the experimental minor mode selectrum-helm-mode, Helm defaults
;; to using complete-read (and thus Selectrum when selectrum-mode is active)
(use-package helm
  :general (:keymaps 'helm-map
                     "M-o"  'helm-select-action        ; List actions
                     "<tab>" 'helm-toggle-visible-mark ; Toggle mark
                     )
  :custom
  (helm-autoresize-mode t)

  ;; Enable fuzzy matching
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-apropos-fuzzy-match t)
  (helm-semantic-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  (helm-M-x-fuzzy-match t)
  )

;;; completion-helm-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-helm-rcp)
