;;; completion-helm-rcp.el --- Summary
;;
;; Helm completion framework and cousin packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Helm
;; Feature-rich version of Ivy completion
;; NOTE: With the experimental minor mode selectrum-helm-mode, Helm defaults
;; to using complete-read (and thus Selectrum when selectrum-mode is active)
(use-package helm
  :custom
  ;; Fuzzy matching
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-apropos-fuzzy-match t)
  (helm-semantic-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  (helm-M-x-fuzzy-match t) 

  (helm-autoresize-mode t)
  :config
  (general-unbind "C-x c") ; Unbind original helm-prefix

  (general-define-key
   :keymaps 'helm-map
   "<tab>" 'helm-execute-persistent-action ; Run persistent action
   "M-o"  'helm-select-action) ; List actions

  (kb/leader-keys
    "oh" '(helm-command-prefix :which-key "Helm prefix")
    "oha" '(helm-apropos :which-key "Helm-apropos")

    ;; "bb" 'helm-mini
    ;; "ff" 'helm-find-files
    )
  )

;;;; Helm-swoop
;; Interactive version of multi-occur with helm
(use-package helm-swoop
  :disabled t ; Conflicting bingings within PDF
  :custom
  (helm-swoop-split-with-multiple-windows nil) ; If this value is t, split window inside the current window
  (helm-swoop-split-direction 'split-window-vertically) ; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (helm-swoop-speed-or-color t) ; If nil, you can slightly boost invoke speed in exchange for text color
  (helm-multi-swoop-edit-save t) ; Save buffer when helm-multi-swoop-edit complete
  :config
  (general-define-key ; From helm-swoop to helm-multi-swoop-all
   :keymaps 'helm-swoop-map
   "M-i" 'helm-multi-swoop-all-from-helm-swoop)

  (general-define-key ; Helm-swoop when in isearch
   :keymaps 'isearchp-mode-map
   "M-i" 'helm-swoop-from-isearch)

  (general-define-key ; When doing evil-search, hand the word over to helm-swoop
   :keymaps 'evil-motion-state-map
   "M-i" 'helm-swoop-from-evil-search)

  (general-define-key
   :keymaps '(helm-find-files-map helm-read-file-map)
   "C-<backspace>" 'helm-find-files-up-one-level) ; This overwrites auto expansion toggle?

  (kb/leader-keys
    "shs" '(helm-swoop :which-key "Helm-swoop")
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-helm-rcp)
;;; Commentary:
;;
;;; completion-helm-rcp.el ends here
