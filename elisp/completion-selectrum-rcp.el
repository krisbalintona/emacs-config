;;; completion-selectrum-rcp.el --- Summary
;;
;; Selectrum completion framework and cousin packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Selectrum
;; Advanced complete-read
(use-package selectrum
  :hook (after-init . selectrum-mode)
  :custom
  ;; Change backends
  (amx-backend 'selectrum)
  ;; (projectile-completion-system 'default)
  :config
  ;; Selectrum minibuffer faces
  (set-face-attribute 'selectrum-current-candidate nil :inherit 'ivy-minibuffer-match-highlight)
  (set-face-attribute 'selectrum-primary-highlight nil :inherit 'ivy-minibuffer-match-face-2)
  (set-face-attribute 'selectrum-secondary-highlight nil :inherit 'ivy-minibuffer-match-face-4)
  (set-face-attribute 'selectrum-completion-annotation nil :inherit 'ivy-grep-info)
  )

;;;; Selectrum-prescient
;; Selectrum with prescient completion style
  (use-package selectrum-prescient
    :hook (after-init . selectrum-prescient-mode)
    )

;;;; Cousin packages
;;;;; Consult.el
;; Counsel equivalent for default Emacs (and thus selectrum!)
(use-package consult
  :after selectrum
  :straight (consult :type git :host github :repo "minad/consult")
  :hook (selectrum-mode . consult-preview-mode)
  :custom
  (consult-preview-outline nil) ; Annoying
  (consult-mode-histories ; What variable consult-history looks at for history
   '((eshell-mode . eshell-history-ring)
     (comint-mode . comint-input-ring)
     (term-mode . term-input-ring))
   )
  
  (consult-narrow-key [?<])
  (consult-widen-key [?< ?<])
  ;; Optional configure a "view" library to be used by `consult-buffer`.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)
  :config
  (general-define-key
   [remap apropos-command] '(consult-apropos :which-key "Consult apropos")
   )

  (add-hook 'prog-mode-hook
            (lambda ()
              (kb/leader-keys
                "le" '(consult-error :which-key "Consult error"))
              ))

  (kb/leader-keys
    "bb" '(consult-buffer :which-key "Consult buffer")
    ;; ("C-x 4 b" . consult-buffer-other-window)
    ;; ("C-x 5 b" . consult-buffer-other-frame)
    ;; ("C-x r x" . consult-register)
    "mm" '(counsel-bookmark :which-key "Consult bookmark")
    "mr" '(consult-mark :which-key "Consult mark-ring")
    "so" '(consult-outline :which-key "Consult outline")
    "ss" '(consult-line :which-key "Consult swiper")
    "si" '(consult-imenu :which-key "Consult imenu")
    ;; ("-s m" . consult-multi-occur)
    "ff" '(find-file :which-key "Find file")
    "iy" '(consult-yank-pop :which-key "Consult yank-pop")
    "ha" '(consult-apropos :which-key "Consult apropos")
    )
  )

;;;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)) ; Show as much information as possible
  )

;;;;; Embark
;; Allow an equivalent to ivy-actions to regular complete-read minibuffers (and
;; thus selectrum!)
(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  :config
  (general-define-key
   :keymaps 'selectrum-minibuffer-map
   "M-o" '(embark-act :which-key "Embark-act")
   ;; "?" '(embark-act-noexit :which-key "Embark-act-noexit")
   ;; "?" '(embark-export :which-key "Embark-export")
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-selectrum-rcp)
;;; Commentary:
;;
;;; completion-selectrum-rcp.el ends here
