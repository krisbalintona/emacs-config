;;; completion-selectrum-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Selectrum completion framework and cousin packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Selectrum
;; Advanced complete-read
(use-package selectrum
  ;:after ivy
  :hook (after-init . selectrum-mode)
  :custom
  (selectrum-num-candidates-displayed 10) ; Maximum candidates shown
  (selectrum-fix-minibuffer-height t) ; Fixed height?
  (selectrum-extend-current-candidate-highlight t) ; Highlight entire line
  (selectrum-count-style 'current/matches)
  (selectrum-show-indices nil) ; Can also be custom if passed a function
  :config
  ;; Selectrum minibuffer faces
  ;; Foregrounds based on ivy-minibuffer-match-face-*
  (set-face-attribute 'selectrum-current-candidate nil
                      :inherit 'ivy-minibuffer-match-highlight
                      :weight 'semi-bold)
  (set-face-attribute 'selectrum-completion-annotation nil
                      :inherit 'ivy-grep-info)

  (kb/leader-keys
    "ff" '(find-file :which-key "Find file")
    "hf" '(describe-function :which-key "Desc func")
    "hv" '(describe-variable :which-key "Desc var")
    "ho" '(describe-symbol :which-key "Desc sym")
    )
  )

;;;; Cousin packages
;;;;; Selectrum-prescient
;; Selectrum with prescient completion style
(use-package selectrum-prescient
  :hook (after-init . selectrum-prescient-mode)
  :config
  (set-face-attribute 'selectrum-prescient-primary-highlight nil
                      :foreground "#dc85f7")
  (set-face-attribute 'selectrum-prescient-secondary-highlight nil
                      :foreground "#E5C07B")
  )

;;;;; Consult.el
;; Counsel equivalent for default Emacs (and thus selectrum!)
(use-package consult
  :ensure-system-package ((fd . fdfind)
                          (rg . ripgrep))
  :after selectrum
  :straight (consult :type git :host github :repo "minad/consult")
  :custom
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

  (consult-project-root-function #'projectile-project-root)
  :config
  (defun consult-fdfind (&optional dir)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path"))
      (consult-find dir))
    )

  ;; Customize consult commands
  (consult-customize
   ;; For `consult-buffer'
   consult-buffer :preview-key (kbd "M-l")
   consult-buffer :prompt "Can use b, m, f, p..."
   ;; For `consult-ripgrep'
   consult-ripgrep :preview-key (kbd "M-l")
   ;; For `consult-fdfind'. Make sure this is after the definition of
   ;; `consult-fdfind'
   consult-fdfind :preview-key (kbd "M-l")
   )

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
    "iy" '(consult-yank-pop :which-key "Consult yank-pop")
    "ha" '(consult-apropos :which-key "Consult apropos")
    "pf" '(consult-find :which-key "Consult find file")
    "ps" '(consult-ripgrep :which-key "Consult rg")
    )
  )

;;;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)) ; Show as much information as possible
  :config
  ;; Marginalia faces
  (set-face-attribute 'marginalia-documentation nil
                      :inherit nil
                      :foreground "#98C379"
                      :slant 'italic)
  )

;;;;; Embark
;; Allow an equivalent to ivy-actions to regular complete-read minibuffers (and
;; thus selectrum!)
(use-package embark
  :custom
  (embark-prompt-style 'default) ; Or manual completion
  (embark-action-indicator (concat ; A function, string, or nil - remember: propertize returns a string
                            (propertize "Embark on" 'face '(bold underline))
                            ":")) 
  :config
  (general-define-key
   ;; :keymaps 'selectrum-minibuffer-map
   "M-o" '(embark-act :which-key "Embark-act")
   ;; "?" '(embark-act-noexit :which-key "Embark-act-noexit")
   ;; "?" '(embark-export :which-key "Embark-export")
   )
  )

;;;;;; Embark with Selectrum
;; Embark elisp to enable selectrum to be used in selectrum (the way I want to
;; use it)

;; Allows embark to take targets from the selectrum minibuffer and act on them.
;; Taken from
;; https://github.com/raxod502/selectrum/wiki/Additional-Configuration#minibuffer-actions-with-embark
(with-eval-after-load 'embark
  (add-hook 'embark-target-finders 'selectrum-get-current-candidate)
  (add-hook 'embark-candidate-collectors
            (defun embark-selectrum-candidates+ ()
              (when selectrum-active-p
                (selectrum-get-current-candidates
                 ;; Pass relative file names for dired.
                 minibuffer-completing-file-name))))
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate) ; No unnecessary computation delay after injection.
  (add-hook 'embark-input-getters
            (defun embark-selectrum-input-getter+ ()
              (when selectrum-active-p
                (let ((input (selectrum-get-current-input)))
                  (if minibuffer-completing-file-name
                      ;; Only get the input used for matching.
                      (file-name-nondirectory input)
                    input)))))

  ;; Reset list after embark-act (which may change candidates e.g. delete-file).
  ;; Taken from
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#selectrum
  (defun refresh-selectrum ()
    "Reset the Selectrum candidate list."
    (setq selectrum--previous-input-string nil))
  (add-hook 'embark-pre-action-hook #'refresh-selectrum)
  )

;;;;;; Embark with Consult
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

;;;;;; Misc embark actions
;; Embark actions which are useful but I either haven't found a place for them
;; yet or have yet to properly solidify its place in its own heading. Bind
;; actions in the maps of embark-keymap-alist to make use of them within embark.

;; Magit status of repo containing a given file. Taken from Magit status of repo
;; containing a given file
(defun embark-magit-status ()
  "Run `magit-status' on repo containing the embark target."
  (interactive)
  (magit-status (locate-dominating-file (embark-target) ".git")))

;;; completion-selectrum-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-selectrum-rcp)
