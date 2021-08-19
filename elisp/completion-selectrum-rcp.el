;;; completion-selectrum-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Selectrum completion framework and its cousin packages.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Selectrum
;; Advanced complete-read
(use-package selectrum
  :ghook 'after-init-hook
  :general (kb/leader-keys
             "ff" '(find-file :which-key "Find file")
             "hf" '(describe-function :which-key "Desc func")
             "hv" '(describe-variable :which-key "Desc var")
             "ho" '(describe-symbol :which-key "Desc sym")
             )
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
  )

;;;;; Selectrum-prescient
;; Selectrum with prescient completion style
(use-package selectrum-prescient
  :ghook 'after-init-hook
  :custom
  ;; Use `prescient' to sort and filter
  (selectrum-prescient-enable-filtering t)
  (selectrum-prescient-enable-sorting t)

  ;; How does it filter?
  (prescient-filter-alist '((literal . prescient-literal-regexp)
                            (literal-prefix . prescient-literal-prefix-regexp)
                            (initialism . prescient-initials-regexp)
                            (regexp . prescient-regexp-regexp)
                            (fuzzy . prescient-fuzzy-regexp)
                            (prefix . prescient-prefix-regexp)
                            (anchored . prescient-anchored-regexp))
                          )
  (prescient-filter-method '(literal regexp anchored initialism))

  (prescient-use-char-folding t)
  (prescient-use-case-folding t)
  (prescient-sort-full-matches-first t)

  (prescient-history-length 200)
  (prescient-frequency-decay 0.999)
  (prescient-frequency-threshold 0.10)
  :config
  (set-face-attribute 'selectrum-prescient-primary-highlight nil
                      :foreground "#dc85f7")
  (set-face-attribute 'selectrum-prescient-secondary-highlight nil
                      :foreground "#E5C07B")
  )

;;;; Consult
;; Counsel equivalent for default Emacs (and thus selectrum!)
(use-package consult
  :ensure-system-package ((fd . fd-find)
                          (rg . ripgrep))
  :straight (consult :type git :host github :repo "minad/consult")
  :after selectrum
  :general
  ([remap apropos-command] '(consult-apropos :which-key "Consult apropos"))
  (kb/leader-keys
    :keyamps 'prog-mode-map
    "le" '(consult-error :which-key "Consult error"))
  (kb/leader-keys
    :keymaps 'org-mode-map
    [remap consult-outline] '(consult-org-heading :which-key "Consult outline"))
  (kb/leader-keys
    "fr" '(consult-recent-file :which-key "Consult recent file")
    "bb" '(consult-buffer :which-key "Consult buffer")
    ;; ("C-x 4 b" . consult-buffer-other-window)
    ;; ("C-x 5 b" . consult-buffer-other-frame)

    "mm" '(consult-bookmark :which-key "Consult bookmark")
    "mr" '(consult-mark :which-key "Consult mark-ring")

    "so" '(consult-outline :which-key "Consult outline")

    "ss" '(consult-line :which-key "Consult swiper")
    "si" '(consult-imenu :which-key "Consult imenu")
    "sO" '(consult-multi-occur :which-key "Consult multi-occur")

    "iy" '(consult-yank-pop :which-key "Consult yank-pop")
    ;; ("C-x r x" . consult-register)
    "ha" '(consult-apropos :which-key "Consult apropos")
    "pf" '(consult-find :which-key "Consult find file")
    "ps" '(consult-ripgrep :which-key "Consult rg")
    )
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

  (consult-project-root-function #'doom-modeline-project-root)
  :config
  ;; Customize consult commands
  (consult-customize
   ;; For `consult-buffer'
   consult-buffer :preview-key (kbd "M-l")
   consult-buffer :prompt "Can use b, m, f, p..."
   ;; For `consult-ripgrep'
   consult-ripgrep :preview-key (kbd "M-l")
   ;; For `consult-fdfind'. Make sure this is after the definition of
   ;; `consult-recent-file'
   consult-recent-file :preview-key (kbd "M-l")
   ;; `consult-find'
   consult-find :preview-key (kbd "M-l")
   )
  )

;;;; Embark
;; Allow an equivalent to ivy-actions to regular complete-read minibuffers (and
;; thus selectrum!)
(use-package embark
  :after which-key ; Because I replace its value of `prefix-help-command'
  :general
  ("M-o" '(embark-act :which-key "Embark-act"))
  (kb/leader-keys
    "hb" '(embark-bindings :which-key "Embark-bindings")
    )
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  )

;;;; Embark-consult
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :requires (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

;;; completion-selectrum-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-selectrum-rcp)
