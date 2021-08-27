;;; completion-default-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Selectrum completion framework and its cousin packages.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Built-in
;; A lot of this is taken from
;; https://protesilaos.com/dotemacs/#h:c110e399-3f43-4555-8427-b1afe44c0779
(setq completion-styles '(basic initials partial-completion flex)
      completion-category-overrides     ; partial-completion is easier for files
      '((file (styles . (partial-completion orderless))))
      completion-cycle-threshold 2 ; Number of candidates until cycling turns off
      completion-flex-nospace nil
      completion-pcm-complete-word-inserts-delimiters nil
      completion-pcm-word-delimiters "-_./:| " ; Word delimiters
      completion-show-help nil
      completion-auto-help t
      completion-ignore-case t
      ;; The following two are updated in Emacs 28.  They concern the
      ;; *Completions* buffer.
      completions-format 'one-column
      completions-detailed t ; Show more details in completion minibuffer (inspired by `marginalia')
      ;; Grouping of completions for Emacs 28
      completions-group t
      completions-group-sort nil
      completions-group-format
      (concat
       (propertize "    " 'face 'completions-group-separator)
       (propertize " %s " 'face 'completions-group-title)
       (propertize " " 'face 'completions-group-separator
                   'display '(space :align-to right)))

      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t

      enable-recursive-minibuffers t   ; Allow minibuffer commands in minibuffer
      read-answer-short t           ; also check `use-short-answers' for Emacs28
      resize-mini-windows t         ; Not sure what this does
      minibuffer-eldef-shorten-default t) ; Shorten "(default ...)" to "[...]" in minibuffer prompts.
(setq-default case-fold-search t)         ; For general regexp

;;;; Vertico
(use-package vertico
  :ghook 'after-init-hook
  :gfhook '(lambda ()
             ;; (setq completion-styles '(substring initials partial-completion orderless)
             (setq completion-styles '(substring orderless)
                   ))
  :custom
  ;; Workaround for problem with `org-refile'. See
  ;; https://github.com/minad/vertico#org-refile
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  (setq completion-styles '(orderless)
        completion-category-overrides '((file (styles basic-remote partial-completion orderless))))
  )

;;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :custom
  (orderless-component-separator " +")
  (orderless-matching-styles
   '(orderless-flex
     orderless-strict-leading-initialism
     orderless-regexp
     orderless-prefixes
     orderless-literal))
  ;; (orderless-style-dispatchers
  ;;  '(prot-orderless-literal-dispatcher
  ;;    prot-orderless-initialism-dispatcher
  ;;    prot-orderless-flex-dispatcher))
  )

;;;; Selectrum
;; Advanced complete-read
(use-package selectrum
  :disabled t                           ; Trying out `vertico'
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
;; Selectrum with `prescient' completion style
(use-package selectrum-prescient
  :ghook 'after-init-hook
  :custom
  ;; Use `prescient' to sort and filter in `selectrum-mode'
  (selectrum-prescient-enable-filtering t)
  (selectrum-prescient-enable-sorting t)
  :config
  (set-face-attribute 'selectrum-prescient-primary-highlight nil
                      :foreground "#dc85f7")
  (set-face-attribute 'selectrum-prescient-secondary-highlight nil
                      :foreground "#E5C07B")
  )

;;; completion-default-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-default-rcp)
