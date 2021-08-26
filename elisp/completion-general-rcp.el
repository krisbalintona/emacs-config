;;; completion-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are settings and/or packages which are package agnostic, some involved
;; with the default Emacs completion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Default completion settings
;; A lot of this is taken from
;; https://protesilaos.com/dotemacs/#h:c110e399-3f43-4555-8427-b1afe44c0779
;; (setq completion-styles '(basic initials partial-completion flex))
(setq completion-styles '(substring initials partial-completion orderless)
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

;;;;  Orderless
;; Alternative and powerful completion style
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

;;;; Prescient
;; Sorting and filtering of minibuffer candidates. Big benefit is having most
;; recent candidate shown on top
(use-package prescient
  ;; :after selectrum
  :hook (selectrum-prescient-mode . prescient-persist-mode)
  )

;;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :ghook 'after-init-hook
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" '(marginalia-cycle :which-key "Marginalia cycle"))
  :custom
  (marginalia-max-relative-age 0)       ; Don't show relative ages
  :config
  ;; Marginalia faces
  (set-face-attribute 'marginalia-documentation nil
                      :inherit nil
                      :foreground "#98C379"
                      :slant 'italic)
  )

;;; completion-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-general-rcp)
