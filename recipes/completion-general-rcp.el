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
(require 'keybinds-general-rcp)

;;; Built-in
;; A lot of this is taken from
;; https://protesilaos.com/dotemacs/#h:c110e399-3f43-4555-8427-b1afe44c0779
(setq completion-styles '(basic initials partial-completion flex)
      completion-category-overrides
      '((file (styles . (basic
                         basic-remote ; For `tramp' hostname completion with `vertico'
                         partial-completion)))) ; Partial completion for file paths!
      completion-flex-nospace t
      completion-cycle-threshold nil ; Number of candidates until cycling turns off
      completion-lazy-hilit t      ; Performance; added Emacs 30.1
      completion-show-help nil
      completion-auto-help t
      completion-ignore-case t
      completion-pcm-complete-word-inserts-delimiters nil
      completion-pcm-word-delimiters "-_./:| " ; Word delimiters

      ;; The following two are updated in Emacs 28.  They concern the
      ;; *Completions* buffer.
      completions-format 'one-column
      completions-detailed t ; Show more details in completion minibuffer (inspired by `marginalia')
      ;; Grouping of completions for Emacs 28
      completions-group t

      ;; Functionality of `indent-for-tab-command'. Make sure tab doesn't indent
      ;; when you want to perform completion
      tab-always-indent 'complete
      tab-first-completion 'word

      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t

      enable-recursive-minibuffers t   ; Allow minibuffer commands in minibuffer
      read-answer-short t           ; also check `use-short-answers' for Emacs28
      resize-mini-windows t         ; Not sure what this does
      minibuffer-eldef-shorten-default t) ; Shorten "(default ...)" to "[...]" in minibuffer prompts.
(setq-default case-fold-search t)         ; For general regexp
(setq minibuffer-prompt-properties        ; Don't show cursor in the minibuffer
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Add prompt indicator to `completing-read-multiple'. We display
;; [CRM<separator>], e.g., [CRM,] if the separator is a comma. Taken from
;; https://github.com/minad/vertico
(defun crm-indicator (args)
  (cons (format "[completing-read-multiple: %s]  %s"
                (propertize
                 (replace-regexp-in-string
                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                  crm-separator)
                 'face 'error)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;; Prescient
;;;; Itself
;; Sorting and filtering of minibuffer candidates. The difference between
;; `orderless' and this package is that `orderless' filters but does not sort -
;; it leaves that up to the "candidate source and the completion UI."
;; Additionally, `orderless' has style "dispatchers," i.e., I can define
;; predicates for what filtering style to use for which token
(use-package prescient
  :disabled
  :demand
  :custom
  ;; NOTE 2024-02-03: Flex is chosen as a backup in case nothing in prescient is
  ;; matched, which only happens if I'm clueless about what I'm searching for.
  ;; We prefer this over adding the fuzzy matching in `prescient-filter-method'
  ;; because we don't want a bunch of random results included in the filtered
  ;; prescient results and cluttering it
  (completion-styles '(prescient flex))
  (prescient-filter-method
   '(literal initialism regexp))
  (prescient-sort-full-matches-first t)
  (prescient-history-length 200)
  (prescient-aggressive-file-save t)
  :config
  (prescient-persist-mode))

;;;; Vertico-prescient
(use-package vertico-prescient
  :after (prescient vertico)
  :custom
  (vertico-prescient-enable-filtering t)
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil) ; Keep it up to vertico
  (vertico-prescient-completion-styles '(prescient flex))
  ;; See also `prescient--completion-recommended-overrides'
  (vertico-prescient-completion-category-overrides
   '((file (styles basic basic-remote partial-completion))
     (eglot (styles prescient flex))))
  :init
  (vertico-prescient-mode))

;;;; Corfu-prescient
(use-package corfu-prescient
  :after (prescient corfu)
  :custom
  (corfu-prescient-enable-filtering t)
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting t)
  (corfu-prescient-completion-styles '(prescient flex))
  ;; See also `prescient--completion-recommended-overrides'
  (corfu-prescient-completion-category-overrides
   '((file (styles basic basic-remote partial-completion))
     (eglot (styles prescient flex))))
  :init
  (corfu-prescient-mode))

;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-field-width 80)
  (marginalia-align-offset -2)          ; Two to the left
  :init
  (marginalia-mode))

;;; completion-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-general-rcp)
