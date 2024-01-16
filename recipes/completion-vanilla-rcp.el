;;; completion-vanilla-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Completion framework and cousin packages which are lightweight and faithful
;; to the base Emacs architecture.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Built-in
;; A lot of this is taken from
;; https://protesilaos.com/dotemacs/#h:c110e399-3f43-4555-8427-b1afe44c0779
(setq completion-styles '(basic initials partial-completion flex)
      completion-cycle-threshold 2 ; Number of candidates until cycling turns off
      completion-flex-nospace t
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

;;; Vertico
;;;; Itself
(use-package vertico
  :demand                               ; Otherwise won't get loaded immediately
  :elpaca (vertico :files (:defaults "extensions/*"))
  :general
  ("H-." #'vertico-repeat)
  (:keymaps 'vertico-map
            "<escape>" #'minibuffer-keyboard-quit
            "?" #'minibuffer-completion-help
            "C-M-n" #'vertico-next-group
            "C-M-p" #'vertico-previous-group
            "M-o" #'kb/vertico-quick-embark)
  :hook (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
  :custom
  (vertico-count 13)
  (vertico-resize 'grow-only)
  (vertico-cycle nil)
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
  :config
  (vertico-mode)

  (defun kb/vertico-quick-embark (&optional _)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump))))

;;;; Vertico-directory
(use-package vertico-directory
  :elpaca nil
  ;; More convenient directory navigation commands
  :general (:keymaps 'vertico-map
                     "RET" 'vertico-directory-enter
                     "DEL" 'vertico-directory-delete-char
                     "M-DEL" 'vertico-directory-delete-word)
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;; Vertico-multiform
(use-package vertico-multiform
  :demand
  :after vertico
  :elpaca nil
  :custom
  (vertico-multiform-categories
   '((consult-grep buffer)
     (imenu buffer)))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid (vertico-grid-annotate . 20))))
  :config
  (vertico-multiform-mode))

;;;; Vertico-buffer
(use-package vertico-buffer
  :elpaca nil
  :after vertico
  :custom
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;;; Vertico-truncate
;; Truncate long lines while leaving match visible
(use-package vertico-truncate
  :demand
  :after vertico
  :elpaca (:type git
                 :host github
                 :repo "jdtsmith/vertico-truncate")
  :config
  (vertico-truncate-mode))

;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :custom
  (completion-styles '(basic initials substring flex orderless))
  (orderless-matching-styles
   '(orderless-prefixes
     ;; orderless-initialism
     orderless-regexp
     ;; orderless-literal
     ;; orderless-flex
     ;; orderless-strict-initialism
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     kb/orderless-without-literal-dispatcher))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles . (basic
                      basic-remote ; For `tramp' hostname completion with `vertico'
                      partial-completion  ; Partial completion for file paths!
                      orderless)))))
  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun kb/orderless-without-literal-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 0 -1)))))

;;; Fussy
;; Instead of just filtering (e.g. like `orderless' alone), also score the
;; filtered candidates afterward!
;;;; Itself
(use-package fussy
  :disabled               ; Less performant than `orderless' with little benefit
  :elpaca (fussy :type git :host github :repo "jojojames/fussy")
  :commands fussy-all-completions fussy-try-completions
  :custom
  (completion-styles '(fussy orderless flex))

  (fussy-max-candidate-limit 100)     ; Score only the top N shortest candidates
  (fussy-compare-same-score-fn 'fussy-histlen->strlen<)

  (orderless-matching-styles '(orderless-initialism orderless-regexp))
  :config
  (setq
   fussy-filter-fn 'fussy-filter-fast   ; See `fussy-fast-regex-fn'
   fussy-filter-fn 'fussy-filter-orderless
   )
  (setq
   fussy-score-fn 'fussy-sublime-fuzzy-score ; Doesn't work with orderless components
   fussy-score-fn 'fussy-liquidmetal-score
   fussy-score-fn 'fussy-hotfuzz-score
   fussy-score-fn 'fussy-fzf-native-score
   fussy-score-fn 'fussy-fuz-bin-score
   fussy-score-fn 'flx-rs-score
   ))

;;;; Flx-rs
(use-package flx-rs
  :elpaca (flx-rs :repo "jcs-elpa/flx-rs" :fetcher github :files (:defaults "bin"))
  :after flx-rs
  :commands fussy-score
  :config (flx-rs-load-dyn))

;;;; Liquidmetal
(use-package liquidmetal
  :after fussy
  :commands fussy-liquidmetal-score)

;;;; Fuz-bin
(use-package fuz-bin
  :elpaca (fuz-bin :repo "jcs-elpa/fuz-bin" :fetcher github :files (:defaults "bin"))
  :after fussy
  :commands fussy-fuz-score
  :config (fuz-bin-load-dyn))

;;;; Fuz-native
(use-package fzf-native
  :elpaca (fzf-native :repo "dangduc/fzf-native" :host github :files (:defaults "bin"))
  :after fussy
  :commands fussy-fzf-native-score
  :config (fzf-native-load-dyn))

;;;; Subline-fuzzy
(use-package sublime-fuzzy
  :elpaca (sublime-fuzzy :repo "jcs-elpa/sublime-fuzzy" :fetcher github :files (:defaults "bin"))
  :after fussy
  :commands fussy-sublime-fuzzy-score
  :config (sublime-fuzzy-load-dyn))

;;;; Hotfuzz
(use-package hotfuzz
  :after fussy
  :commands fussy-hotfuzz-score)

;;; completion-vanilla-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-vanilla-rcp)
