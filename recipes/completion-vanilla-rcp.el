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
(setq minibuffer-prompt-properties        ; Don't show cursor in the minibuffer
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Vertico
;;;; Itself
(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :general
  ("H-." #'vertico-repeat)
  (:keymaps 'vertico-map
   "<tab>" #'vertico-insert ; Set manually otherwise setting `vertico-quick-insert' overrides this
   "<escape>" #'minibuffer-keyboard-quit
   "?" #'minibuffer-completion-help
   "C-M-n" #'vertico-next-group
   "C-M-p" #'vertico-previous-group
   ;; Multiform toggles
   "<backspace>" #'vertico-directory-delete-char
   "C-w" #'vertico-directory-delete-word
   "C-<backspace>" #'vertico-directory-delete-word
   "RET" #'vertico-directory-enter
   "C-i" #'vertico-quick-insert
   "C-o" #'vertico-quick-exit
   "M-o" #'kb/vertico-quick-embark
   "M-G" #'vertico-multiform-grid
   "M-F" #'vertico-multiform-flat
   "M-R" #'vertico-multiform-reverse
   "M-U" #'vertico-multiform-unobtrusive
   "C-l" #'kb/vertico-multiform-flat-toggle)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save)) ; Make sure vertico state is saved
  :custom
  (vertico-count 13)
  (vertico-resize t)
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
  (vertico-multiform-mode)              ; Extensions

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
                                          (lambda (orig cand prefix suffix index _start)
                                            (setq cand (funcall orig cand prefix suffix index _start))
                                            (concat
                                             (if (= vertico--index index)
                                                 (propertize "» " 'face 'vertico-current)
                                               "  ")
                                             cand))))

;;;; Vertico-grid
(use-package vertico-grid
  :custom
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50))

;;;; Vertico-buffer
(use-package vertico-buffer
  :custom
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;;; Vertico-multiform
;; Extensions
(use-package vertico-multiform
  :custom
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg))))

;;;; Vertico-truncate
;; Truncate long lines while leaving match visible
(use-package vertico-truncate
  :after vertico
  :straight (:type git
             :host github
             :repo "jdtsmith/vertico-truncate")
  :config
  (vertico-truncate-mode))

;;; Selectrum
;;;; Itself
;; Advanced complete-read
(use-package selectrum
  :disabled t                           ; Trying out `vertico'
  :custom
  (selectrum-num-candidates-displayed 'auto)
  (selectrum-max-window-height 10)                 ; Maximum candidates shown
  (selectrum-fix-vertical-window-height t)         ; Fixed height?
  (selectrum-extend-current-candidate-highlight t) ; Highlight entire line
  (selectrum-count-style 'current/matches)
  (selectrum-show-indices nil)
  :init
  (selectrum-mode)

  ;; Optional performance optimization for `selectrum' by highlighting only the
  ;; visible candidates.
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)
        selectrum-highlight-candidates-function #'orderless-highlight-matches))

;;;; Selectrum-prescient
;; Selectrum with `prescient' completion style
(use-package selectrum-prescient
  :after prescient
  :ghook 'selectrum-mode-hook
  :custom
  ;; Use `prescient' to sort and filter in `selectrum-mode' This can be nil if
  ;; with `orderless' and want that to be the one filtering.
  ;; NOTE `Prescient' and `orderless' can both work with `selectrum'
  ;; simultaneously.
  (selectrum-prescient-enable-filtering t)
  (selectrum-prescient-enable-sorting t))

;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :custom
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles . (basic
                      basic-remote ; For `tramp' hostname completion with `vertico'
                      partial-completion  ; Partial completion for file paths!
                      orderless)))
     ;; NOTE 2023-01-14: These are taken from Prot's config
     (project-file (styles . (basic substring partial-completion orderless)))
     (imenu (styles . (basic substring orderless)))
     (kill-ring (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))))
  (completion-styles '(orderless flex))

  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-initialism
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     kb/orderless-without-literal-dispatcher))
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
;;;; Flx-rs
(use-package flx-rs
  :straight (flx-rs :repo "jcs-elpa/flx-rs" :fetcher github :files (:defaults "bin"))
  :commands flx-rs-score
  :config (flx-rs-load-dyn))

;;;; Liquidmetal
(use-package liquidmetal
  :commands fussy-liquidmetal-score)

;;;; Fuz-bin
(use-package fuz-bin
  :straight (fuz-bin :repo "jcs-elpa/fuz-bin" :fetcher github :files (:defaults "bin"))
  :commands fussy-fuz-score
  :config (fuz-bin-load-dyn))

;;;; Fuz-native
(use-package fzf-native
  :straight (fzf-native :repo "dangduc/fzf-native" :host github :files (:defaults "bin"))
  :commands fussy-fzf-native-score
  :config (fzf-native-load-dyn))

;;;; Subline-fuzzy
(use-package sublime-fuzzy
  :straight (sublime-fuzzy :repo "jcs-elpa/sublime-fuzzy" :fetcher github :files (:defaults "bin"))
  :commands fussy-sublime-fuzzy-score
  :config (sublime-fuzzy-load-dyn))

;;;; Hotfuzz
(use-package hotfuzz
  :commands fussy-hotfuzz-score)

;;;; Itself
(use-package fussy
  :disabled              ; Less performant than `orderless' with little benefit
  :straight (fussy :type git :host github :repo "jojojames/fussy")
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
   )
  )

;;; completion-vanilla-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-vanilla-rcp)
