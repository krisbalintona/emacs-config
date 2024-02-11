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

;;; Vertico
;;;; Itself
(use-package vertico
  :demand                               ; Otherwise won't get loaded immediately
  :ensure (vertico :files (:defaults "extensions/*"))
  :general
  ("H-." #'vertico-repeat)
  (:keymaps 'vertico-map
            "<escape>" #'minibuffer-keyboard-quit
            "?" #'minibuffer-completion-help)
  :hook (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
  :custom
  (vertico-count 13)
  ;; (vertico-resize 'grow-only)
  (vertico-resize t)
  (vertico-cycle nil)
  :config
  (vertico-mode)

  ;; Special for `org-agenda-filter' and `org-tags-view'. See
  ;; https://github.com/minad/vertico?tab=readme-ov-file#org-agenda-filter-and-org-tags-view
  (defun kb/org-enforce-basic-completion (&rest args)
    (minibuffer-with-setup-hook
        (:append
         (lambda ()
           (let ((map (make-sparse-keymap)))
             (define-key map [tab] #'minibuffer-complete)
             (use-local-map (make-composed-keymap (list map) (current-local-map))))
           (setq-local completion-styles (cons 'basic completion-styles)
                       vertico-preselect 'prompt)))
      (apply args)))
  (advice-add #'org-make-tags-matcher :around #'kb/org-enforce-basic-completion)
  (advice-add #'org-agenda-filter :around #'kb/org-enforce-basic-completion)

  ;; Left-truncate recentf filename candidates (e.g. for `consult-buffer'). See
  ;; https://github.com/minad/vertico/wiki#left-truncate-recentf-filename-candidates-eg-for-consult-buffer
  (defun kb/vertico-truncate-candidates (args)
    (if-let ((arg (car args))
             (type (get-text-property 0 'multi-category arg))
             ((eq (car-safe type) 'file))
             (w (max 30 (- (window-width) 38)))
             (l (length arg))
             ((> l w)))
        (setcar args (concat "…" (truncate-string-to-width arg l (- l w)))))
    args)
  (advice-add #'vertico--format-candidate :filter-args #'kb/vertico-truncate-candidates)

  ;; Input at bottom of completion list. See
  ;; https://github.com/minad/vertico/wiki#input-at-bottom-of-completion-list
  (defun kb/vertico-bottom--display-candidates (lines)
    "Display LINES in bottom."
    (move-overlay vertico--candidates-ov (point-min) (point-min))
    (unless (eq vertico-resize t)
      (setq lines (nconc (make-list (max 0 (- vertico-count (length lines))) "\n") lines)))
    (let ((string (apply #'concat lines)))
      (add-face-text-property 0 (length string) 'default 'append string)
      (overlay-put vertico--candidates-ov 'before-string string)
      (overlay-put vertico--candidates-ov 'after-string nil))
    (vertico--resize-window (length lines)))
  (advice-add #'vertico--display-candidates :override #'kb/vertico-bottom--display-candidates))

;;;; Vertico-directory
(use-package vertico-directory
  :ensure nil
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
  :ensure nil
  :custom
  (vertico-multiform-categories
   '((consult-grep buffer)
     (imenu buffer)))
  (vertico-multiform-commands
   ;; I use jinx now, but I think it's better to not apply a grid layout to it
   ;; since its use of vertico-groups is useful
   '(("flyspell-correct-*" grid (vertico-grid-annotate . 20))))
  :config
  (vertico-multiform-mode))

;;;; Vertico-buffer
(use-package vertico-buffer
  :ensure nil
  :after vertico
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;;; Vertico-truncate
;; Truncate long lines while leaving match visible
(use-package vertico-truncate
  :demand
  :after vertico
  :ensure (:type git
                 :host github
                 :repo "jdtsmith/vertico-truncate")
  :config
  (vertico-truncate-mode))

;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :custom
  (completion-styles
   '(initials substring orderless basic flex))
  (orderless-matching-styles
   '(orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-literal
     ;; orderless-flex
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  ;; Sets many defaults unfavorable to `orderless', so I set it to nil to use
  ;; just the default `completion-styles'
  (completion-category-defaults nil)
  ;; Overrides `completion-category-defaults'
  (completion-category-overrides
   '((file (styles . (basic
                      basic-remote ; For `tramp' hostname completion with `vertico'
                      orderless
                      partial-completion
                      flex)))))
  (orderless-style-dispatchers '(kb/orderless-consult-dispatch))
  :init
  ;; Taken from Doom
  (defun kb/orderless-consult-dispatch (pattern _index _total)
    "Basically `orderless-affix-dispatch-alist' but with prefixes too."
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1))))))

;;; Fussy
;; Instead of just filtering (e.g. like `orderless' alone), also score the
;; filtered candidates afterward!
;;;; Itself
(use-package fussy
  :disabled               ; Less performant than `orderless' with little benefit
  :ensure (fussy :type git :host github :repo "jojojames/fussy")
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
  :ensure (flx-rs :repo "jcs-elpa/flx-rs" :fetcher github :files (:defaults "bin"))
  :after flx-rs
  :commands fussy-score
  :config (flx-rs-load-dyn))

;;;; Liquidmetal
(use-package liquidmetal
  :after fussy
  :commands fussy-liquidmetal-score)

;;;; Fuz-bin
(use-package fuz-bin
  :ensure (fuz-bin :repo "jcs-elpa/fuz-bin" :fetcher github :files (:defaults "bin"))
  :after fussy
  :commands fussy-fuz-score
  :config (fuz-bin-load-dyn))

;;;; Fuz-native
(use-package fzf-native
  :ensure (fzf-native :repo "dangduc/fzf-native" :host github :files (:defaults "bin"))
  :after fussy
  :commands fussy-fzf-native-score
  :config (fzf-native-load-dyn))

;;;; Subline-fuzzy
(use-package sublime-fuzzy
  :ensure (sublime-fuzzy :repo "jcs-elpa/sublime-fuzzy" :fetcher github :files (:defaults "bin"))
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
