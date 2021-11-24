;;; completion-default-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Selectrum completion framework and its cousin packages.
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
(use-package vertico
  :demand t
  :general
  (:keymaps 'global-map
            "M-r" #'vertico-repeat
            )
  (:keymaps 'vertico-map
            "?" #'minibuffer-completion-help
            "C-<return>" #'vertico-quick-insert
            "M-<return>" #'vertico-quick-exit
            )
  :custom
  ;; Workaround for problem with `org-refile'. See
  ;; https://github.com/minad/vertico#org-refile
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  :init
  (load (concat user-emacs-directory "straight/build/vertico/extensions/vertico-quick.el"))
  (load (concat user-emacs-directory "straight/build/vertico/extensions/vertico-repeat.el"))
  (load (concat user-emacs-directory "straight/build/vertico/extensions/vertico-indexed.el"))
  :config
  (vertico-mode)

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
  )

;;; Selectrum
;; Advanced complete-read
(use-package selectrum
  :disabled t                           ; Trying out `vertico'
  :demand t
  :custom
  (selectrum-num-candidates-displayed 'auto)
  (selectrum-max-window-height 10)                 ; Maximum candidates shown
  (selectrum-fix-vertical-window-height t)         ; Fixed height?
  (selectrum-extend-current-candidate-highlight t) ; Highlight entire line
  (selectrum-count-style 'current/matches)
  (selectrum-show-indices nil)
  :config
  (selectrum-mode)

  ;; Optional performance optimization for `selectrum' by highlighting only the
  ;; visible candidates.
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)
        selectrum-highlight-candidates-function #'orderless-highlight-matches)
  )

;;;; Selectrum-prescient
;; Selectrum with `prescient' completion style
(use-package selectrum-prescient
  :demand t
  :after prescient
  :ghook 'selectrum-mode-hook
  :custom
  ;; Use `prescient' to sort and filter in `selectrum-mode' This can be nil if
  ;; with `orderless' and want that to be the one filtering.
  ;; NOTE `Prescient' and `orderless' can both work with `selectrum'
  ;; simultaneously.
  (selectrum-prescient-enable-filtering t)
  (selectrum-prescient-enable-sorting t)
  )

;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles . (basic-remote ; For `tramp' hostname completion with `vertico'
                      partial-completion ; Kinda like initialism for directory/file names
                      orderless          ; Of course, default to `orderless'
                      )))))

  (orderless-component-separator   ; What separates components
   ;; " +"                            ; Default
   ;; 'split-string-and-unquote       ; "for shell-like double-quotable space"
   'orderless-escapable-split-on-space  ; Use backslash for literal space
   )
  (orderless-matching-styles
   '(orderless-literal
     orderless-initialism
     orderless-prefixes
     orderless-regexp
     orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instaed
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher    ; = suffix for literal
     prot-orderless-initialism-dispatcher ; , suffix for initialism
     prot-orderless-flex-dispatcher       ; ~ suffix for flex
     ))
  :init
  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  )

;;; completion-default-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-default-rcp)
