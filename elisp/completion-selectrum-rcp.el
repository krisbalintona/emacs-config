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

;;; completion-selectrum-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-selectrum-rcp)
