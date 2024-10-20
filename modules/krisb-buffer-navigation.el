;;; Isearch
;; Incremental search
(use-package isearch
  :ensure nil
  :custom
  (isearch-repeat-on-direction-change t)
  (isearch-allow-scroll t)
  (isearch-allow-motion t)
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no)
  ;; Make regular Isearch interpret the empty space as a regular expression that
  ;; matches any character between the words you give it. Learned from
  ;; Protesilaos. Also be aware of `isearch-toggle-lax-whitespace'
  (isearch-lax-whitespace t)
  (search-whitespace-regexp ".*?"))

;;; Imenu
(use-package imenu
  :ensure nil
  :custom
  (org-imenu-depth 7)                   ; Show more than just 2 levels...
  (imenu-auto-rescan t)
  (use-package-enable-imenu-support t)
  (imenu-flatten 'group))

;;; Puni
;; Major-mode agnostic structural editing, faithful to built-ins
(use-package puni
  :custom
  (puni-confirm-when-delete-unbalanced-active-region t)
  :config
  (setq puni-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-d") 'puni-forward-kill-word)
          (define-key map (kbd "M-DEL") 'puni-backward-kill-word)
          (define-key map [remap kill-line] 'puni-kill-line)
          (define-key map [remap backward-sexp] 'puni-backward-sexp)
          (define-key map [remap forward-sexp] 'puni-forward-sexp)
          (define-key map [remap beginning-of-defun] 'puni-beginning-of-sexp)
          (define-key map [remap end-of-defun] 'puni-end-of-sexp)
          (define-key map [remap backward-list] 'puni-backward-sexp-or-up-list)
          (define-key map [remap forward-list] 'puni-forward-sexp-or-up-list)
          (define-key map (kbd "C-M-9") 'puni-syntactic-backward-punct)
          (define-key map (kbd "C-M-0") 'puni-syntactic-forward-punct)
          (define-key map (kbd "C-M-r") 'puni-raise)
          (define-key map (kbd "C-M-=") 'puni-splice)
          (define-key map (kbd "C-M-S-o") 'puni-split)
          (define-key map (kbd "C-M-[") 'puni-slurp-backward)
          (define-key map (kbd "C-M-]") 'puni-slurp-forward)
          (define-key map (kbd "C-M-{") 'puni-barf-backward)
          (define-key map (kbd "C-M-}") 'puni-barf-forward)
          map))
  (puni-global-mode 1))

;;; Provide
(provide 'krisb-buffer-navigation)
