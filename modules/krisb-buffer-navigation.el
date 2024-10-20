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

;;; Avy
;; Quickly jump to any character
(use-package avy
  :commands krisb-avy-goto-parens
  :bind (("C-; C-;" . avy-goto-char-timer)
         ("C-; s" . avy-goto-symbol-1)
         ("C-; l" . avy-goto-line)
         ("C-; p" . krisb-avy-goto-parens))
  :custom
  (avy-all-windows t)                   ; Scope
  (avy-case-fold-search nil)
  (avy-single-candidate-jump t)
  (avy-timeout-seconds 0.3)
  (avy-style 'at-full)
  (avy-keys '(?a ?w ?r ?u ?i ?o ?p))
  (avy-dispatch-alist ; Avy actions (first narrow so letter combinations appear)
   '((?k . avy-action-kill-stay)
     (?K . avy-action-kill-move)
     (?t . avy-action-teleport)
     (?m . avy-action-mark)
     (?y . avy-action-yank)
     (?z . avy-action-zap-to-char)
     (?. . krisb-avy-action-embark)
     (?h . krisb-avy-action-help)
     (?d . krisb-avy-action-define)
     (?e . krisb-avy-action-eval)))
  (avy-orders-alist
   '((avy-goto-char . krisb-avy-order-farthest)
     (avy-goto-char-2 . krisb-avy-order-farthest)
     (avy-goto-word-0 . krisb-avy-order-farthest)
     (avy-goto-word-1 . krisb-avy-order-farthest)
     (avy-goto-char-timer . krisb-avy-order-farthest)
     (krisb-avy-goto-parens . krisb-avy-order-farthest)))
  :config
  (krisb-modus-themes-setup-faces
   "avy"
   ;; Don't bold so text isn't shifted much
   (set-face-attribute 'avy-lead-face nil :inherit 'modus-themes-reset-soft)
   (set-face-attribute 'avy-lead-face-0 nil :inherit 'modus-themes-reset-soft)
   (set-face-attribute 'avy-lead-face-1 nil :inherit 'modus-themes-reset-soft)
   (set-face-attribute 'avy-lead-face-2 nil :inherit 'modus-themes-reset-soft))

  (defun krisb-avy-order-farthest (x)
    (- (abs (- (if (numberp (car x))
                   (car x)
                 (caar x))
               (point)))))

  ;; Taken from the avy wiki
  (defun krisb-avy-goto-parens ()
    "Go to an open or close parens."
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump (rx (+ (or (literal "(") (literal ")")))))))

  ;; Additional avy dispatch actions. Most are inspired or taken from
  ;; https://karthinks.com/software/avy-can-do-anything/
  ;; Embark
  (defun krisb-avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Helpful
  (defun krisb-avy-action-help (pt)
    (save-excursion
      (goto-char pt)
      (if (featurep 'helpful)
          (helpful-at-point)
        (describe-symbol (symbol-at-point))))
    (when (featurep 'helpful)
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Dictionary
  (defun krisb-avy-action-define (pt)
    (require 'checking-words-rcp)
    (save-excursion
      (goto-char pt)
      (krisb-dictionary-at-point))
    ;; If with `universal-arg', don't switch to help buffer
    (when current-prefix-arg
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Evaluation
  (defun krisb-avy-action-eval (pt)
    (save-excursion
      (goto-char pt)
      (if (fboundp 'eros-eval-last-sexp)
          (call-interactively 'eros-eval-last-sexp)
        (call-interactively 'eval-last-sexp)))
    t))

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
