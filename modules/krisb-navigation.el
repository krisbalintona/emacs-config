;;; Puni
;; Major-mode agnostic structural editing, faithful to built-ins
(use-package puni
  :bind (([remap kill-word] . puni-forward-kill-word)
         ([remap backward-kill-word] . puni-backward-kill-word)
         ([remap kill-line] . puni-kill-line)
         ([remap backward-sexp] . puni-backward-sexp)
         ([remap forward-sexp] . puni-forward-sexp)
         ([remap beginning-of-defun] . puni-beginning-of-sexp)
         ([remap end-of-defun] . puni-end-of-sexp)
         ([remap backward-list] . puni-backward-sexp-or-up-list)
         ([remap forward-list] . puni-forward-sexp-or-up-list)
         ("C-M-9" . puni-syntactic-backward-punct)
         ("C-M-0" . puni-syntactic-forward-punct)
         ("C-M-r" . puni-raise)
         ("C-M-=" . puni-splice)
         ("C-M-S-o" . puni-split)
         ("C-M-[" . puni-slurp-backward)
         ("C-M-]" . puni-slurp-forward)
         ("C-M-{" . puni-barf-backward)
         ("C-M-}" . puni-barf-forward))
  :custom
  (puni-confirm-when-delete-unbalanced-active-region t))

;;; Consult
;; Counsel equivalent for default Emacs completion. It provides many useful
;; commands.
(use-package consult
  :bind (("C-x B" . consult-buffer)
         ;; Remaps
         ([remap bookmark-jump] . consult-bookmark)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap Info-search] . consult-info)
         ([remap point-to-register] . consult-register-store)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap imenu] . consult-imenu)
         ([remap flymake-show-buffer-diagnostics] . consult-flymake)
         :map consult-narrow-map
         ("?" . consult-narrow-help)          ; Show available narrow keys
         :map goto-map                  ; The `M-g' prefix
         ("f" . consult-flymake)
         ("o" . consult-outline)
         ("e" . consult-compile-error)
         ("l" . consult-line)
         :map search-map                ; The `M-s' prefix
         ("i" . consult-info)
         ("g" . consult-git-grep)
         ("G" . consult-grep)
         ("r" . consult-ripgrep)
         ("f" . consult-find)
         ("F" . consult-locate)
         :map org-mode-map
         ([remap consult-outline] . consult-org-heading)
         ("M-g a" . consult-org-agenda))
  :custom
  (consult-preview-key "C-M-;")
  (consult-bookmark-narrow
   '((?f "File" bookmark-default-handler)
     (?i "Info" Info-bookmark-jump)
     (?h "Help" help-bookmark-jump Info-bookmark-jump
         Man-bookmark-jump woman-bookmark-jump)
     (?p "PDFs" pdf-view-bookmark-jump-handler)
     (?a "Activities" activities-bookmark-handler)
     (?d "Docview" doc-view-bookmark-jump)
     (?s "Eshell" eshell-bookmark-jump)
     (?w "Web" eww-bookmark-jump xwidget-webkit-bookmark-jump-handler)
     (?v "VC Directory" vc-dir-bookmark-jump)
     (nil "Other")))
  (consult-ripgrep-args
   (concat
    "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --search-zip"
    ;; Additional args
    " --line-number --hidden"))
  :config
  (require 'krisb-consult-ext)

  ;; Log-edit history
  (add-to-list 'consult-mode-histories
               '(log-edit-mode log-edit-comment-ring log-edit-comment-ring-index log-edit-beginning-of-line))

  ;; Use the faster plocate rather than locate
  (when (executable-find "plocate")
    (setopt consult-locate-args "plocate --ignore-case --existing --regexp"))

  ;; Use consult UI with xref
  (with-eval-after-load 'xref
    ;; Use Consult to select xref locations with preview
    (setopt xref-show-definitions-function #'consult-xref
            xref-show-xrefs-function #'consult-xref))

  ;; Registers
  (with-eval-after-load 'register
    ;; Fancier formatting of preview
    (setopt register-preview-function #'consult-register-format)
    ;; Fancier formatting of preview window. Adds thin lines, sorting and hides
    ;; the mode line of the register preview window. Copied from
    ;; https://github.com/minad/consult#use-package-example
    (advice-add 'register-preview :override #'consult-register-window))

  ;; Pulsar pulses
  (with-eval-after-load 'pulsar
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)))

;;; Jump
;;;; Isearch
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

;;;; Imenu
(use-package imenu
  :ensure nil
  :custom
  (org-imenu-depth 7)                   ; Show more than just 2 levels...
  (imenu-auto-rescan t)
  (use-package-enable-imenu-support t)
  (imenu-flatten 'group)
  :config
  (with-eval-after-load 'pulsar
    (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)))

;;;; Avy
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

;;;; Grep
(use-package grep
  :custom
  (grep-save-buffers 'ask)
  (grep-use-headings t))

;;;; Recentf
;; Enable logging of recent files
(use-package recentf
  :demand t
  :ensure nil
  :bind ( :map krisb-file-keymap
          ("r" . recentf-open-files))
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  :config
  (recentf-mode 1))

;;; Folding
;;;; Outline
(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-highlight t)
  (outline-blank-line t))

;;;; Outshine
;; Outline-minor-mode but with better keybindings and more support.
(use-package outshine
  :diminish outshine-mode
  :hook ((LaTeX-mode prog-mode conf-mode) . outshine-mode)
  :bind ( :map outshine-mode-map
          ("C-x n s". outshine-narrow-to-subtree)
          :map diff-mode-map
          ("S-<iso-lefttab>" . outshine-cycle-buffer)
          ("<tab>" . outshine-cycle)
          ("C-x n s" . outshine-narrow-to-subtree))
  :custom
  (outshine-use-speed-commands t))

;;; Provide
(provide 'krisb-navigation)
