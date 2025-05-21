;; -*- lexical-binding: t; -*-

;;; Jump

;;;; Intra-file

;;;;; Occur
(use-package replace
  :ensure nil
  :config
  (with-eval-after-load 'krisb-reveal
    (defun kris-reveal-occur-find-information ()
      "Return information required by `krisb-reveal-fold-commands'.
See the docstring of `krisb-reveal-fold-commands'."
      (save-window-excursion
        (save-excursion
          (occur-mode-goto-occurrence)
          (cons (point) (current-buffer)))))
    (dolist (command '(next-error-no-select
                       previous-error-no-select
                       occur-mode-display-occurrence
                       occur-mode-goto-occurrence
                       occur-mode-goto-occurrence-other-window))
      (add-to-list 'krisb-reveal-fold-commands
                   (list :command command
                         :location #'kris-reveal-occur-find-information
                         :predicate (lambda () (eq major-mode 'occur-mode)))))))

;;;;; Avy
;; Quickly jump to any character.  For a great resource on the power of avy, see
;; https://karthinks.com/software/avy-can-do-anything/.
(use-package avy
  :commands krisb-avy-goto-parens
  :bind (("C-; C-;" . avy-goto-char-timer)
         ("C-; s" . avy-goto-symbol-1)
         ("C-; l" . avy-goto-line)
         ("C-; p" . krisb-avy-goto-parens))
  :custom
  ;; For descriptions of all the options, see the avy wiki:
  ;; https://github.com/abo-abo/avy/wiki/defcustom
  (avy-all-windows t)
  (avy-case-fold-search nil)
  (avy-single-candidate-jump nil)
  (avy-timeout-seconds 0.3)
  (avy-keys '(?a ?w ?r ?u ?i ?o ?p))
  (avy-background nil)
  (avy-highlight-first t)
  ;; 2025-04-03: For a description of the styles see
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style.  The primary
  ;; considerations are readability with many candidates and ability see and
  ;; input the sequences quickly.  I typically favor the speedy input of
  ;; sequences over readability.  So, I set a default to 'words but for commands
  ;; that have many candidates I prefer 'at (which shows single characters only;
  ;; this is good for readability but unfavorable for speed).  (Note: The 'pre
  ;; and 'post options make the text shift.)
  (avy-style 'words)
  (avy-styles-alist
   '((krisb-avy-goto-parens . at)))
  ;; Alist of avy actions.  See the "Remembering to Avy" section in in
  ;; https://karthinks.com/software/avy-can-do-anything/ for the rationale
  ;; behind the bindings below
  (avy-dispatch-alist
   '((?k . avy-action-kill-stay)
     (?K . krisb-avy-action-kill-whole-line)
     (?t . avy-action-teleport)
     (?T . krisb-avy-action-teleport-whole-line)
     (?w . avy-action-copy)
     (?W . krisb-avy-action-copy-whole-line)
     (?y . avy-action-yank)
     (?Y . krisb-avy-action-yank-whole-line)
     (?m . avy-action-mark)
     (?z . avy-action-zap-to-char)
     (?. . krisb-avy-action-embark)
     (?h . krisb-avy-action-help)
     (?= . krisb-avy-action-dictionary)
     (?e . krisb-avy-action-eval)
     (?w . krisb-avy-action-kill-ring-save)))
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
   ;; By default, modus-themes inherits from the bold face.  But this is
   ;; undesirable for variable-pitch faces since it causes the text to shift.
   ;; To avoid this, we set the inherited face attribute to the default with
   ;; bold removed.
   (set-face-attribute 'avy-lead-face nil :inherit '(modus-themes-search-current modus-themes-reset-soft))
   (set-face-attribute 'avy-lead-face-0 nil :inherit '(modus-themes-search-current modus-themes-reset-soft))
   (set-face-attribute 'avy-lead-face-1 nil :inherit '(modus-themes-search-current modus-themes-reset-soft))
   (set-face-attribute 'avy-lead-face-2 nil :inherit '(modus-themes-search-current modus-themes-reset-soft)))

  (defun krisb-avy-order-farthest (x)
    (- (abs (- (if (numberp (car x))
                   (car x)
                 (caar x))
               (point)))))

  ;; Goto (i.e. selection) commands
  (defun krisb-avy-goto-parens ()
    "Go to an open or close parens."
    (interactive)
    (let ((avy-command this-command))
      (avy-with krisb-avy-goto-parens
        ;; There will be many candidates if called in an e.g. elisp buffer.  The
        ;; solution is changing the `avy-style' appropriately, either by setting
        ;; `avy-style' directly or `avy-styles-alist'.
        (avy-jump (rx (or "(" ")")))
        ;; When jumping to a closing parens, move the point forward one
        ;; character, since in these cases, I want the point to end after the
        ;; parens, not before
        (when (eq (char-after) (string-to-char ")"))
          (forward-char))))
    t)

  ;; Actions
  (defun krisb-avy-action-help (pt)
    "Help avy action."
    (save-excursion
      (goto-char pt)
      (if (featurep 'helpful)
          (helpful-at-point)
        (describe-symbol (symbol-at-point))))
    (when (featurep 'helpful)
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Additional avy dispatch actions. Most are inspired or taken from
  ;; https://karthinks.com/software/avy-can-do-anything/
  ;; Embark
  (defun krisb-avy-action-embark (pt)
    "Embark avy action."
    (require 'embark)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun krisb-avy-action-dictionary (pt)
    "Dictionary avy action."
    (save-excursion
      (goto-char pt)
      (krisb-dictionary-dwim))
    ;; If with `universal-arg', don't switch to help buffer
    (when current-prefix-arg
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun krisb-avy-action-eval (pt)
    "Eval avy action."
    (save-excursion
      (goto-char pt)
      (if (fboundp 'eros-eval-last-sexp)
          (call-interactively 'eros-eval-last-sexp)
        (call-interactively 'eval-last-sexp)))
    t)

  (defun krisb-avy-action-kill-whole-line (pt)
    "Kill whole line avy action.
Taken from https://karthinks.com/software/avy-can-do-anything/."
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun krisb-avy-action-teleport-whole-line (pt)
    "Teleport whole line avy action.
Taken from https://karthinks.com/software/avy-can-do-anything/."
    (krisb-avy-action-kill-whole-line pt)
    (save-excursion (yank) t))

  (defun krisb-avy-action-copy-whole-line (pt)
    "Copy whole line avy action.
Taken from https://karthinks.com/software/avy-can-do-anything/."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun krisb-avy-action-yank-whole-line (pt)
    "Yank whole line avy action.
Taken from https://karthinks.com/software/avy-can-do-anything/."
    (krisb-avy-action-copy-whole-line pt)
    (save-excursion (yank) t)))

;;;; Inter-file

;;; Provide
(provide 'krisb-navigation)
