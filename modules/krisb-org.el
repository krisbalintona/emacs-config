;;; Built-in
;;;; Org
(use-package org
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode)
         (org-mode . (lambda () (setq-local line-spacing 0.2 fill-column 120))))
  :bind (("C-M-s-s" . org-store-link)
         :map krisb-note-keymap
         ("c" . org-capture))
  :custom
  (org-directory krisb-org-directory)

  ;; Headline appearance
  (org-hide-leading-stars nil)
  (org-n-level-faces 8)
  (org-cycle-separator-lines 2)
  (org-cycle-level-faces t)
  (org-ellipsis " ⮷")
  (org-startup-folded 'nofold)
  (org-tags-column 0)

  ;; Markup appearance
  (org-hide-emphasis-markers t)     ; Remove org-mode markup characters
  (org-hide-macro-markers nil)
  (org-pretty-entities t)           ; Show as UTF-8 characters (useful for math)
  (org-pretty-entities-include-sub-superscripts t) ; Show superscripts and subscripts? Also see `org-export-with-sub-superscripts'
  (org-use-sub-superscripts '{}) ; Requires brackets to recognize superscripts and subscripts
  (org-hidden-keywords nil)

  ;; Movement
  (org-special-ctrl-a/e t)
  (org-ctrl-k-protect-subtree 'error)

  ;; Plain lists
  (org-list-allow-alphabetical t)
  (org-list-use-circular-motion t)

  ;; Headline insertion
  (org-blank-before-new-entry
   '((heading . auto)
     ;; Don't let Emacs make decisions about where to insert newlines
     (plain-list-item . nil)))
  (org-insert-heading-respect-content nil) ; Let M-RET make heading in place
  (org-M-RET-may-split-line '((table . nil)
                              (default . t)))

  ;; Blocks
  (org-structure-template-alist
   '(("s" . "src")
     ("e" . "src emacs-lisp")
     ("E" . "src emacs-lisp :results value code :lexical t")
     ("t" . "src emacs-lisp :tangle FILENAME")
     ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
     ("x" . "example")
     ("X" . "export")
     ("v" . "verse")
     ("c" . "comment")
     ("q" . "quote")))

  ;; Other
  (org-file-apps
   '((directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default)
     ("\\.docx\\'" . system)
     ("\\.odt\\'" . system)
     ;; Default to `auto-mode-alist'
     (auto-mode . emacs)))
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-edit-timestamp-down-means-later t)
  :custom-face
  (org-quote ((t (:family ,(face-attribute 'variable-pitch :family) :extend t :inherit 'org-block))))
  (org-ellipsis ((t (:box unspecified :inherit default)))) ; Don't make line taller because of org-ellipsis
  :config
  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Pulsar pulses
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
      (add-hook hook #'pulsar-recenter-center)
      (add-hook hook #'pulsar-reveal-entry))))

;;;; Org-faces
(use-package org-faces
  :ensure nil
  :custom
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline nil)
  (org-fontify-whole-block-delimiter-line nil)
  (org-fontify-quote-and-verse-blocks t))

;;;; Org-src
(use-package org-src
  :ensure nil
  :custom
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-src-block-faces nil) ; Open src block window on current buffer were in the language's major mode
  (org-edit-src-turn-on-auto-save nil)
  (org-edit-src-auto-save-idle-delay 3))


;;;; Org-id
(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-clone-delete-id t)
  (org-id-method 'ts)
  (org-id-link-to-org-use-id 'use-existing))

;;;; Org-babel
(use-package ob
  :ensure nil
  :custom
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)))
  (org-confirm-babel-evaluate nil))

;;;;;; Org-modern
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-keyword nil)

  (org-modern-hide-stars "· ") ; Is affected by the value of `org-hide-leading-stars'
  (org-modern-star 'fold)
  (org-modern-fold-stars
   '(("▶" . "▼")
     ("▷" . "▽")
     ("⯈" . "⯆")
     ("▹" . "▿")
     ("▸" . "▾")))

  (org-modern-todo t) ; NOTE 2024-10-10: I set `org-modern-todo-faces' in my org-agenda section
  (org-modern-priority t)
  ;; See my value for `org-priority-faces'
  (org-modern-priority-faces
   '((?A :inverse-video t :inherit (bold org-priority))
     (?B :inverse-video t :inherit (bold org-priority))
     (?C :inverse-video t :inherit org-priority)
     (?D :inverse-video t :inherit org-priority)
     (?E :inverse-video t :inherit (shadow org-priority))
     (?F :inverse-video t :inherit (shadow org-priority))))
  ;; See my value for `org-todo-keyword-faces'
  (org-modern-todo-faces
   '(("NEXT" :inherit (bold success org-modern-todo))
     ("TODO" :inherit (org-todo org-modern-todo))
     ("HOLD" :inherit (shadow error org-modern-todo))
     ("MAYBE" :inherit (shadow org-todo org-modern-todo))
     ("DONE" :inherit (bold org-done org-modern-todo))
     ("CANCELED" :inherit (error org-modern-todo))))

  (org-modern-label-border 3)
  (org-modern-tag t)

  (org-modern-block-fringe nil) ; Doesn't work well with `olivetti-style' set to 'fancy
  (org-modern-block-name '("⌜" . "⌞"))

  (org-modern-footnote '(nil (raise 0.15) (height 0.9)))
  (org-modern-list '((?+ . "◦")
                     (?- . "–")
                     (?* . "•")))
  (org-modern-timestamp t)

  (org-modern-table t)
  (org-modern-table-vertical 3)
  (org-modern-table-horizontal 0.1)
  :custom-face
  (org-modern-label
   ((t :height 0.9 :width condensed :weight regular :underline nil)))
  (org-modern-todo ((t :weight semibold :inverse-video t :inherit org-modern-label)))
  :config
  (defun kb/modus-themes--setup-org-modern (theme)
    "Set up org-modern faces."
    (when (string-match "^modus-" (symbol-name theme))
      ;; See my value for `org-tag-faces'
      (setopt org-modern-tag-faces
              `(("project"
                 :foreground ,(face-background 'default nil t)
                 :background ,(face-foreground 'modus-themes-fg-magenta-cooler nil t))))))
  (add-hook 'enable-theme-functions #'kb/modus-themes--setup-org-modern))

;;; Provide
(provide 'krisb-org)
