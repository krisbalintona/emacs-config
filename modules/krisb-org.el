;;; Built-in
;;;; Org
(use-package org
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode)
         (org-mode . (lambda () (setq-local line-spacing 0.2 fill-column 120))))
  :bind (("C-c s" . org-store-link)
         ("C-c c" . org-capture))
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
  (org-special-ctrl-k t)
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
  (require 'krisb-org-ext)

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
  :custom
  (org-clone-delete-id t)
  (org-id-method 'ts)
  (org-id-link-to-org-use-id 'use-existing))

;;;; Org-attach
(use-package org-attach
  :ensure nil
  :custom
  (org-attach-preferred-new-method 'id) ; Necessary to add the ATTACH tag
  (org-attach-auto-tag "ATTACH")
  (org-attach-dir-relative nil)         ; Use relative file paths?
  (org-attach-id-dir (expand-file-name "resources" org-directory))
  (org-attach-method 'cp)            ; Attach copies of files
  (org-attach-archive-delete 'query) ; If subtree is deleted or archived, ask user
  (org-attach-id-to-path-function-list
   '(org-attach-id-ts-folder-format
     org-attach-id-uuid-folder-format
     org-attach-id-fallback-folder-format)))

;;;; Org-footnote
(use-package org-footnote
  :ensure nil
  :after org
  :custom
  (org-footnote-section nil)            ; Don't create footnote headline
  (org-footnote-auto-adjust t)
  (org-footnote-define-inline nil))

;;;; Org-babel
(use-package ob
  :ensure nil
  :custom
  (org-babel-load-languages '((python . t)
                              (emacs-lisp . t)))
  (org-confirm-babel-evaluate nil))


;;;; Org-refile
(use-package org-refile
  :ensure nil
  :custom
  (org-refile-use-cache nil)
  (org-refile-targets
   `((,krisb-org-agenda-directory-files . (:level . 0))
     (,krisb-org-agenda-directory-files . (:tag . "project"))
     (,krisb-org-agenda-main-file . (:maxlevel . 3))))
  ;; TODO 2024-10-07: Think about whether I actually want this before. What if I
  ;; want to refile to a non-todo heading in the current file?
  (org-refile-target-verify-function    ; Only let not done todos be refile targets
   (lambda () (if (org-entry-is-todo-p) (not (org-entry-is-done-p)))))
  (org-refile-allow-creating-parent-nodes 'confirm)
  :config
  ;; Workaround for orderless issue with `org-refile'. See
  ;; https://github.com/minad/vertico#org-refile
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (when (bound-and-true-p vertico-mode)
    (advice-add #'org-olpath-completing-read :around
                (lambda (&rest args)
                  (minibuffer-with-setup-hook
                      (lambda () (setq-local completion-styles '(basic)))
                    (apply args))))))

;;;; Org-archive
(use-package org-archive
  :ensure nil
  :custom
  (org-archive-subtree-save-file-p t)  ; Save archive file always
  (org-archive-subtree-add-inherited-tags t)
  :config
  (define-advice org-archive--compute-location
      (:around (orig-fun &rest args) krisb-org-archive--compute-location-denote-format-string)
    "Take LOCATION in `org-archive--compute-location' and expand %D.
%D is expanded to the denote identifier."
    ;; Modify LOCATION before normal operations
    (cl-letf (((car args)
               (if (fboundp 'denote-retrieve-filename-identifier)
                   (replace-regexp-in-string "%D"
                                             (denote-retrieve-filename-identifier (buffer-file-name (buffer-base-buffer)))
                                             (car args))
                 (car args))))
      (apply orig-fun args))))

;;;; Org-num
(use-package org-num
  :ensure nil
  :diminish
  :bind ( :map krisb-toggle-keymap
          ("n" . org-num-mode))
  :custom
  (org-num-face 'fixed-pitch)
  (org-num-skip-commented t)
  (org-num-skip-footnotes t)
  (org-num-skip-unnumbered t))

;;; Org-contrib
(use-package org-contrib
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))) ; The ignore tag will export contents but ignore heading

;;; Org-modern
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
  (krisb-modus-themes-setup-faces
   "org-modern"
   (setopt org-modern-tag-faces
           `(("project"
              :foreground ,(face-background 'default nil t)
              :background ,(face-foreground 'modus-themes-fg-magenta-cooler nil t))))))

;;; Org-appear
;; Show hidden characters (e.g. emphasis markers, link brackets) when point is
;; over enclosed content
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-delay 0.0)
  (org-appear-trigger 'always)
  (org-appear-autoemphasis t)
  (org-appear-autolinks 'just-brackets)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))

;;; Org-tidy
;; Make org drawers less visually obtrusive.
(use-package krisb-org-hide-drawers
  :ensure nil
  :diminish
  :hook (org-mode . krisb-org-hide-drawers-mode)
  :bind ( :map krisb-toggle-keymap
          ("t" . krisb-org-hide-drawers-transient))
  :custom
  (krisb-org-hide-drawers-blacklist '("CUSTOM_ID"))
  :config
  (require 'transient)
  (transient-define-prefix krisb-org-hide-drawers-transient ()
    "Transient map for useful krisb-org-hide-drawers commands."
    [("u" "Hide drawers" krisb-org-hide-drawers-create-overlays)
     ("u" "Unhide drawers" krisb-org-hide-drawers-delete-overlays)
     ("t" "Toggle hiding" krisb-org-hide-drawers-toggle)]))

;;; Org-bulletproof
;; Automatically cycle plain list bullet point styles.
(use-package org-bulletproof
  :hook (org-mode . org-bulletproof-mode)
  :custom
  (org-bulletproof-ordered-cycle '("1." "1)"))
  (org-bulletproof-unordered-cycle '("+" "-" "*")))

;;; Org-web-tools
;; Paste https links with automatic descriptions
(use-package org-web-tools
  :bind ( :map krisb-yank-keymap
          ("b" . org-web-tools-insert-link-for-url))
  :config
  (with-eval-after-load 'org-attach
    (add-to-list 'org-attach-commands
                 '((?w) org-web-tools-archive-attach
                   "Download then attach an archive of a webpage using `org-web-tools'\n")))

  (advice-add 'org-web-tools-read-url-as-org :after #'view-mode))

;;; Org-bookmark-heading
(use-package org-bookmark-heading
  ;; TODO 2024-10-30: Consider also adding `org-cycle-set-startup-visibility' to
  ;; `org-bookmark-heading-after-jump-hook'
  :hook (org-bookmark-heading-after-jump . org-narrow-to-subtree)
  :custom
  (org-bookmark-heading-make-ids nil))

;;; Provide
(provide 'krisb-org)
