;; -*- lexical-binding: t; -*-

;;; Built-in

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

;;;; Ol
(use-package ol
  :ensure nil
  :custom
  (org-link-search-must-match-exact-headline nil))

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
   '((krisb-org-agenda-directory-files . (:level . 0))
     (krisb-org-agenda-directory-files . (:tag . "project"))
     (krisb-org-agenda-main-file . (:maxlevel . 3))))
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
  (org-archive-subtree-save-file-p 'from-org)
  (org-archive-subtree-add-inherited-tags t))

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

;;; Ol
(use-package ol
  :ensure nil
  :after org
  :config
  ;; Bespoke "color" org link type; meant for in-buffer colorized text as well
  ;; as simple colorized text in export.
  (defun krisb-org-link-color-export (link description format)
    (let ((desc (or description link)))
      (cond
       ((eq format 'latex) (format "\\textcolor{%s}{%s}" link desc))
       (t desc))))

  (org-link-set-parameters "color"
                           :face (lambda (path) `(:foreground ,path))
                           :export #'krisb-org-link-color-export
                           :complete (lambda (&optional _)
                                       (concat "color:"
                                               (completing-read "Choose color: " (list-colors-duplicates (defined-colors)))))))

;;; Org-contrib
(use-package org-contrib
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))) ; The ignore tag will export contents but ignore heading

;;; Krisb-org-ext
(use-package krisb-org-ext
  :ensure nil
  :after org
  :hook (org-mode . krisb-org-ext-setup-eldoc)
  :config
  (defun krisb-org-ext-setup-eldoc ()
    "Set up `eldoc-documentation-functions' in org-mode buffers."
    (add-hook 'eldoc-documentation-functions #'krisb-org-ext-eldoc-footnote nil t)
    (setq-local eldoc-idle-delay 1)))

;;; Org-bulletproof
;; Automatically cycle plain list bullet point styles.
(use-package org-bulletproof
  :hook (org-mode . org-bulletproof-mode)
  :custom
  (org-bulletproof-ordered-cycle '("1." "1)"))
  (org-bulletproof-unordered-cycle '("+" "-" "*")))

;;; Org-keyterm-index
(use-package org-keyterm-index
  :after org
  :load-path "/home/krisbalintona/emacs-repos/packages/org-keyterm-index/")

;;; Org-bookmark-heading
(use-package org-bookmark-heading
  ;; TODO 2024-10-30: Consider also adding `org-cycle-set-startup-visibility' to
  ;; `org-bookmark-heading-after-jump-hook'
  :custom
  (org-bookmark-heading-make-ids nil))

;;; Org-project-capture
(use-package org-project-capture
  :bind ( :map project-prefix-map
          ("n c" . org-project-capture-project-todo-completing-read)
          ("n C" . org-project-capture-capture-for-current-project)
          ("n g" . org-project-capture-goto-location-for-project))
  :custom
  (org-project-capture-backend (make-instance 'org-project-capture-project-backend))
  (org-project-capture-projects-file (expand-file-name "coding_projects.org" krisb-org-agenda-directory))
  :config
  (org-project-capture-single-file))

;;; Provide
(provide 'krisb-org)
