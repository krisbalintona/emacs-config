;;; programming-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Language-agnostic packages helpful or required for programming.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Autofill
;;;;; Yasnippet
;; Template-expansion system (doesn't include templates)
(use-package yasnippet
  :ghook ('after-init-hook 'yas-minor-mode-on)
  :custom
  (warning-suppress-types
   '(((yasnippet backquote-change))
     (comp)
     (:warning))
   )
  )

;;;;; Doom-snippets
;; Large library of snippet templates
(use-package doom-snippets
  :after yasnippet
  :hook (after-init . yas-reload-all)
  :straight (doom-snippts :type git :host github :repo "hlissner/doom-snippets")
  )

;;;;; Org-tempo
;; Completion for org-block types
(use-package org-tempo
  :straight nil
  :config
  (dolist (expansion '(("sh" . "src sh")
                       ("el" . "src emacs-lisp")
                       ("py" . "src python")
                       ;; ("sc" . "src scheme")
                       ;; ("ts" . "src typescript")
                       ;; ("yaml" . "src yaml")
                       ;; ("json" . "src json")
                       )
                     org-structure-template-alist)
    (push expansion org-structure-template-alist))
  )

;;;; Project management
;;;;; Projectile
;; Navigate and manage project directories easier
(use-package projectile
  :disabled t ; In favor of `project.el'
  :hook (after-init . projectile-mode)
  :init
  (when (file-directory-p user-emacs-directory)
    (setq projectile-project-search-path `(,user-emacs-directory)))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom
  (projectile-completion-system 'default) ; Use selectrum
  (projectile-enable-caching t)
  (projectile-track-known-projects-automatically nil) ; Don't create projects automatically
  :config
  ;; Hydra menu
  (pretty-hydra-define hydra:selectrum-projectile
    (:color blue :hint t :foreign-keys run :quit-key "q" :exit t)
    ("Projectile"
     (("i" projectile-invalidate-cache :color red)
      ("n" projectile-add-known-project))
     "Buffers"
     (("b" projectile-switch-to-buffer)
      ("K" projectile-kill-buffers)
      ("S" projectile-save-project-buffers))
     "Find"
     (("d" projectile-find-dir)
      ("D" projectile-dired)
      ("f" projectile-find-file)
      ("p" projectile-switch-project))
     "Search"
     (("r" projectile-replace)
      ("R" projectile-replace-regexp)
      ("s" counsel-projectile-rg))
     ))

  (kb/leader-keys
    "p" '(:ignore t :which-key "Projectile")
    "p?" '(hydra:selectrum-projectile/body :which-key "Help menu")
    ;; "pf"  'projectile-find-file
    "pp"  'projectile-switch-project
    ;; "ps"  'counsel-projectile-rg
    "pb"  'projectile-switch-to-buffer
    "pD"  'projectile-dired
    ;; "pc"  'projectile-compile-project
    )
  )

;;;;; Counsel-projectile
;; Use Ivy as projectile interface
(use-package counsel-projectile
  :requires (counsel projectile)
  :ghook 'counsel-mode-hook
  )

;;;;; Project.el
(use-package project
  :config
  (kb/leader-keys
    "p" '(:ignore t :which-key "Project")
    "pF"  '(project-find-file :which-key "Project find file")
    "pp"  '(project-switch-project :which-key "Project.el switch project")
    "pb"  '(project-switch-to-buffer :which-key "Project switch to buffer")
    "pD"  '(project-dired :which-key "Project dired")
    )
  )


;;;; Directory navigation
;;;;; Dired
;; Emacs' file manager
(use-package dired
  :straight nil
  :gfhook 'dired-hide-details-mode
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t) ; Guess default target directory
  (dired-hide-details-hide-symlink-targets nil) ; Don't hide symlink targets
  (dired-recursive-copies 'always) ; Copy directories recursively?
  )

;;;;; Dired-git
;; Show git information in dired
(use-package dired-git
  :ghook 'dired-mode-hook
  )

;;;; Aesthetics
;;;;; Highlight-indent-guides
;; Show indicator for indentation levels (like in VS Code)
(use-package highlight-indent-guides
  :ghook 'prog-mode-hook
  :gfhook 'highlight-indent-guides-auto-set-faces ; Set faces based on theme
  :custom
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-character ?⏐)
  )

;;;;; Rainbow-mode
;; Colorify color codes
(use-package rainbow-mode
  :hook 'text-mode-hook 'prog-mode-hook
  )

;;;;; Highlight-defined
;; Have face for emacs-lisp variables already defined
(use-package highlight-defined
  :ghook 'prog-mode-hook
  :custom
  (highlight-defined-face-use-itself t)
  :config
  (set-face-attribute 'highlight-defined-variable-name-face nil :inherit 'font-lock-variable-name-face :foreground "#9caabf")
  )

;;;;; Highlight-quoted
;; Make (lisp) quotes and quoted symbols easier to distinguish from free variables by highlighting
;; them
(use-package highlight-quoted
  :hook 'emacs-lisp-mode-hook
  )

;;;; Other
;;;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  ;; :demand t ; For the initial scratch buffer at startup
  :hook (scratch-create-buffer . kb/scratch-buffer-setup)
  :general ("C-c s" '(scratch :which-key "Create scratch"))
  :preface
  (defun kb/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly. Taken from https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/"
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t))
    )
  )

;;; programming-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-general-rcp)
