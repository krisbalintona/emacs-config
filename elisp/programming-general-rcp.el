;;; programming-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Language-agnostic packages helpful or required for programming.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

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
    "pf"  '(project-find-file :which-key "Project find file")
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
  :general
  (:keymaps 'dired-mode-map
            :states 'normal
            "h" 'dired-up-directory
            "l" 'dired-find-file)
  (kb/leader-keys
    "od" '(dired :which-key "Dired"))
  :custom
  (dired-auto-revert-buffer t)          ; Automatically revert buffer
  (dired-dwim-target nil)               ; Guess default target directory?
  (dired-hide-details-hide-symlink-targets nil) ; Don't hide symlink targets
  (dired-recursive-copies 'always)              ; Copy directories recursively?
  (dired-listing-switches "-agho --group-directories-first") ; Flags `dired' passes to `ls'
  )

;;;;; Dired-git
;; Show git information in dired
(use-package dired-git
  :ghook 'dired-mode-hook
  :custom
  (dired-git-disable-dirs
   '())
  (dired-git-parallel 7)                ; Number of parallel processes
  )

;;;;; Dired-single
;; Use the same dired buffer for every directory you open using `dired'.
(use-package dired-single
  :general (:keymaps 'dired-mode-map
                     [remap dired-up-directory] 'dired-single-up-directory
                     [remap dired-find-file] 'dired-single-buffer)
  )

;;;;; All-the-icons-dired
;; Add icons which represent file types
(use-package all-the-icons-dired
  :ghook 'dired-mode-hook
  :gfhook '(lambda () (setq-local all-the-icons-scale-factor 0.95))
  :custom
  (all-the-icons-dired-monochrome nil) ; Icon the same color as the text on the line?
  )

;;;;; Dired-open
;; Override how dired opens files with certain extensions
(use-package dired-open
  :custom
  ;; ;; Try to use `xdg-open' before anything else
  ;; (add-to-list 'dired-open-functions #'dired-open-xdg t) ; Doesn't work as expected!
  (dired-open-extensions '(("png" . "feh")
                           ("mkv" . "vlc")))
  )

;;;;; Dired-hide-dotfiles
;; Hide dotfiles
(use-package dired-hide-dotfiles
  :ghook 'dired-mode-hook
  :general (:keymaps 'dired-mode-map
                     :states 'normal
                     "H" 'dired-hide-dotfiles-mode)
  :custom
  (dired-hide-dotfiles-verbose nil) ; No annoying announcements in echo area anymore
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
  :ghook 'text-mode-hook 'prog-mode-hook
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
  :ghook 'emacs-lisp-mode-hook
  )

;;;; Other
;;;;; Tramp
(use-package tramp
  :custom
  (tramp-default-method "ssh")
  )

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

;;;;; Consult
;; Counsel equivalent for default Emacs completion. It provides many useful
;; commands.
(use-package consult
  :ensure-system-package ((fd . fd-find)
                          (rg . ripgrep))
  :straight (consult :type git :host github :repo "minad/consult")
  :general
  ([remap apropos-command] '(consult-apropos :which-key "Consult apropos"))
  (kb/leader-keys
    :keyamps 'prog-mode-map
    "le" '(consult-error :which-key "Consult error"))
  (kb/leader-keys
    :keymaps 'org-mode-map
    :states '(normal visual motion)
    [remap consult-outline] '(consult-org-heading :which-key "Consult outline"))
  (kb/leader-keys
    "fr" '(consult-recent-file :which-key "Consult recent file")
    "bb" '(consult-buffer :which-key "Consult buffer")
    ;; ("C-x 4 b" . consult-buffer-other-window)
    ;; ("C-x 5 b" . consult-buffer-other-frame)

    "mm" '(consult-bookmark :which-key "Consult bookmark")
    "mr" '(consult-mark :which-key "Consult mark-ring")

    "so" '(consult-outline :which-key "Consult outline")

    "ss" '(consult-line :which-key "Consult swiper")
    "si" '(consult-imenu :which-key "Consult imenu")
    "sO" '(consult-multi-occur :which-key "Consult multi-occur")

    "iy" '(consult-yank-pop :which-key "Consult yank-pop")
    ;; ("C-x r x" . consult-register)
    "ha" '(consult-apropos :which-key "Consult apropos")
    "pF" '(consult-find :which-key "Consult find file")
    "ps" '(consult-ripgrep :which-key "Consult rg")
    )
  :custom
  (consult-mode-histories ; What variable consult-history looks at for history
   '((eshell-mode . eshell-history-ring)
     (comint-mode . comint-input-ring)
     (term-mode . term-input-ring))
   )
  (consult-project-root-function #'vc-root-dir)
  :config
  ;; Enhanced multiple selection experience. Replaced the built-in method
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Customize consult commands
  (consult-customize
   ;; For `consult-buffer'
   consult-buffer :preview-key (kbd "M-'")
   consult-buffer :prompt "Can use b, m, f, p..."
   ;; For `consult-ripgrep'
   consult-ripgrep :preview-key (kbd "M-'")
   ;; For `consult-fdfind'. Make sure this is after the definition of
   ;; `consult-recent-file'
   consult-recent-file :preview-key (kbd "M-'")
   ;; `consult-find'
   consult-find :preview-key (kbd "M-'")
   )
  )

;;;;; Embark-consult
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :requires (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

;;; programming-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-general-rcp)
