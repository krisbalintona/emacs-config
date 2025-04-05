;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/krisbalintona/Documents/org-database/agenda/archive/2023-and-before todos.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/archive/bui.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/archive/buoy.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/archive/crc.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/archive/cs1730.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/archive/emacs.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/archive/emails.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/archive/hist0244.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/archive/hist1974i.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/archive/phil1340.org_archive"
     "/home/krisbalintona/Documents/org-database/agenda/gcal.org"
     "/home/krisbalintona/Documents/org-database/agenda/recurring.org"
     "/home/krisbalintona/Documents/org-database/agenda/refile.org"
     "/home/krisbalintona/Documents/org-database/agenda/todo.org") nil nil "Customized with use-package org-agenda")
 '(package-selected-packages
   '(ace-window activities adaptive-wrap agitate all-the-icons apheleia astute
                beframe boon breadcrumb bufler cape cascading-dir-locals
                citar-denote citar-org-roam cl-generic cm-mode cond-star
                consult-xref-stack corfu-prescient csv-mode cursory darkroom
                default-text-scale denote denote-explore denote-interface
                diff-hl diminish dired-hist dired-subtree dumber-jump
                easysession eat edit-indirect editorconfig eglot eglot-booster
                elisp-demos embark-consult engine-mode enlight erc eros
                eshell-atuin eshell-syntax-highlighting eshell-z
                exec-path-from-shell faceup fancy-compilation fish-mode
                flymake-collection fontaine forge form-feed gcmh gif-screencast
                git-share god-mode grid hammy hide-mode-line
                highlight-function-calls hotfuzz hugoista hyprlang-ts-mode
                ibuffer-project idlwave indexed info-colors inspector jinx
                kbd-mode keychain-environment kind-icon lazy-guard ligature lin
                litanize lorem-ipsum marginalia meow mermaid-mode mixed-pitch
                mode-minder modus-themes motion-selection motion-selection-mode
                mpv nadvice nerd-icons-completion nerd-icons-dired
                nerd-icons-ibuffer no-littering notmuch-addr notmuch-transient
                nov ntlm ob-mermaid oc-csl-activate ol-notmuch olivetti on
                orderless org org-appear org-bookmark-heading org-bulletproof
                org-contrib org-edna org-make-toc org-mime org-modern org-node
                org-remark org-review org-roam-ql org-roam-ui org-web-tools
                outli outline-indent ox-hugo package-build package-lint-flymake
                paren-face pcmpl-args pdf-meta-edit pdf-tools peg pinentry
                popper powerthesaurus psession pulsar puni python rainbow-mode
                recursion-indicator saveplace-pdf-view scratch show-font
                sinister smart-mark so-long soap-client sudo-edit suggest svg
                system-packages tmr tramp transient transpose-frame treesit-auto
                try typewriter-roll-mode ultra-scroll use-package vc-jj
                verilog-mode vertico-prescient which-key window-tool-bar wombag
                ytdl))
 '(package-vc-selected-packages
   '((mode-minder :url "https://github.com/jdtsmith/mode-minder.git")
     (kbd-mode :url "https://github.com/kmonad/kbd-mode")
     (flymake-collection :url
                         "https://github.com/krisbalintona/flymake-collection.git"
                         :branch "vale-remove-unnecessary-splice" :lisp-dir
                         "src/")
     (outli :url "https://github.com/jdtsmith/outli")
     (eglot-booster :url "https://github.com/jdtsmith/eglot-booster.git")
     (oblique :url "https://github.com/zzkt/oblique-strategies.git")
     (oblique-strategies :url "https://github.com/zzkt/oblique-strategies.git")
     (clever-cite :vc-backend Git :url
                  "https://github.com/Hugo-Heagren/clever-cite.git")
     (git-share :vc-backend Git :url "https://github.com/mgmarlow/git-share")
     (lazy-guard :url "https://codeberg.org/vifon/lazy-guard.git")
     (sinister :url "https://github.com/positron-solutions/sinister")
     (vc-jj :url "https://codeberg.org/emacs-jj-vc/vc-jj.el.git")
     (jj-vc :url "https://codeberg.org/emacs-jj-vc/vc-jj.el.git")
     (hugoista :url "https://codeberg.org/c-alpha/hugoista.git")
     (org-supertag :url "https://github.com/yibie/org-supertag.git")
     (ultra-scroll :url "https://github.com/jdtsmith/ultra-scroll")
     (jujutsushi :url "https://git.sr.ht/~puercopop/jujutsushi")
     (eat :url "https://codeberg.org/vifon/emacs-eat.git" :branch
          "fish-integration")
     (jujutsu :url "https://github.com/bennyandresen/jujutsu.el.git")
     (buffer-terminator :url
                        "https://github.com/jamescherti/buffer-terminator.el")
     (consult-xref-stack :url
                         "https://github.com/brett-lempereur/consult-xref-stack")
     (org-roam-folgezettel :url
                           "git@github.com:krisbalintona/org-roam-folgezettel.git")
     (cm-mode :url "https://github.com/joostkremers/criticmarkup-emacs.git")
     (org-super-agenda :url
                       "https://github.com/Alexander-Miller/org-super-agenda.git")
     (grid :url "https://github.com/ichernyshovvv/grid.el")
     (wombag :url "https://github.com/karthink/wombag.git")
     (denote-interface :url "git@github.com:krisbalintona/denote-interface.git")))
 '(safe-local-variable-directories
   '("/home/krisbalintona/Documents/org-database/notes/blog/"
     "/home/krisbalintona/emacs-repos/other/melpa/"
     "/home/krisbalintona/Documents/org-database/notes/main/"
     "/home/krisbalintona/Documents/org-database/notes/commonplace_book/buoy/"
     "/home/krisbalintona/Documents/org-database/notes/"
     "/home/krisbalintona/Documents/org-database/notes/references/"
     "/home/krisbalintona/Documents/org-database/notes/papers/"
     "/home/krisbalintona/Documents/org-database/notes/thoughts/"
     "/home/krisbalintona/Documents/org-database/notes/buoy/"
     "/home/krisbalintona/Documents/org-database/notes/zettels/"
     "/home/krisbalintona/emacs-repos/packages/citar/"
     "/home/krisbalintona/Documents/org-database/notes/bib/"
     "/home/krisbalintona/Documents/org-database/agenda/"
     "/home/krisbalintona/Documents/org-database/notes/commonplace_book/"))
 '(safe-local-variable-values
   '((org-review-delay . "+3d") (eval add-hook 'before-save-hook 'org-make-toc)
     (org-use-tag-inheritance) (org-use-tag-inheritance . t)
     (org-time-stamp-custom-formats "<%B %d, %Y>" . "<%B %d, %Y %H:%M>")
     (org-time-stamp-custom-formats quote ("<%B %d, %Y>" . "<%B %d, %Y %H:%M>"))
     (system-time-locale . "C")
     (eval add-hook 'org-insert-heading-hook 'org-expiry-insert-created 99 t)))
 '(tmr-description-list '("Stop working!" "Work time 😄") nil nil "Customized with use-package tmr")
 '(warning-suppress-types
   '((org-element org-element-parser) (org-element org-element-parser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(meow-position-highlight-number-1 ((((class color) (min-colors 256)) :inherit (bold modus-themes-reset-soft) :background "#0050af")))
 '(meow-position-highlight-number-2 ((((class color) (min-colors 256)) :inherit (bold modus-themes-reset-soft) :background "#7f1f7f")))
 '(meow-position-highlight-number-3 ((((class color) (min-colors 256)) :inherit (bold modus-themes-reset-soft) :background "#625a00")))
 '(meow-region-cursor-1 ((((class color) (min-colors 256)) :inherit (bold modus-themes-reset-soft) :background "#0050af")))
 '(meow-region-cursor-2 ((((class color) (min-colors 256)) :inherit (bold modus-themes-reset-soft) :background "#7f1f7f")))
 '(meow-region-cursor-3 ((((class color) (min-colors 256)) :inherit (bold modus-themes-reset-soft) :background "#625a00"))))
