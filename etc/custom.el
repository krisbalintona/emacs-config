;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(abridge-diff ace-window activities adaptive-wrap agitate all-the-icons
                  apheleia artbollocks-mode astute async auctex-cont-latexmk
                  breadcrumb bufler cape cascading-dir-locals citar cl-generic
                  cm-mode cond-star consult-xref-stack corfu-prescient cursory
                  darkroom default-text-scale denote diminish dired-hist
                  dumber-jump eat editorconfig eglot-booster
                  eglot-signature-eldoc-talkative el-patch elisp-demos
                  embark-consult engine-mode erc eros eshell-atuin
                  eshell-syntax-highlighting eshell-z exec-path-from-shell
                  faceup fancy-compilation fish-mode flymake-collection fontaine
                  forge form-feed gcmh git-share god-mode hammy hide-mode-line
                  highlight-function-calls highlight-parentheses hl-todo hotfuzz
                  hugoista hyprlang-ts-mode indexed info-colors inspector jinx
                  keychain-environment kind-icon kv lazy-guard lice ligature lin
                  lorem-ipsum lsp-bridge marginalia markdown-mode mermaid-mode
                  mixed-pitch mode-minder modus-themes nadvice
                  nerd-icons-completion nerd-icons-dired nerd-icons-ibuffer
                  nix-mode nix-ts-mode no-littering notmuch-addr
                  notmuch-transient nov ntlm ob-mermaid ol-notmuch olivetti on
                  orderless org-appear org-bookmark-heading org-bulletproof
                  org-contrib org-edna org-make-toc org-mime org-modern org-node
                  org-project-capture org-remark org-review org-roam-ql
                  org-roam-ui org-web-tools outli ox-hugo package-build
                  package-lint-flymake paren-face pcmpl-args pdf-tools peg
                  persist-state powerthesaurus pulsar puni python rainbow-mode
                  realgud recursion-indicator savefold saveplace-pdf-view
                  scratch show-font smart-mark so-long soap-client
                  stillness-mode sudo-edit suggest svg system-packages tmr tramp
                  treesit-auto try typewriter-roll-mode ultra-scroll use-package
                  vc-jj verilog-mode vertico-prescient which-key window-tool-bar
                  wombag yasnippet))
 '(package-vc-selected-packages
   '((vc-jj :url "https://codeberg.org/emacs-jj-vc/vc-jj.el.git")
     (indexed :url "https://github.com/meedstrom/indexed.git" :branch "dev")
     (lsp-bridge :vc-backend Git :url
                 "https://github.com/manateelazycat/lsp-bridge.git")
     (org-modern-indent :vc-backend Git :url
                        "https://github.com/jdtsmith/org-modern-indent.git")
     (savefold :url "https://github.com/jcfk/savefold.el.git")
     (savefold.el :vc-backend Git :url "https://github.com/jcfk/savefold.el.git")
     (mini-ontop.el :vc-backend Git :url
                    "https://github.com/hkjels/mini-ontop.el.git")
     (stillness :url "https://github.com/neeasade/stillness-mode.el")
     (fancy-joiner :vc-backend Git :url
                   "https://git.sr.ht/~flandrew/fancy-joiner")
     (democratize :url "https://git.sr.ht/~flandrew/democratize")
     (mode-minder :url "https://github.com/jdtsmith/mode-minder.git")
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
   '((eval add-hook 'before-save-hook 'org-make-toc nil t)
     (org-review-delay . "+3d") (eval add-hook 'before-save-hook 'org-make-toc)
     (org-use-tag-inheritance) (org-use-tag-inheritance . t)
     (org-time-stamp-custom-formats "<%B %d, %Y>" . "<%B %d, %Y %H:%M>")
     (org-time-stamp-custom-formats quote ("<%B %d, %Y>" . "<%B %d, %Y %H:%M>"))
     (system-time-locale . "C")
     (eval add-hook 'org-insert-heading-hook 'org-expiry-insert-created 99 t)))
 '(tmr-description-list '("Stop working!" "Work time 😄") nil nil "Customized with use-package tmr")
 '(warning-suppress-types
   '((files missing-lexbind-cookie
            "/home/krisbalintona/.emacs.d/var/prescient-save.el")
     (org-element org-element-parser) (org-element org-element-parser))))
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
