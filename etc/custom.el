;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window activities adaptive-wrap agitate all-the-icons apheleia astute
                beframe breadcrumb buffer-terminator bufler cape citar-denote
                citar-org-roam cl-generic cm-mode consult-xref-stack
                corfu-prescient csv-mode cursory darkroom denote-explore
                denote-interface diff-hl diminish dired-hist dumber-jump
                easysession eat edit-indirect editorconfig eglot elisp-demos
                embark-consult engine-mode enlight erc eros eshell-atuin
                eshell-syntax-highlighting eshell-z exec-path-from-shell faceup
                fish-mode flymake-collection fontaine forge form-feed gcmh
                gif-screencast grid hammy hide-mode-line
                highlight-function-calls hotfuzz hyprlang-ts-mode
                ibuffer-project idlwave info-colors inspector jinx jujutsu
                jujutsushi keychain-environment kind-icon lin lorem-ipsum
                magit-file-icons marginalia mermaid-mode mixed-pitch
                modus-themes mpv nadvice nerd-icons-completion nerd-icons-dired
                nerd-icons-ibuffer no-littering notmuch-addr notmuch-transient
                nov ntlm ob-mermaid oc-csl-activate ol-notmuch olivetti on
                orderless org-appear org-bookmark-heading org-bulletproof
                org-contrib org-edna org-make-toc org-mime org-modern
                org-node-fakeroam org-remark org-review org-roam-ql org-roam-ui
                org-supertag org-tidy org-web-tools outshine ox-hugo
                package-build package-lint-flymake paren-face pcmpl-args
                pdf-meta-edit pdf-tools peg pinentry popper powerthesaurus
                psession pulsar puni python rainbow-mode recursion-indicator
                saveplace-pdf-view scratch show-font smart-mark so-long
                soap-client sudo-edit svg system-packages tablist tmr tramp
                treesit-auto try typewriter-roll-mode ultra-scroll use-package
                verilog-mode vertico-prescient which-key window-tool-bar wombag
                ytdl))
 '(package-vc-selected-packages
   '((org-supertag :url "https://github.com/yibie/org-supertag.git")
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
   '("/home/krisbalintona/emacs-repos/other/melpa/"
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
   '((eval add-hook 'before-save-hook 'org-make-toc) (org-use-tag-inheritance)
     (org-use-tag-inheritance . t)
     (org-time-stamp-custom-formats "<%B %d, %Y>" . "<%B %d, %Y %H:%M>")
     (org-time-stamp-custom-formats quote ("<%B %d, %Y>" . "<%B %d, %Y %H:%M>"))
     (system-time-locale . "C")
     (eval add-hook 'org-insert-heading-hook 'org-expiry-insert-created 99 t)))
 '(tmr-description-list '("Stop working!" "Work time 😄") nil nil "Customized with use-package tmr"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
