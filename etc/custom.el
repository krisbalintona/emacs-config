;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(activities arei astute bui cape citar-org-node cl-generic cond-star
                consult corfu do-at-point eat edit-indirect
                editorconfig ef-themes eglot el-patch elisp-demos erc
                exec-path-from-shell faceup fancy-compilation
                fish-mode flymake-vale fontaine gcmh geiser-guile
                highlight-function-calls hotfuzz inspector jinx lin
                magit-popup mixed-pitch no-littering notmuch-addr
                notmuch-bookmarks ntlm ol-notmuch olivetti on
                orderless org-contrib org-hide-drawers org-mime
                org-modern org-ql org-review org-roam-folgezettel
                org-web-tools orgmdb outli package-upgrade-guard
                paren-face persid powerthesaurus pulsar puni python
                setup smart-mark so-long soap-client svg tablist
                tempel timeout track-changes tramp tramp-theme try
                use-package vc-jj verilog-mode vertico which-key
                window-tool-bar wombag work-timer))
 '(package-vc-selected-packages
   '((org-mime :url "https://github.com/krisbalintona/org-mime.git"
               :branch "merge")
     (persid :url "https://github.com/rougier/persid.git")
     (citar-org-node :url
                     "https://github.com/krisbalintona/citar-org-node.git")
     (package-upgrade-guard :url
                            "https://github.com/kn66/package-upgrade-guard.el.git")
     (flymake-vale :url
                   "https://github.com/tpeacock19/flymake-vale.git")
     (work-timer :url "git@github.com:krisbalintona/work-timer.git")
     (wombag :url "https://github.com/krisbalintona/wombag.git"
             :branch "merge")
     (org-roam-folgezettel :url
                           "https://github.com/krisbalintona/org-roam-folgezettel.git"
                           :branch "vtable-unstable")
     (org-hide-drawers :url
                       "https://github.com/krisbalintona/org-hide-drawers.git"
                       :branch "devel")
     (vc-jj :url "https://codeberg.org/krisbalintona/vc-jj.el.git"
            :branch "merge")
     (hotfuzz :url "https://github.com/axelf4/hotfuzz.git"
              :shell-command
              "cmake -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_FLAGS=-march=native && cmake --build build")
     (vtable :url "https://github.com/krisbalintona/emacs.git" :branch
             "vtable-ship-mints" :main-file
             "lisp/emacs-lisp/vtable.el")
     (org-capture :url "https://github.com/krisbalintona/org-mode.git"
                  :branch "org-capture" :main-file
                  "lisp/org-capture.el")))
 '(safe-local-variable-directories
   '("/home/krisbalintona/Documents/org-database/notes/notebooks/buoy/"
     "/home/krisbalintona/Documents/org-database/notes/notebooks/"
     "/home/krisbalintona/Documents/org-database/notes/media/"
     "/home/krisbalintona/repos/guix/"
     "/home/krisbalintona/Documents/org-database/"
     "/home/krisbalintona/.emacs.d/var/elpaca/repos/org-roam/"
     "/home/krisbalintona/Documents/org-database/notes/main/"
     "/home/krisbalintona/Documents/org-database/notes/manuscripts/academic/"
     "/home/krisbalintona/Documents/org-database/notes/commonplace_book/buoy/"
     "/home/krisbalintona/Documents/org-database/notes/"
     "/home/krisbalintona/Documents/org-database/notes/references/"
     "/home/krisbalintona/emacs-repos/configs/doom-emacs/"
     "/home/krisbalintona/Documents/org-database/agenda/"
     "/home/krisbalintona/Documents/org-database/notes/commonplace_book/"
     "/home/krisbalintona/Documents/org-database/notes/manuscripts/blog/"
     "/home/krisbalintona/.config/guix/current/share/guile/site/3.0/"
     "/home/krisbalintona/nix-config/"))
 '(safe-local-variable-values
   '((eval add-hook 'org-insert-heading-hook 'org-expiry-insert-created
           nil t)
     (eval add-hook 'before-save-hook 'org-make-toc)
     (org-use-tag-inheritance . t) (org-review-delay . "+3d")
     (eval add-hook 'org-insert-heading-hook
           'org-expiry-insert-created 99 t)
     (org-use-tag-inheritance))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-default ((t (:inherit 'default))))
 '(highlight-function-calls-face ((t (:underline nil :inherit font-lock-function-call-face))))
 '(log-edit-summary ((t (:family "Overpass Nerd Font Propo"))))
 '(org-cite ((t (:foreground "DarkSeaGreen4"))))
 '(org-cite-key ((t (:foreground "forest green" :slant italic)))))
