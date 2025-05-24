;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(arei bui edit-indirect ef-themes el-patch fontaine geiser-guile jinx
          magit-popup no-littering on tablist))
 '(safe-local-variable-directories
   '("/home/krisbalintona/emacs-repos/configs/doom-emacs/"
     "/home/krisbalintona/Documents/org-database/agenda/"
     "/home/krisbalintona/Documents/org-database/notes/commonplace_book/"
     "/home/krisbalintona/Documents/org-database/notes/manuscripts/blog/"
     "/home/krisbalintona/.config/guix/current/share/guile/site/3.0/"
     "/home/krisbalintona/nix-config/"))
 '(safe-local-variable-values
   '((org-review-delay . "+3d")
     (eval add-hook 'org-insert-heading-hook
           'org-expiry-insert-created 99 t)
     (org-use-tag-inheritance)))
 '(xref-show-definitions-function 'xref-show-definitions-completing-read nil nil "Customized with use-package xref"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
