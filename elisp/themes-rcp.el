;;; themes-rcp.el --- Summary
;;
;; Here are all the themes that interest me. One is enabled and all the others
;; are disabled.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(use-package doom-themes
  :disabled t
  :config (load-theme 'doom-dracula t))

(use-package doom-themes
  :disabled t
  :config (load-theme 'doom-palenight t))

(use-package atom-one-dark-theme
  :config (load-theme 'atom-one-dark t))

(use-package mood-one-theme
  :disabled t
  :config (load-theme 'mood-one t))

(use-package spacemacs-theme
  :disabled t
  :config (load-theme 'spacemacs-dark t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'themes-rcp)
;;; Commentary:
;;
;;; themes-rcp.el ends here
