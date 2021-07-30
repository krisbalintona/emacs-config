;;; programming-web-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages that are relevant to web development.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; CSS-mode
(use-package css-mode
  :hook ((css-mode . electric-pair-mode)
         )
  :custom
  (css-indent-offset 4)
  :config
  (general-define-key
   :keymaps 'css-mode-map
   "C-x n s" 'outshine-narrow-to-subtree
   )
  )

;;;; Handlebars-mode
(use-package handlebars-mode
  :hook ((handlebars-mode . electric-pair-mode)
         (handlebars-mode . highlight-indent-guides-mode)
         (handlebars-mode . display-line-numbers-mode)
         (handlebars-mode . visual-line-mode)
         )
  :custom
  (handlebars-basic-offset 4)
  :config
  (general-define-key
   :keymaps 'handlebars-mode-map
   "C-x n s" 'outshine-narrow-to-subtree
   )
  )

;;;; Javascript
(use-package js-mode
  :straight nil
  :hook ((js-mode . electric-pair-mode)
         (js-mode . highlight-indent-guides-mode)
         (js-mode . display-line-numbers-mode)
         (js-mode . visual-line-mode)
         )
  :config
  (general-define-key
   :keymaps 'js-mode-map
   "C-x n s" 'outshine-narrow-to-subtree
   )
  )

;;;; Yaml
(use-package yaml
  :config
  (general-define-key
   :keymaps 'yaml-mode-map
   [remap evil-indent] 'yaml-indent-line
   )
  )

;;; programming-web-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-web-rcp)
