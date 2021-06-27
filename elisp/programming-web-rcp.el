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
         )
  :custom
  (handlebars-basic-offset 4)

  (general-define-key
   :keymaps 'handlebars-mode-map
   "C-x n s" 'outshine-narrow-to-subtree
   )
  )

;;; programming-web-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-web-rcp)
