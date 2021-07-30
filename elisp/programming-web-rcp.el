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

;;;; Web-mode
;; Compatible with most template engines (e.g. handlebars mode, mustache) and
;; proper indentation based on content (i.e. CSS, HTML, JavaScript, or code).
(use-package web-mode
  :mode "\\.hbs\\'"
  :hook ((web-mode . electric-pair-mode)
         (web-mode . highlight-indent-guides-mode)
         (web-mode . display-line-numbers-mode)
         (web-mode . visual-line-mode)
         )
  :custom
  (web-mode-comment-formats '(("handlebars" . "{{!")
                              ("javascript" . "/*")
                              ("java" . "//")
                              ("typescript" . "//")
                              ("php" . "/*")
                              ("css" . "/*"))
                            )
  (web-mode-comment-style 2)
  :config
  (general-define-key
   :keymaps 'web-mode-map
   "C-x n s" 'outshine-narrow-to-subtree
   )
  )

;;;; Javascript
(use-package js2-mode
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
