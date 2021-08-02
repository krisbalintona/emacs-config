;;; programming-web-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages that are relevant to web development.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Web-mode
;; Compatible with most template engines (e.g. handlebars mode, mustache) and
;; proper indentation based on content (i.e. CSS, HTML, JavaScript, or code).
(use-package web-mode
  :mode ("\\.hbs\\'"
         "\\.yaml\\'"
         )
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

  (flycheck-handlebars-executable "/usr/local/bin/handlebars")
  :config
  (general-define-key
   :keymaps 'web-mode-map
   "C-x n s" 'outshine-narrow-to-subtree
   )
  )

;;;; CSS-mode
(use-package css-mode
  :hook ((css-mode . electric-pair-mode)
         )
  :custom
  (css-indent-offset 2)

  (flycheck-css-stylelint-executable "/usr/local/bin/stylelint")
  (flycheck-stylelintrc (concat no-littering-var-directory "flycheck/.stylelintrc.json"))
  (flycheck-stylelint-quiet nil)
  :config
  (general-define-key
   :keymaps 'css-mode-map
   "C-x n s" 'outshine-narrow-to-subtree
   )
  )

;;;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :hook ((js2-mode . electric-pair-mode)
         (js2-mode . highlight-indent-guides-mode)
         (js2-mode . display-line-numbers-mode)
         (js2-mode . visual-line-mode)
         )
  :custom
  (flycheck-javascript-standard-executable "/usr/local/bin/semistandard")
  (js-indent-level 2)
  (flycheck-checker-error-threshold 10000)

  (flycheck-javascript-eslint-executable "~/node_modules/.bin/eslint")
  :config
  (general-define-key
   :keymaps 'js2-mode-map
   "C-x n s" 'outshine-narrow-to-subtree
   )
  )

;;;; Json
(use-package json-mode
  :custom
  (flycheck-json-jsonlint-executable "/usr/local/bin/jsonlint")
  )

;;;; Yaml
(use-package yaml-mode
  :hook ((yaml-mode . highlight-indent-guides-mode)
         (yaml-mode . display-line-numbers-mode)
         (yaml-mode . visual-line-mode)
         )
  :custom
  (flycheck-yaml-jsyaml-executable "/usr/local/bin/js-yaml")
  :config
  (general-define-key
   :keymaps 'yaml-mode-map
   [remap evil-indent] 'yaml-indent-line
   )
  )

;;; programming-web-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-web-rcp)
