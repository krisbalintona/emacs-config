;;; programming-web-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages that are relevant to web development.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)
(require 'convenient-functions-rcp)

;;;; Web-mode
;; Compatible with most template engines (e.g. handlebars mode, mustache) and
;; proper indentation based on content (i.e. CSS, HTML, JavaScript, or code).
(use-package web-mode
  :ensure-system-package (handlebars . "sudo npm --global install handlebars")
  :mode ("\\.hbs\\'"
         "\\.yaml\\'")
  :gfhook
  'electric-pair-mode
  'highlight-indent-guides-mode
  'display-line-numbers-mode
  'visual-line-mode
  :custom
  (web-mode-comment-formats '(("handlebars" . "{{!")
                              ("javascript" . "/*")
                              ("java" . "//")
                              ("typescript" . "//")
                              ("php" . "/*")
                              ("css" . "/*"))
                            )
  (web-mode-comment-style 2)

  (flycheck-handlebars-executable (kb/shell-command-to-string "which handlebars"))
  )

;;;; CSS-mode
(use-package css-mode
  :ensure-system-package (stylelint . "sudo npm install --global --save-dev stylelint stylelint-config-standard")
  :gfhook 'electric-pair-mode
  :custom
  (css-indent-offset 2)

  (flycheck-css-stylelint-executable (kb/shell-command-to-string "which stylelint"))
  (flycheck-stylelintrc (concat no-littering-var-directory "flycheck/.stylelintrc.json"))
  (flycheck-stylelint-quiet nil)
  )

;;;; Javascript
(use-package js2-mode
  :ensure-system-package ((eslint . "sudo npm install --global --save-dev eslint")
                          (semistandard . "sudo npm install --global semistandard"))
  :mode "\\.js\\'"
  :gfhook
  'electric-pair-mode
  'highlight-indent-guides-mode
  'display-line-numbers-mode
  'visual-line-mode
  :custom
  (js-indent-level 2)

  (flycheck-checker-error-threshold 10000)
  (flycheck-javascript-standard-executable (kb/shell-command-to-string "which semistandard"))
  (flycheck-javascript-eslint-executable (kb/shell-command-to-string "which eslint"))
  )

;;;; Json
(use-package json-mode
  :ensure-system-package (jsonlint . "sudo npm install --global jsonlint")
  :custom
  (flycheck-json-jsonlint-executable (kb/shell-command-to-string "which jsonlint"))
  )

;;;; Yaml
(use-package yaml-mode
  :ensure-system-package (js-yaml . "sudo npm install --global js-yaml")
  :gfhook
  'highlight-indent-guides-mode
  'display-line-numbers-mode
  'visual-line-mode
  :general (:keymaps 'yaml-mode-map
                     [remap evil-indent] 'yaml-indent-line)
  :custom
  (flycheck-yaml-jsyaml-executable (kb/shell-command-to-string "which js-yaml"))
  )

;;; programming-web-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-web-rcp)
