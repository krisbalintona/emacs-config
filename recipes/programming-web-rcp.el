;;; programming-web-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages that are relevant to web development.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'convenient-functions-rcp)

;;; Web-mode
;; Compatible with most template engines (e.g. handlebars mode, mustache) and
;; proper indentation based on content (i.e. CSS, HTML, JavaScript, or code).
(use-package web-mode
  :ensure-system-package (handlebars . "sudo npm --global install handlebars") ; For
                                                                               ; ghost
  :mode ("\\.hbs\\'"                    ; For ghost
         "\\.yaml\\'"
         "\\.html\\'")
  :gfhook
  'highlight-indent-guides-mode
  'display-line-numbers-mode
  'visual-line-mode
  :custom
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 4)
  (web-mode-comment-style 1)

  (web-mode-engines-alist
   '(("go" . "\\.html\\'")))            ; For hugo

  ;; Features
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t) ; CSS colorization
  (web-mode-enable-block-face t) ; Block face: set blocks background and default
                                 ; foreground
  (web-mode-enable-part-face t) ; Part face: set parts background and default
                                ; foreground
  (web-mode-enable-comment-interpolation nil) ; Font lock comment keywords
  (web-mode-enable-heredoc-fontification t) ; Heredoc (cf. PHP strings)
                                            ; fontification

  ;; Other
  (flycheck-handlebars-executable (executable-find "handlebars"))
  :config
  (setf (alist-get "handlebars" web-mode-comment-formats nil nil 'string=) '("{{!")))

;;; CSS-mode
(use-package css-mode
  :ensure-system-package (stylelint . "sudo npm install --global --save-dev stylelint stylelint-config-standard")
  :custom
  (css-indent-offset 2)

  (flycheck-css-stylelint-executable (executable-find "stylelint"))
  (flycheck-stylelintrc (concat no-littering-var-directory "flycheck/.stylelintrc.json"))
  (flycheck-stylelint-quiet nil)
  )

;;; Js2-mode
(use-package js2-mode
  :ensure-system-package ((eslint . "sudo npm install --global --save-dev eslint")
                          (semistandard . "sudo npm install --global semistandard"))
  :mode "\\.js\\'"
  :gfhook
  'highlight-indent-guides-mode
  'display-line-numbers-mode
  'visual-line-mode
  :custom
  (js-indent-level 4)

  (flycheck-checker-error-threshold 10000)
  (flycheck-javascript-standard-executable (executable-find "semistandard"))
  (flycheck-javascript-eslint-executable (executable-find "eslint")))

;;; Json-mode
(use-package json-mode
  :ensure-system-package (jsonlint . "sudo npm install --global jsonlint")
  :custom
  (flycheck-json-jsonlint-executable (executable-find "jsonlint")))

;;; Yaml-mode
(use-package yaml-mode
  :ensure-system-package (js-yaml . "sudo npm install --global js-yaml")
  :gfhook
  'highlight-indent-guides-mode
  'display-line-numbers-mode
  'visual-line-mode
  :general (:keymaps 'yaml-mode-map
                     [remap evil-indent] 'yaml-indent-line)
  :custom
  (flycheck-yaml-jsyaml-executable (executable-find "js-yaml")))

;;; Toml-mode
(use-package toml-mode)

;;; programming-web-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-web-rcp)
