;; -*- lexical-binding: t; -*-

;;; Ligature
;; Ligatures!  Be aware that this differs from `prettify-symbols-mode' because
;; ligatures are provided by and must be supported by the particular font.  See
;; for configuration examples: https://github.com/j/wiki
(use-package ligature
  :hook (window-setup . global-ligature-mode)
  :config
  ;; Enables simple HTML ligations for web-related major modes using the string
  ;; notation to create ligations
  (ligature-set-ligatures
   '(html-mode nxml-mode web-mode)
   '("<!--" "-->" "</>" "</" "/>" "://"))

  ;; Enable all Iosevka ligatures in programming modes.  Taken from
  ;; https://github.com/mickeynp/ligature.el/wiki#iosevka
  (ligature-set-ligatures
   '(prog-mode conf-mode)
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
     "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
     "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
     ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

;;; Astute.el
;; Redisplay typographical punctuation (e.g., em-dashes as "—" and en-dashes as
;; "–")
(use-package astute
  :hook (text-mode . astute-mode)
  :custom
  (astute-lighter "")
  (astute-prefix-single-quote-exceptions
   '("bout"
     "em"
     "n'"
     "cause"
     "round"
     "twas"
     "tis")))

;;; Prettify-symbols-mode
(setopt prettify-symbols-unprettify-at-point 'right-edge)

;; Org-mode
(defun krisb-prettify-symbols--org-mode-setup ()
  "Set up pretty symbols in `org-mode'."
  (add-to-list 'prettify-symbols-alist '("->" . ?→))
  (add-to-list 'prettify-symbols-alist '("<-" . ?←)))
(add-hook 'org-mode-hook #'krisb-prettify-symbols--org-mode-setup)

;; Prog-mode
(defun krisb-prettify-symbols--prog-mode-setup ()
  "Set up pretty symbols in `prog-mode'."
  (add-to-list 'prettify-symbols-alist '("->" . ?→))
  (add-to-list 'prettify-symbols-alist '("<-" . ?←))
  (add-to-list 'prettify-symbols-alist '("<->" . ?↔))
  (add-to-list 'prettify-symbols-alist '("lambda" . ?λ)))
(add-hook 'prog-mode-hook #'krisb-prettify-symbols--prog-mode-setup)

;;; Show-font
;; Best font previewer
(use-package show-font)

;;; Default-text-scale
;; Text-scale-mode but Emacs-wide
(use-package default-text-scale)

;;; Provide
(provide 'krisb-fonts)
