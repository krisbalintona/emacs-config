;;; template-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to template expansion.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Yasnippet
;; Template-expansion system (doesn't include templates)
(use-package yasnippet
  :diminish yas-minor-mode
  :custom
  (yas-alias-to-yas/prefix-p nil)
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines nil)
  (yas-choose-tables-first t)
  (yas-inhibit-overlay-modification-protection nil)
  (yas-snippet-revival t)
  (yas-triggers-in-field nil)
  :init
  (yas-global-mode))

;;; Consult-yasnippet
(use-package consult-yasnippet
  :after yasnippet
  :general
  ([remap yas-insert-snippet] 'consult-yasnippet
   [remap yas-visit-snippet-file] 'consult-yasnippet-visit-snippet-file))

;;; Tempel
;; Small and simple snippet/template system compatible with corfu.
(use-package tempel
  :disabled                             ; Migrate to yasnippet
  :general
  ("M-+" 'tempel-complete               ; List all available templates
   "M-*" 'tempel-insert)                ; Insert typed template
  (:keymaps 'tempel-map
   "C-M-c" 'tempel-done)
  :custom
  (tempel-file (no-littering-expand-var-file-name "tempel-templates")))

;;; template-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'template-rcp)
