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

;;; Org-tempo
;; Completion for org-block types. Adds to the functionality of `org-structure'.
(use-package org-tempo
  :straight nil
  :after org
  :config
  ;; Add additional org-block types
  (dolist (expansion '(;; ("sc" . "src scheme")
                       ;; ("ts" . "src typescript")
                       ;; ("yaml" . "src yaml")
                       ;; ("json" . "src json")
                       )
                     org-structure-template-alist)
    (push expansion org-structure-template-alist))
  )

;;; Yasnippet
;; Template-expansion system (doesn't include templates)
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (advice-add 'doom-snippets-initialize
              :after #'(lambda ()
                         "Ensure my personal snippets dir is first and therefore the
default when creating snippets. Then start `yas-global-mode'."
                         (add-to-list 'yas-snippet-dirs (no-littering-expand-etc-file-name "yasnippet/snippets")))))


;;; Doom-snippets
;; Large library of yasnippet templates
(use-package doom-snippets
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

;;; template-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'template-rcp)
