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
  :hook (emacs-startup . (lambda ()
                           "Ensure my personal snippets dir is first and therefore the
default when creating snippets"
                           (push (no-littering-expand-etc-file-name "yasnippet/snippets") yas-snippet-dirs)))
  :init
  (yas-global-mode)
  )

;;; Doom-snippets
;; Large library of yasnippet templates
(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*"))
  )

;;; template-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'template-rcp)
