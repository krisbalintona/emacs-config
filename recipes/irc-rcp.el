;;; irc-rcp.el --- Summary
;;
;;; Commentary:
;;
;; IRC related configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Ement
;; Install `plz' HTTP library (not on MELPA yet).
(use-package plz :ensure (plz :type git :host github :repo "alphapapa/plz.el"))
;; Matrix client
(use-package ement
  :ensure (ement :type git :host github :repo "alphapapa/ement.el")
  :preface
  )

;;; irc-feed-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'irc-rcp)
