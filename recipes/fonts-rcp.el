;;; fonts-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to fonts and faces.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Unicode-fonts
;; Support unicode characters
(use-package unicode-fonts
  ;; When `unicode-fonts-setup' is run it rebuilds the disk cache during Emacs
  ;; startup whenever a font is added or removed, or any relevant configuration
  ;; variables are changed.
  :hook ((window-setup server-after-make-frame) . unicode-fonts-setup)
  :preface
  ;; Dependencies, though other packages have probably already installed them.
  (use-package font-utils :demand t)
  (use-package ucs-utils :demand t)
  (use-package list-utils :demand t)
  )

;;; All-the-icons
;; Provides a bunch of unicode icons which many other packages leverage
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.1)
  )

;;; Mixed-pitch
;; Allow the same buffer to have both fixed- and variable-pitch
;; NOTE Changes the family and height of the default face to the family and
;; height of the variable-pitch face
(use-package mixed-pitch
  :ghook 'text-mode-hook
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces
               '(highlight-indent-guides-character-face ; Highlight-indent-guides
                 ;; Ace-jump
                 ace-jump-face-foreground
                 ace-jump-face-background
                 ))
  )

;;; Default-text-scale
;; Text-scale-mode but Emacs-wide
(use-package default-text-scale)

;;; fonts-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'fonts-rcp)
