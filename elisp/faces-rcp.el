;;; faces-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All the faces and fonts I use.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)

;;;; Face definitions
;; Potential Fonts:
;; Default:
;; - RobotoMono Nerd Font
;; - Iosevka Fixed Slab=
;; - Iosevka Term SS04=
;; Fixed-Pitch:
;; - FiraCode Nerd Font
;; - Hack Nerd Font Mono
;; Variable-Pitch:
;; - Noto Sans
;; - IBM Plex Sans KR (Note: This doesn't have italics available)
;; - IBM Plex Sans Condensed
(defvar kb/default-font "Iosevka Term SS04")
(defvar kb/fixed-pitch-font "Hack Nerd Font Mono")
(defvar kb/variable-pitch-font "LiterationSerif Nerd Font")
;; (defvar kb/variable-pitch-font "Garamond-Math")
;; (defvar kb/variable-pitch-font "ETBembo")
;; (defvar kb/modeline-font "Noto Sans")
(defvar kb/modeline-font "NotoSans Nerd Font")

;;;; Setup fonts
(defun kb/default-fonts-setup ()
  "Set Emacs-wide fonts."
  (interactive)
  (add-to-list 'default-frame-alist `(font . ,kb/default-font))

  (set-face-attribute 'default nil :font kb/default-font :height 135)
  (set-face-attribute 'fixed-pitch nil :font kb/fixed-pitch-font :height 140)
  ;; (set-face-attribute 'variable-pitch nil :font kb/variable-pitch-font :height 148)
  (set-face-attribute 'variable-pitch nil :font kb/variable-pitch-font :height 158)

  (set-fontset-font ; Emoji support
   t
   '(#x1f300 . #x1fad0)
   (cond
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola")
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
    ;; GNU Emacs Removes Color Emoji Support on the Mac
    ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
    ))
  )
(general-add-hook '(server-after-make-frame-hook window-setup-hook) 'kb/default-fonts-setup)

;;;; Mixed-pitch
;; Allow faces to be selectively fixed- or variable-pitch
;; Note: Changes the family and height of the default face to the family and
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

;;; faces-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'faces-rcp)
