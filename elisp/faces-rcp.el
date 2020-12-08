;;; faces-rcp.el --- Summary
;;
;; These are the base faces I used across all of Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

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
(defvar kb/variable-pitch-font "ETBembo")
(defvar kb/modeline-font "Noto Sans")

;;;; Setup fonts
(defun kb/default-fonts-setup ()
  "Set Emacs-wide fonts."

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

;;;; Add to hooks
;; Set fonts based on if daemon or not
(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'kb/default-fonts-setup)
  (add-hook 'window-setup-hook 'kb/default-fonts-setup))

;;;; Mixed-pitch
;; Allow faces to be selectively fixed- or variable-pitch
;; Note: Changes the family and height of the default face to the family and
;; height of the variable-pitch face
(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode)
  :custom
  (mixed-pitch-fixed-pitch-faces
   '(diff-added diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face line-number line-number-current-line line-number-major-tick line-number-minor-tick markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim

                highlight-indent-guides-character-face
                ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'faces-rcp)
;;; Commentary:
;;
;;; faces-rcp.el ends here
