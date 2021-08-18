;;; better-defaults-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Set more sane Emacs-wide defaults for Emacs as well as other QoL modes. These
;; settings are package-agnostic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'early-packages-rcp)

;;;; Remove unnecessary UI
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;;; Variable defaults
(setq-default ad-redefinition-action 'accept                                                      ; Don’t warn when advice is added for functions
              large-file-warning-threshold nil                                                    ; Don't warn when opening large files
              auto-save-default nil                                                               ; Don't auto save, prevents transitory files from being saved
              auto-save-list-file-prefix nil                                                      ; Prevent tracking for auto-saves

              inhibit-startup-screen t                                                            ; Disable start-up screen
              initial-scratch-message ";; Hi, Onii-chan~ ❀◕ ‿ ◕❀\n;; Let's have some fun...\n\n"  ; Set a cringe scratch buffer message

              x-stretch-cursor t                                                                  ; Stretch cursor to the glyph width
              cursor-in-non-selected-windows nil                                                  ; Hide the cursor in inactive windows
              mouse-yank-at-point t                                                               ; Yank at point rather than pointer

              trash-directory (concat no-littering-var-directory "trash")                         ; Trash directory
              delete-by-moving-to-trash t                                                         ; Delete files to trash

              find-file-visit-truename t                                                          ; Follow symlink to actual file

              scroll-conservatively most-positive-fixnum                                          ; Always scroll by one line
              scroll-margin 7                                                                     ; Add a margin when scrolling vertically
              line-spacing 1                                                                      ; This is the default line spacing
              mouse-wheel-tilt-scroll t                                                           ; Enable horizontal scrolling with mouse

              select-enable-clipboard t                                                           ; Merge system's and Emacs' clipboard

              sentence-end-double-space nil                                                       ; Single space after period denotes end of sentence

              show-help-function nil                                                              ; Disable help text on most UI elements

              uniquify-buffer-name-style 'forward                                                 ; Uniquify buffer names
              window-combination-resize t                                                         ; Resize windows proportionally
              split-height-threshold nil                                                          ; Threshiold for vertical window splitting
              split-width-threshold 160                                                           ; Threshold for horizontal window splitting
              recenter-positions '(5 top bottom)                                                  ; Set re-centering positions
              help-window-select t                                                                ; Focus new help windows when opened
              fill-column 80                                                                      ; Set width for automatic line breaks

              truncate-string-ellipsis "…"                                                        ; For all elliipsis

              max-mini-window-height 0.15                                                         ; Minibuffer height

              make-backup-files nil                                                               ; Don't make backups
              create-lockfiles nil                                                                ; Don't create lockfiles
              find-file-existing-other-name t                                                     ; Visit buffer when finding file if it already exists?

              tab-width 4
              evil-shift-width tab-width
              indent-tabs-mode nil

              visible-bell nil
              ring-bell-function 'ignore                                                          ; Disable annoying error sound on Windows 10

              confirm-kill-emacs 'y-or-n-p                                                        ; Confirm before killing emacs
              )

;;;; Thinner vertical fringes
(fringe-mode '(5 . 5))

;;;; Make asking "Y or N"
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Aviod cursor collisions
(mouse-avoidance-mode 'jump)      ; Avoid collision of mouse with point

;;;; Display line numbers
(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :config
  (column-number-mode) ; Column number in modeline

  ;; Enabled for these
  (dolist (mode '(prog-mode-hook
                  LaTeX-mode-hook
                  ))
    (add-hook mode (lambda () (display-line-numbers-mode 1))))


  ;; Disabled for these
  (dolist (mode '(org-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  )

;;;; Kill child processes without confirm
(custom-set-variables '(confirm-kill-processes nil))

;;;; Ignore case
(setq completion-ignore-case t)
(custom-set-variables
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t))

;;;; Recognize camel case
(global-subword-mode t) ; Iterate through CamelCase words

;;;; Update timestamp
(add-hook 'before-save-hook 'time-stamp) ; or (add-hook 'write-file-functions

;;;; Highlight line in org-agenda
(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode t)))

;;;; Pinentry (pin-entry)
;; Pinentry is responsible for querying passphrases
(require 'epg)
(setq epg-pinentry-mode 'loopback) ; Ask through the minibuffer, instead of external Pinentry program

;;;; ESC everywhere
;; Make ESC quit prompts everywhere
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;;; Size-indication-mode
;; Show file-size
(use-package simple
  :straight nil
  :hook (after-init. size-indication-mode)
  )

;;;; Load custom file
;; Must be loaded after early-packages-rcp because that is where custom-file
;; location is defined
(when (file-exists-p custom-file)
  (load custom-file))

;;; better-defaults-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'better-defaults-rcp)
