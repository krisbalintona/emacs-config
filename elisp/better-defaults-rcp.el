;;; better-defaults-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Set more sane Emacs-wide settings and minor QoL changes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'straight-package-management-rcp)

;;;; No-littering
;; Set better default package paths
(require 'recentf)
(straight-use-package 'no-littering)
(require 'no-littering)
(setq no-littering-etc-directory (expand-file-name "data/" user-emacs-directory) ; Config files
      no-littering-var-directory (expand-file-name "var/" user-emacs-directory)) ; Persistent files

;;;; Reenable these disabled commands
(put 'narrow-to-region 'disabled nil) ; `narrow-to-region'

;;;; Buffer-local defaults
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
              line-spacing 0.1                                                                    ; Additional spacing between lines
              mouse-wheel-tilt-scroll t                                                           ; Enable horizontal scrolling with mouse

              select-enable-clipboard t                                                           ; Merge system's and Emacs' clipboard

              sentence-end-double-space nil                                                       ; Single space after period denotes end of sentence

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

;;;; Kill child processes without confirm
(custom-set-variables '(confirm-kill-processes nil))

;;;; Make asking "Y or N"
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Aviod cursor collisions
(mouse-avoidance-mode 'jump)      ; Avoid collision of mouse with point

;;;; Recognize camel case as words
(global-subword-mode t) ; Iterate through CamelCase words

;;;; Require pin-entry for passowrds
;; Pinentry is responsible for querying passphrases
(require 'epg)
(setq epg-pinentry-mode 'loopback) ; Ask through the minibuffer, instead of external Pinentry program

;;; better-defaults-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'better-defaults-rcp)
