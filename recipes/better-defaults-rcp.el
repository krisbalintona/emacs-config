;;; better-defaults-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Set more sane Emacs-wide settings and minor QoL changes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;; No-littering
;; Set better default package paths
(require 'recentf)
(straight-use-package 'no-littering)
(require 'no-littering)
(setq no-littering-etc-directory (expand-file-name "data/" user-emacs-directory) ; Config files
      no-littering-var-directory (expand-file-name "var/" user-emacs-directory)) ; Persistent files

;;; Reenable these disabled commands
(put 'narrow-to-region 'disabled nil) ; `narrow-to-region'

;;; Disable startup echo message
;; See `startup-echo-area-message'
(fset #'display-startup-echo-area-message #'ignore)

;;; Buffer-local defaults
(setq-default ad-redefinition-action 'accept                                                      ; Don’t warn when advice is added for functions
              confirm-kill-emacs 'y-or-n-p                                                        ; Confirm before killing emacs
              large-file-warning-threshold nil                                                    ; Don't warn when opening large files

              inhibit-startup-screen t                                                            ; Disable start-up screen
              initial-scratch-message ";; Hi, Onii-chan~ ❀◕ ‿ ◕❀\n;; Let's have some fun...\n\n"  ; Set a cringe scratch buffer message
              inhibit-default-init t

              x-stretch-cursor t                                                                  ; Stretch cursor to the glyph width
              cursor-in-non-selected-windows nil                                                  ; Hide the cursor in inactive windows

              trash-directory (concat no-littering-var-directory "trash")                         ; Trash directory
              delete-by-moving-to-trash t                                                         ; Delete files to trash

              find-file-visit-truename t                                                          ; Follow symlink to actual file

              scroll-preserve-screen-position 'always                                             ; Preserve the posn-position of the point after scrolling
              scroll-conservatively most-positive-fixnum                                          ; Always scroll by one line
              scroll-margin 7                                                                     ; Add a margin when scrolling vertically

              select-enable-clipboard t                                                           ; Merge system's and Emacs' clipboard

              sentence-end-double-space nil                                                       ; Single space after period denotes end of sentence

              help-window-select t                                                                ; Focus new help windows when opened
              fill-column 80                                                                      ; Set width for automatic line breaks

              truncate-string-ellipsis "…"                                                        ; For all ellipsis

              max-mini-window-height 0.15                                                         ; Minibuffer height

              auto-save-default nil                                                               ; Don't auto save, prevents transitory files from being saved
              auto-save-list-file-prefix nil                                                      ; Prevent tracking for auto-saves
              make-backup-files nil                                                               ; Don't make backups
              create-lockfiles nil                                                                ; Don't create lockfiles
              find-file-existing-other-name t                                                     ; Visit buffer when finding file if it already exists?

              tab-width 4
              evil-shift-width tab-width
              indent-tabs-mode nil

              visible-bell nil
              ring-bell-function 'ignore                                                          ; Disable annoying error sound on Windows 10
              )

;;; Kill child processes without confirm
(custom-set-variables '(confirm-kill-processes nil))

;;; Make asking "Y or N"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Aviod cursor collisions
(mouse-avoidance-mode 'jump)      ; Avoid collision of mouse with point

;;; Recognize camel case as words
(global-subword-mode t) ; Iterate through CamelCase words

;;; Require pin-entry for passowrds
;; Pinentry is responsible for querying passphrases
(require 'epg)
(setq epg-pinentry-mode 'loopback) ; Ask through the minibuffer, instead of external Pinentry program

;;; Don't confirm when killing a process
(setq confirm-kill-processes nil)

;;; Ignore case for buffer and file names
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;; Have window-setup hooks run on server-make-frame-hook
;; Otherwise those `window-setup-hook's won't be run at startup for Emacs
;; daemons.
(add-hook 'server-after-make-frame-hook #'(lambda () (run-hooks 'window-setup-hook)))

;;; Middle-click paste at point, not at cursor.
(setq mouse-yank-at-point t)

;;; Toggle visiting of image files as images (Auto Image File mode).
(auto-image-file-mode t)

;;; Show unfinished keystrokes quickly
;; Echo keystrokes (of unfinished commands) much quicker
(setq echo-keystrokes 0.5)

;;; Window sizing and splitting
(setq window-resize-pixelwise t
      fit-window-to-buffer-horizontally t
      fit-frame-to-buffer t

      ;; Favor vertical splits over horizontal ones
      split-width-threshold 180
      split-height-threshold 80
      )

;;; More leeway for Emacs subprocesses
;; Let Emacs subprocesses read more data per chunk
(setq read-process-output-max (* 1024 1024)) ; 1mb

;;; Load custom file
;; Set and load custom file which contains persistent settings.
(with-eval-after-load 'no-littering
  (setq custom-file (no-littering-expand-var-file-name "custom.el"))
  (load custom-file))

;;; better-defaults-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'better-defaults-rcp)
