;;; better-defaults-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Set more sane Emacs-wide settings and minor QoL changes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;; Custom
;; Show variable names in their lisp form
(setq custom-unlispify-tag-names nil)

;;; No-littering
;; Set better default package paths
(require 'recentf)
(straight-use-package 'no-littering)
(setq no-littering-etc-directory (expand-file-name "data/" user-emacs-directory) ; Config files
      no-littering-var-directory (expand-file-name "var/" user-emacs-directory)) ; Persistent files
(require 'no-littering)                 ; Put after setting the variables I want
(no-littering-theme-backups)            ; Sets various built-in variables 

;;; Enable all disabled commands
(customize-set-variable disabled-command-function nil "Enable all commands")

;;; Disable startup echo message
;; See `startup-echo-area-message'
(fset #'display-startup-echo-area-message #'ignore)

;;; Buffer-local defaults
(setq-default ad-redefinition-action 'accept                                                      ; Don’t warn when advice is added for functions
              confirm-kill-emacs 'y-or-n-p                                                        ; Confirm before killing emacs

              inhibit-startup-screen t                                                            ; Disable start-up screen
              initial-scratch-message ";; Hi, Onii-chan~ ❀◕ ‿ ◕❀\n;; Let's have some fun...\n\n"  ; Set a cringe scratch buffer message

              x-stretch-cursor t                                                                  ; Stretch cursor to the glyph width
              cursor-in-non-selected-windows nil                                                  ; Hide the cursor in inactive windows

              trash-directory (concat no-littering-var-directory "trash")                         ; Trash directory
              delete-by-moving-to-trash t                                                         ; Delete files to trash

              find-file-visit-truename t                                                          ; Follow symlink to actual file
              create-lockfiles nil                                                                ; Don't create lockfiles

              select-enable-clipboard t                                                           ; Merge system's and Emacs' clipboard

              sentence-end-double-space nil                                                       ; Single space after period denotes end of sentence

              fill-column 80                                                                      ; Set width for automatic line breaks

              truncate-string-ellipsis "…"                                                        ; For all ellipsis

              max-mini-window-height 0.3                                                          ; Max minibuffer height

              tab-width 4
              indent-tabs-mode nil

              visible-bell nil
              ring-bell-function 'ignore)

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

      ;; Favor vertical splits over horizontal ones
      split-width-threshold 180
      split-height-threshold 80)

;;; More leeway for Emacs subprocesses
;; Let Emacs subprocesses read more data per chunk
(setq read-process-output-max (* 1024 1024)) ; 1mb

;;; Scrolling behavior
(setq scroll-preserve-screen-position 'always ; Preserve the posn-position of the point after scrolling
      scroll-error-top-bottom nil      ; Point shouldn't be at bottom of buffer
      scroll-margin 0                  ; Add a margin when scrolling vertically
      scroll-conservatively 0
      scroll-up-aggressively nil       ; Center
      scroll-down-aggressively nil     ; Center
      fast-but-imprecise-scrolling t)

;;; Do not load outdated byte code files
(setq load-prefer-newer t)

;;; Highlight next error
(setq next-error-message-highlight t)

;;; Delete-selection-mode
;; When selecting text, if typing new text, replace the selected text with the
;; new text
(delete-selection-mode t)

;;; Repeatedly popping mark
(setq set-mark-command-repeat-pop t)

;;; Case insensitive `auto-mode'
;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;;; Load custom file
;; Set and load custom file which contains persistent settings.
(with-eval-after-load 'no-littering
  (setq custom-file (no-littering-expand-var-file-name "custom.el"))
  (load custom-file))

;;; better-defaults-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'better-defaults-rcp)
