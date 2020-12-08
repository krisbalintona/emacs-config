;;; better-defaults-rcp.el
;;
;;; Code:

;; UnnecessaryUI
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; UnnecessaryUI

;;; VariableDefaults
(setq-default ad-redefinition-action 'accept             ; Don’t warn when advice is added for functions
              large-file-warning-threshold nil           ; Don't warn when opening large files
              auto-save-default nil                      ; Don't auto save, prevents transitory files from being saved
              auto-save-list-file-prefix nil             ; Prevent tracking for auto-saves

              inhibit-startup-screen t                   ; Disable start-up screen
              initial-scratch-message ";; Hi, Onii-chan~ ❀◕ ‿ ◕❀\n;; Let's have some fun...\n\n" ; Set a cringe scratch buffer message

              x-stretch-cursor t                         ; Stretch cursor to the glyph width
              cursor-in-non-selected-windows nil         ; Hide the cursor in inactive windows
              mouse-yank-at-point t                      ; Yank at point rather than pointer

              delete-by-moving-to-trash t                ; Delete files to trash

              scroll-conservatively most-positive-fixnum ; Always scroll by one line
              scroll-margin 7                            ; Add a margin when scrolling vertically

              select-enable-clipboard t                  ; Merge system's and Emacs' clipboard

              sentence-end-double-space nil              ; Single space after period denotes end of sentence

              show-help-function nil                     ; Disable help text on most UI elements

              uniquify-buffer-name-style 'forward        ; Uniquify buffer names
              window-combination-resize t                ; Resize windows proportionally
              split-height-threshold nil                 ; Threshiold for vertical window splitting
              split-width-threshold 160                  ; Threshold for horizontal window splitting
              recenter-positions '(5 top bottom)         ; Set re-centering positions
              help-window-select t                       ; Focus new help windows when opened
              fill-column 80                             ; Set width for automatic line breaks

              truncate-string-ellipsis "…"               ; For all elliipsis

              max-mini-window-height 0.15                ; Minibuffer height

              make-backup-files nil                      ; Don't make backups
              create-lockfiles nil                       ; Don't create lockfiles
              find-file-existing-other-name t            ; Visit buffer when finding file if it already exists?

              tab-width 4
              evil-shift-width tab-width
              indent-tabs-mode nil

              visible-bell nil
              )
;;; VariableDefaults

;;; ThinnerVerticalFringes
(fringe-mode '(5 . 5))
;;; ThinnerVerticalFringes

;;; ReplaceWithYorN
(fset 'yes-or-no-p 'y-or-n-p)
;;; ReplaceWithYorN

;;; AviedCursorCollisions
(mouse-avoidance-mode 'jump)      ; Avoid collision of mouse with point
;;; AviedCursorCollisions

;;: DownAndUpCase
(put 'downcase-region 'disabled nil) ; Enable downcase-region
(put 'upcase-region 'disabled nil)   ; Enable upcase-region
;;: DownAndUpCase

;;; ConfirmQuit
(setq confirm-kill-emacs 'y-or-n-p) ; Confirm before killing emacs
;;; ConfirmQuit

;;; KillChildProcessesWithoutConfirm
(custom-set-variables '(confirm-kill-processes nil))
;;; KillChildProcessesWithoutConfirm

;;; IgnoreCase
(setq completion-ignore-case t)
(custom-set-variables
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t))
;;; IgnoreCase

;;; RecognizeCamelCase
(global-subword-mode t) ; Iterate through CamelCase words
;;; RecognizeCamelCase

;;: ESCQuitEverywhere
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;: ESCQuitEverywhere

 ;;; UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
 ;;; UTF-8

;;; UpdateTimestamp
(add-hook 'before-save-hook 'time-stamp) ; or (add-hook 'write-file-functions
;;; UpdateTimestamp

;;; HighlightLineInAgenda
(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode t)))
;;; HighlightLineInAgenda

(provide 'better-defaults-rcp)
;;; Commentary:
;; Set more sane defaults for Emacs as well as other QoL modes. These settings are
;; package-agnostic
;;
;;; better-defaults-rcp.el ends here
