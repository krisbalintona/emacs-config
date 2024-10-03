;;; better-defaults-rcp.el --- Better setq-default settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Set more sane Emacs-wide settings and minor QoL changes.

;;; Code:
(require 'personal-variables-rcp)

;;;; Customize
;; Show variable names in their lisp form
(setopt custom-unlispify-tag-names nil
        custom-safe-themes t)             ; Treat all themes as safe

;;;; No-littering
;; Set better default package paths
(use-package no-littering)
;; Set these variables prior to loading the feature
(setq no-littering-etc-directory (expand-file-name "data/" user-emacs-directory) ; Config files
      no-littering-var-directory (expand-file-name "var/" user-emacs-directory)) ; Persistent files
(when (fboundp 'elpaca-wait)
  (elpaca-wait))
(require 'no-littering)
(no-littering-theme-backups)            ; Sets various built-in variables

;;;; Enable all disabled commands
(customize-set-variable 'disabled-command-function nil "Enable all commands")

;;;; Disable startup echo message
;; See `startup-echo-area-message'
(fset #'display-startup-echo-area-message #'ignore)

;;;; Buffer-local defaults
(setq-default ad-redefinition-action 'accept                                                      ; Don’t warn when advice is added for functions
              confirm-kill-emacs 'y-or-n-p                                                        ; Confirm before killing emacs

              trash-directory (no-littering-expand-var-file-name "trash")                         ; Trash directory
              delete-by-moving-to-trash t                                                         ; Delete files to trash
              create-lockfiles nil                                                                ; Don't create lockfiles

              select-enable-clipboard t                                                           ; Merge system's and Emacs' clipboard

              sentence-end-double-space nil                                                       ; Single space after period denotes end of sentence

              fill-column 80                                                                      ; Set width for automatic line breaks

              truncate-string-ellipsis "…"                                                        ; For all ellipsis

              max-mini-window-height 0.3                                                          ; Max minibuffer height

              tab-width 4
              indent-tabs-mode nil

              visible-bell nil)

;;;; Cursor settings
(setq-default cursor-type '(bar . 4)
              cursor-in-non-selected-windows 'box
              x-stretch-cursor t)       ; Stretch cursor to the glyph width

;;;; Kill child processes without confirm
(custom-set-variables '(confirm-kill-processes nil))

;;;; Aviod cursor collisions
(mouse-avoidance-mode 'jump)      ; Avoid collision of mouse with point

;;;; Recognize camel case as words
(global-subword-mode t) ; Iterate through CamelCase words

;;;; Require pin-entry for passowrds
;; Pinentry is responsible for querying passphrases
(require 'epg-config)
(setq epg-pinentry-mode 'loopback) ; Ask through the minibuffer, instead of external Pinentry program

;;;; Don't confirm when killing a process
(setq confirm-kill-processes nil)

;;;; Ignore case for buffer and file names
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;;; Middle-click paste at point, not at cursor.
(setq mouse-yank-at-point t)

;;;; Toggle visiting of image files as images (Auto Image File mode).
(auto-image-file-mode t)

;;;; Show unfinished keystrokes quickly
;; Echo keystrokes (of unfinished commands) much quicker
(setq echo-keystrokes 0.5)

;;;; More leeway for Emacs subprocesses
;; Let Emacs subprocesses read more data per chunk
(setq read-process-output-max (* 4 1024 1024)) ; 4mb
;; Recommend here
;; https://www.reddit.com/r/emacs/comments/17nl7cw/comment/k7u1ueu/?utm_source=share&utm_medium=web2x&context=3
(setq process-adaptive-read-buffering nil)

;;;; Scrolling behavior
(setq scroll-preserve-screen-position t
      scroll-error-top-bottom nil
      scroll-margin 0
      next-screen-context-lines 12
      scroll-conservatively 0           ; Affects `scroll-step'
      scroll-minibuffer-conservatively t
      scroll-up-aggressively nil        ; Center after point leaves window
      scroll-down-aggressively nil)     ; Center after point leaves window

;;;; Do not load outdated byte code files
(setopt load-prefer-newer t)

;;;; Highlight next error
(setq next-error-message-highlight t)

;;;; Recenter upon `next-error'
(setq next-error-recenter '(4))

;;;; Delete-selection-mode
;; When selecting text, if typing new text, replace the selected text with the
;; new text
(delete-selection-mode t)

;;;; Repeatedly popping mark
(setq set-mark-command-repeat-pop t)

;;;; Case insensitive `auto-mode'
;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;;;; Keep all logged messages
(setq message-log-max t)

;;;; Don't do anything with inactive mark
(setq mark-even-if-inactive nil)

;;;; How we uniquify buffer names
(setq uniquify-buffer-name-style 'forward)

;;;; Indent and formatting
(setq-default left-fringe-width  8
              right-fringe-width 8)

;;;; Word wrapping
;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;;;; Truncate lines
;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)
;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;;;; Language environment and input method
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)
;; ...And the clipboard on Windows is often a wider encoding (UTF-16), so leave
;; Emacs to its own devices there.
(unless kb/sys-win
  (setq selection-coding-system 'utf-8))

;;;; Make right-click show context menu
(when (display-graphic-p)
  (context-menu-mode))

;;;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;;;; Only show these byte-compile warnings
(setopt byte-compile-warnings (cl-remove 'obsolete byte-compile-warning-types))

;;;; Defer font lock during input
(setopt jit-lock-defer-time 0)

;;;; Load custom file
;; Set and load custom file which contains persistent settings.
(with-eval-after-load 'no-littering
  (setq custom-file (no-littering-expand-var-file-name "custom.el"))
  (load custom-file))

(provide 'better-defaults-rcp)
;;; better-defaults-rcp.el ends here
