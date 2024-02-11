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

;;;; Custom
;; Show variable names in their lisp form
(setq custom-unlispify-tag-names nil)

;;;; No-littering
;; Set better default package paths
(use-package no-littering)
;; Set these variables prior to loading the feature
(setq no-littering-etc-directory (expand-file-name "data/" user-emacs-directory) ; Config files
      no-littering-var-directory (expand-file-name "var/" user-emacs-directory)) ; Persistent files
(elpaca-wait)
(require 'no-littering)
(no-littering-theme-backups)            ; Sets various built-in variables

;;;; Enable all disabled commands
(customize-set-variable disabled-command-function nil "Enable all commands")

;;;; Disable startup echo message
;; See `startup-echo-area-message'
(fset #'display-startup-echo-area-message #'ignore)

;;;; Buffer-local defaults
(setq-default ad-redefinition-action 'accept                                                      ; Don’t warn when advice is added for functions
              confirm-kill-emacs 'y-or-n-p                                                        ; Confirm before killing emacs

              initial-scratch-message ";; Hi, Onii-chan~ ❀◕ ‿ ◕❀\n;; Let's have some fun...\n\n"  ; Set a cringe scratch buffer message

              trash-directory (no-littering-expand-var-file-name "trash")                         ; Trash directory
              delete-by-moving-to-trash t                                                         ; Delete files to trash

              find-file-visit-truename t                                                          ; Follow symlink to actual file
              create-lockfiles nil                                                                ; Don't create lockfiles

              select-enable-clipboard t                                                           ; Merge system's and Emacs' clipboard

              sentence-end-double-space nil                                                       ; Single space after period denotes end of sentence

              fill-column 80                                                                      ; Set width for automatic line breaks

              truncate-string-ellipsis "…"                                                        ; For all ellipsis

              max-mini-window-height 0.3                                                          ; Max minibuffer height

              tab-width 8
              indent-tabs-mode nil

              visible-bell nil
              ring-bell-function 'ignore)

;;;; Cursor settings
(setq-default cursor-type 'bar
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
(require 'epg)
(setq epg-pinentry-mode 'loopback ; Ask through the minibuffer, instead of external Pinentry program
      epa-file-cache-passphrase-for-symmetric-encryption t)

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
(setq read-process-output-max (* 1024 1024)) ; 1mb

;;;; Scrolling behavior
(setq scroll-preserve-screen-position t
      scroll-error-top-bottom nil
      scroll-margin 0
      next-screen-context-lines 12
      scroll-conservatively 0           ; Affects `scroll-step'
      scroll-minibuffer-conservatively t
      scroll-up-aggressively nil        ; Center after point leaves window
      scroll-down-aggressively nil      ; Center after point leaves window
      ;; These are the three ways to increase scrolling performance.
      ;; See (info "(emacs) Scrolling") for details
      fast-but-imprecise-scrolling t
      ;; jit-lock-defer-time 0.2
      ;; redisplay-skip-fontification-on-input t
      )

;;;; Do not load outdated byte code files
(setq load-prefer-newer t)

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

;;;; Load custom file
;; Set and load custom file which contains persistent settings.
(with-eval-after-load 'no-littering
  (setq custom-file (no-littering-expand-var-file-name "custom.el"))
  (load custom-file))

(provide 'better-defaults-rcp)
;;; better-defaults-rcp.el ends here
