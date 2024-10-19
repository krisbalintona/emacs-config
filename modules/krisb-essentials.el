;;; Keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Allow minibuffer commands in minibuffer
(setq enable-recursive-minibuffers t)

;;; Ignore case basically everywhere
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)
(setq-default case-fold-search t)

;;; `indent-for-tab-command' functionality.
(setopt tab-always-indent 'complete
        tab-first-completion 'word)

;;; Provide
(provide 'krisb-essentials)

;;; krisb-essentials.el --- Essential Emacs-wide settings  -*- lexical-binding: t; -*-

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

;; Setting general user options.

;;; Code:

;;; Customize
(setopt custom-file (no-littering-expand-etc-file-name "custom.el")
        custom-safe-themes t
        custom-theme-allow-multiple-selections t
        custom-unlispify-tag-names nil
        custom-buffer-style 'links
        custom-search-field nil)

;;; Scrolling
(setopt scroll-error-top-bottom nil
        scroll-preserve-screen-position t
        scroll-margin 0
        next-screen-context-lines 6
        scroll-minibuffer-conservatively t
        scroll-conservatively 1         ; Affects `scroll-step'
        scroll-up-aggressively nil      ; Center after point leaves window?
        scroll-down-aggressively nil)   ; Center after point leaves window?

;;;;; Font locking
(setopt jit-lock-defer-time 0.15
        ;; NOTE 2024-09-16: The below are used by Doom Emacs.
        ;; These are the three ways to increase scrolling performance.
        ;; See (info "(emacs) Scrolling") for details. Introduced in Emacs HEAD
        ;; (b2f8c9f), this inhibits fontification while receiving input, which should
        ;; help a little with scrolling performance.
        redisplay-skip-fontification-on-input t
        ;; More performant rapid scrolling over unfontified regions. May cause
        ;; brief spells of inaccurate syntax highlighting right after scrolling,
        ;; which should quickly self-correct.
        fast-but-imprecise-scrolling t)

;;; Miscellaneous
;;;; Enable all disabled commands
(setopt disabled-command-function nil)

;;;; Stretch cursor to the glyph width
(setopt x-stretch-cursor t)

;;;; Avoid collision of mouse with point
(mouse-avoidance-mode 'jump)

;;;; Recognize camel case as words
(global-subword-mode 1)

;;;; Middle-click pastes at point, not at mouse
(setopt mouse-yank-at-point t)

;;;; More leeway for Emacs subprocesses
;; Let Emacs subprocesses read more data per chunk
(setopt read-process-output-max (* 4 1024 1024)) ; 4mb
;; Recommend here
;; https://www.reddit.com/r/emacs/comments/17nl7cw/comment/k7u1ueu/?utm_source=share&utm_medium=web2x&context=3
(setopt process-adaptive-read-buffering nil)

;;;; Delete-selection-mode
;; When selecting text, if typing new text, replace the selected text with the
;; new text
(delete-selection-mode t)

;;;; Don't do anything with inactive mark
(setopt mark-even-if-inactive nil)

;;;; Strategy for uniquifying buffer names
(setopt uniquify-buffer-name-style 'post-forward)

;;;; Show context menu from right-click
(when (display-graphic-p)
  (context-menu-mode 1))

;;;; Don't show "obsolete" byte-compile warnings
(setopt byte-compile-warnings (remove 'obsolete byte-compile-warning-types))

;;;; Enable `view-mode' when calling `read-only-mode'
(setopt view-read-only t)


;;;; Behavior for `cycle-spacing-actions'
;; Read the docstring for an explanation (or try it out!)
(setopt cycle-spacing-actions '(just-one-space (delete-all-space -) restore))

;;;; Word wrapping
;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;;;; Repeatedly pop mark with C-u SPC
(setopt set-mark-command-repeat-pop t)

;;;; Default fill column
(setq-default fill-column 80)

;;;; Insert spaces instead of tab characters
(setq-default indent-tabs-mode nil)

;;;; Trash
(setq-default trash-directory (no-littering-expand-var-file-name "trash")
              delete-by-moving-to-trash t)

;;;; Don't create lock files
(setopt create-lockfiles nil)

;;;; Confirm to kill emacs
(setopt confirm-kill-emacs 'y-or-n-p)

;;;; Don’t warn when advising
(setopt ad-redefinition-action 'accept)

;;;; Double space delimits end of sentence?
(setq-default sentence-end-double-space nil)
(add-hook 'prog-mode-hook (lambda () (setq-local sentence-end-double-space t)))
(add-hook 'conf-mode-hook (lambda () (setq-local sentence-end-double-space t)))

(provide 'krisb-essentials)
;;; krisb-essentials.el ends here
