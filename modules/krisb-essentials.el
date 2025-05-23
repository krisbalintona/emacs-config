;; -*- lexical-binding: t; -*-

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

;;; Set initial scratch buffer major mode and message
(setopt initial-scratch-message "Hello 👋"
        ;; Shave seconds off startup time by starting the scratch buffer in
        ;; `fundamental-mode'
        initial-major-mode 'fundamental-mode)

;;; Custom
(setopt custom-file (no-littering-expand-etc-file-name "custom.el")
        custom-safe-themes t
        custom-theme-allow-multiple-selections t
        custom-unlispify-tag-names nil
        custom-buffer-style 'links
        custom-search-field nil)

(when (and custom-file (file-exists-p custom-file))
  (load custom-file))

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
;; NOTE 2024-09-16: The below are used by Doom Emacs.
;; There are the three ways to increase scrolling performance.  See
;;
;; (info "(emacs) Scrolling")
;;
;; for details.
(setopt
 ;; Introduced in Emacs commit b2f8c9f, this inhibits fontification while
 ;; receiving input, which should help a little with scrolling performance.
 redisplay-skip-fontification-on-input t
 ;; More performant rapid scrolling over unfontified regions.  May cause brief
 ;; spells of inaccurate syntax highlighting right after scrolling, which should
 ;; quickly self-correct.
 fast-but-imprecise-scrolling t)

;;; Recognizing `M-SPC' under WSLg
;; 2024-10-29: There is currently an issue in WSLg that prevents Alt+Space from
;; being caught by X11.  A workaround is described in
;; https://github.com/microsoft/wslg/issues/1068#issuecomment-1817786154.  Use
;; PowerToys Keyboard Manager to rebind Alt+Space to Alt+F13 in then using
;; xmodmap to redirect Alt+F13 to M-SPC.  (Instead of creating a file, I do it
;; using a shell command below.)
(shell-command "xmodmap -e 'keycode 191 = space'")

;;; Commands
;;;; Restart or close Emacs
(defun krisb-restart-or-kill-emacs (&optional arg restart)
  "Kill Emacs.
If called with RESTART (`universal-argument’ interactively) restart
Emacs instead. Passes ARG to `save-buffers-kill-emacs'."
  (interactive "P")
  (save-buffers-kill-emacs arg (or restart (equal arg '(4)))))
(bind-key [remap save-buffers-kill-terminal] #'krisb-restart-or-kill-emacs)

;;;; Scrolling
(bind-keys
 ("C-M-S-s-p" . scroll-down-line)
 ("C-M-S-s-n" . scroll-up-line))

;;;; Joining lines
(defun krisb-open-line-above-goto ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current
mode. Credit to
https://emacsredux.com/blog/2013/06/15/open-line-above/"
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun krisb-open-line-below-goto ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
Credit to https://emacsredux.com/blog/2013/03/26/smarter-open-line/"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun krisb-join-line-above ()
  "Join the current line with the line above."
  (interactive)
  (save-excursion (delete-indentation))
  (when (string-match-p "\\`\\s-*$" (thing-at-point 'line))
    (funcall indent-line-function)))

(defun krisb-join-line-below ()
  "Join the current line with the line below."
  (interactive)
  (save-excursion (delete-indentation t))
  (when (bolp)
    (funcall indent-line-function)))

(bind-keys
 ("C-S-p" . krisb-open-line-above-goto)
 ("C-S-n" . krisb-open-line-below-goto)
 ("C-S-k" . krisb-join-line-above)
 ("C-S-j" . krisb-join-line-below))

;;;; Empty trash
(defun krisb-empty-trash ()
  "Empty the trash directory."
  (interactive)
  (let ((size (string-trim (shell-command-to-string (concat"du -sh " trash-directory " | cut -f1")))))
    (when (and delete-by-moving-to-trash
               (yes-or-no-p (format "Empty trash directory of %s size? " size)))
      (save-window-excursion (async-shell-command (concat "rm -rf " trash-directory))))))

;;;; Remove all advice from a function
;; Thanks to
;; https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
(defun krisb-advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props)
                 (advice-remove sym advice))
               sym))

;;;; Delete this file
(defun krisb-delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.
  If PATH is not specified, default to the current buffer's file.

  If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S? " short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          ;; ;; Ensures that windows displaying this buffer will be switched to
          ;; ;; real buffers (`doom-real-buffer-p')
          ;; (doom/kill-this-buffer-in-all-windows buf t)
          ;; (doom--update-files path)
          (kill-this-buffer)
          (message "Deleted %S" short-path))))))
(bind-key "D" #'krisb-delete-this-file 'krisb-file-keymap)

;;;; Yank current buffer's file-path
(defun krisb-yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let ((filename (or buffer-file-name list-buffers-directory)))
      (progn (kill-new filename)
             (message "Copied %s" filename))
    (error "Couldn't find filename in current buffer")))
(bind-key "w" #'krisb-yank-buffer-filename 'krisb-file-keymap)

;;;; Unfill paragraph
;; Protesilaos's `prot-simple-unfill-region-or-paragraph'
(defun krisb-unfill-region-or-paragraph (&optional beg end)
  "Unfill paragraph or, when active, the region.
Join all lines in region delimited by BEG and END, if active, while
respecting any empty lines (so multiple paragraphs are not joined, just
unfilled).  If no region is active, operate on the paragraph.  The idea
is to produce the opposite effect of both `fill-paragraph' and
`fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph))))
(bind-key "M-Q" #'krisb-unfill-region-or-paragraph)

;;; Minor modes
;;;; Recognize camel case as words
(global-subword-mode 1)

;;;; Repeat-mode
(repeat-mode 1)

;;;; Delete-selection-mode
;; When selecting text, if typing new text, replace the selected text with the
;; new text
(delete-selection-mode t)

;;;; Show context menu from right-click
(when (display-graphic-p)
  (context-menu-mode 1))

;;;; Avoid collision of mouse with point
(mouse-avoidance-mode 'jump)

;;;; Visual-line-mode in *Messages* buffer
(add-hook 'messages-buffer-mode-hook #'visual-line-mode)

;;;; Undo frame deletions
(undelete-frame-mode 1)

;;;; So-long-mode everywhere
(global-so-long-mode 1)

;;;; Show a default value only when default is applicable
(minibuffer-electric-default-mode 1)

;;;; Display-line-numbers
;; Show line numbers on the left fringe
(use-package display-line-numbers
  :ensure nil
  :bind ( :map krisb-toggle-keymap
          ("l" . display-line-numbers-mode))
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width-start t)) ; Keep width consistent in buffer

;;;; Krisb-reveal
(use-package krisb-reveal
  :ensure nil
  :config
  (krisb-reveal-global-mode 1))

;;; Miscellaneous
;;;; Enable all disabled commands
(setopt disabled-command-function nil)

;;;; Stretch cursor to the glyph width
(setopt x-stretch-cursor t)

;;;; Middle-click pastes at point, not at mouse
(setopt mouse-yank-at-point t)

;;;; More leeway for Emacs subprocesses
;; Let Emacs subprocesses read more data per chunk
(setopt read-process-output-max (* 4 1024 1024)) ; 4mb
;; Recommend here
;; https://www.reddit.com/r/emacs/comments/17nl7cw/comment/k7u1ueu/?utm_source=share&utm_medium=web2x&context=3
(setopt process-adaptive-read-buffering nil)

;;;; Don't do anything with inactive mark
(setopt mark-even-if-inactive nil)

;;;; Strategy for uniquifying buffer names
(setopt uniquify-buffer-name-style 'post-forward-angle-brackets)

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
(defun krisb-sentence-end-double-space-setup ()
  "Set up the value for `sentence-end-double-space'."
  (setq-local sentence-end-double-space
              (cond ((derived-mode-p '(prog-mode conf-mode log-edit-mode)) t)
                    ((derived-mode-p 'text-mode) nil))))

(dolist (mode '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode #'krisb-sentence-end-double-space-setup))

;;;; Keep the cursor out of the read-only portions of the minibuffer
(setopt minibuffer-prompt-properties
        '( read-only t
           cursor-intangible t
           face minibuffer-prompt))

;;;; Allow minibuffer commands in minibuffer
(setopt enable-recursive-minibuffers t)

;;;; Ignore case when convenient
(setopt read-buffer-completion-ignore-case t
        case-fold-search t)

;;;; `indent-for-tab-command' functionality.
(setopt tab-always-indent 'complete
        tab-first-completion 'word)

;;;; Duplicate-dwim binding
(bind-key "C-x ;" #'duplicate-dwim)
(setopt duplicate-line-final-position -1
        duplicate-region-final-position 1)

;;;; Rebind case commands
;; Remap these defaults; they are effectively the same while phasing out the
;; need the *-region binds
(bind-keys
 ([remap upcase-word] . upcase-dwim)
 ([remap downcase-word] . downcase-dwim)
 ([remap capitalize-word] . capitalize-dwim))

;;;; Echo unfinished keystrokes quicker
;; Echo keystrokes (of unfinished commands) much quicker
(setopt echo-keystrokes 0.5)

;;;; Quitting windows to match my intentions more
(setopt quit-restore-window-no-switch t)

;;;; Killing buffers smartly deletes windows too sometimes
(setopt kill-buffer-quit-windows t)

;;;; Don't display warning buffer at the bottom of frame
(setopt warning-display-at-bottom nil)

;;;; Don't highlight region if it isn't already active when calling `exchange-point-and-mark'
(setopt exchange-point-and-mark-highlight-region nil) ; New in Emacs 31.1

;;;; Don't visually shift text when using `rectangle-mark-mode'
(setopt rectangle-indicate-zero-width-rectangle nil) ; New in Emacs 31.1

;;;; Recenter upon `next-error'
(setopt next-error-recenter '(4))

;;;; Header line text scaling
(setq-default text-scale-remap-header-line t)

;;;; Truncate lines
;; If non-nil and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
;; So we set this to nil.
(setopt truncate-partial-width-windows nil)

;;;; Continuation line indicator character
;; See for an explanation of these concepts
;; https://www.reddit.com/r/emacs/comments/1fxr1ci/comment/lqpf2bz/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(set-display-table-slot standard-display-table 1 ?⏎)

;;;; Prefer UTF-8 file and selection encoding
(prefer-coding-system 'utf-8)
;; The clipboard on Windows is often a wider encoding (UTF-16), so leave Emacs
;; to its own devices there.  Otherwise, encode text into the clipboard into UTF-8
(unless (eq system-type 'windows-nt)
  (setopt selection-coding-system 'utf-8))

;;;; Prefer unicode charset
(set-charset-priority 'unicode)

;;; krisb-essentials.el ends here
(provide 'krisb-essentials)
