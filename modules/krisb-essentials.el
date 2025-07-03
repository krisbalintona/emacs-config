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

;;; Commands

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

;;; Minor modes
;;;; Recognize camel case as words
(global-subword-mode 1)

;;;; Krisb-reveal
(use-package krisb-reveal
  :ensure nil
  :config
  (krisb-reveal-global-mode 1))

;;; Miscellaneous

;;;; More leeway for Emacs subprocesses
;; Let Emacs subprocesses read more data per chunk
(setopt read-process-output-max (* 4 1024 1024)) ; 4mb
;; Recommend here
;; https://www.reddit.com/r/emacs/comments/17nl7cw/comment/k7u1ueu/?utm_source=share&utm_medium=web2x&context=3
(setopt process-adaptive-read-buffering nil)

;;;; Don't show "obsolete" byte-compile warnings
(setopt byte-compile-warnings (remove 'obsolete byte-compile-warning-types))

;;;; Enable `view-mode' when calling `read-only-mode'
(setopt view-read-only t)

;;;; Repeatedly pop mark with C-u SPC
(setopt set-mark-command-repeat-pop t)

;;;; Default fill column
(setq-default fill-column 80)

;;;; Confirm to kill emacs
(setopt confirm-kill-emacs 'y-or-n-p)

;;;; Ignore case when convenient
(setopt read-buffer-completion-ignore-case t
        case-fold-search t)

;;;; Echo unfinished keystrokes quicker
;; Echo keystrokes (of unfinished commands) much quicker
(setopt echo-keystrokes 0.5)

;;;; Don't display warning buffer at the bottom of frame
(setopt warning-display-at-bottom nil)

;;;; Don't highlight region if it isn't already active when calling `exchange-point-and-mark'
(setopt exchange-point-and-mark-highlight-region nil) ; New in Emacs 31.1

;;;; Don't visually shift text when using `rectangle-mark-mode'
(setopt rectangle-indicate-zero-width-rectangle nil) ; New in Emacs 31.1

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

;;; krisb-essentials.el ends here
(provide 'krisb-essentials)
