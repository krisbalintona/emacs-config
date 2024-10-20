;;; convenient-functions-rcp.el --- Bespoke miscellaneous functions  -*- lexical-binding: t; -*-

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

;; These are small groups of code, many of which are self-defined, that I find
;; useful. Most of these functions are taken from elsewhere (e.g. Doom).

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'custom-directories-rcp)

;;;; Indent whole buffer
(defun kb/format-buffer-indentation--base (&optional beg end)
  "Basic indentation fix using `indent-region'.
By default, indents entire buffer. If BEG and END are specified,
act upon that region instead."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (untabify beg end)           ; Untabify before the following alters points
      ;; FIXME 2024-01-13: Because untabify could potentially increase the
      ;; number of tabs in the region, indent-region could potentially fail to
      ;; indent in the edge case that characters near the end of the region need
      ;; to be indented.
      (indent-region beg end nil)
      (delete-trailing-whitespace))))

(defun kb/format-buffer-indentation--fill-column (beg end)
  "Basic indentation fix and wrap comments."
  (kb/format-buffer-indentation--base (or beg (point-min)) (or end (point-max)))
  (unless (region-active-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward comment-start nil t)
        (call-interactively 'fill-paragraph)
        (forward-line 1)))))

(defun kb/format-buffer-indentation (beg end)
  "Properly indent the entire buffer."
  (interactive (list
                (if (region-active-p) (region-beginning) (point-min))
                (if (region-active-p) (region-end) (point-max))))
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (let ((start-time (current-time)))
    (message "Formatting buffer...")
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (kb/format-buffer-indentation--base beg end))
     ((eq major-mode 'inferior-emacs-lisp-mode)
      (kb/format-buffer-indentation--base
       (save-excursion (end-of-buffer) (comint-bol))
       (save-excursion (end-of-buffer) (comint-next-prompt 1))))
     ((eq major-mode 'conf-mode)
      (conf-align-assignments)
      (kb/format-buffer-indentation--base beg end))
     ((eq major-mode 'latex-mode)
      (kb/format-buffer-indentation--base beg end)
      (require 'latex-general-rcp)
      (kb/tabular-magic))
     ((eq major-mode 'org-mode)
      (kb/format-buffer-indentation--base beg end)
      (kb/org-add-blank-lines (unless (region-active-p) 'whole-buffer))
      (org-align-tags (not (region-active-p)))
      (when (buffer-file-name)
        (save-buffer)))
     ((eq major-mode 'racket-mode)
      (kb/format-buffer-indentation--base beg end)
      (when (buffer-file-name)
        (save-buffer)))
     ((and (require 'apheleia nil t)
           (apheleia--get-formatters))    ; If available apheleia formatter
      (let* ((apheleia-mode t))           ; Save silently
        (apheleia-format-after-save)))
     ((derived-mode-p 'message-mode)
      (delete-trailing-whitespace))
     ((derived-mode-p '(prog-mode conf-mode))
      (kb/format-buffer-indentation--fill-column beg end))
     (t
      (kb/format-buffer-indentation--base beg end)))
    (message (format-time-string "Formatting buffer... Done. Took %s.%3N seconds."
                                 (float-time (time-subtract (current-time) start-time))))))
(bind-key [remap indent-region] #'kb/format-buffer-indentation)

;;;; Yank current buffer's file-path
(defun kb/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (progn (kill-new filename)
             (message "Copied %s" filename))
    (error "Couldn't find filename in current buffer")))
(bind-key "w" #'kb/yank-buffer-filename 'krisb-file-keymap)

;;;; Delete this file
(defun kb/delete-this-file (&optional path force-p)
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
(bind-key "D" #'kb/delete-this-file 'krisb-file-keymap)

;;;; kb/org-add-blank-lines
;; Ensure that there are blank lines before and after org heading. Use with
;; =universal-argument= to apply to whole buffer
(defun unpackaged/org-add-blank-lines (&optional prefix)
  "Ensure blank lines between headings and their contents.

Also ensures that blank lines exist after each heading's drawers.

With PREFIX, operate on whole buffer, otherwise operate on the
current subtree."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us
                      ;; from seeing newlines before the current heading, so we
                      ;; do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p'
                         ;; would suffice, but for some reason it doesn't work
                         ;; correctly when operating on hidden text. This works,
                         ;; taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))
(defun kb/org-add-blank-lines (&optional whole-buffer)
  "Call `unpackaged/org-add-blank-lines'.

Called before saving in org files which are not in
`krisb-org-agenda-directory'."
  (when (and
         ;; NOTE 2022-02-03: This next line is a very important check. It fixes
         ;; a persistent and annoying bug when using `org-roam-capture' and
         ;; sometimes its variants.
         (not (bound-and-true-p org-capture-mode)) ; Not in org-capture buffer
         (buffer-file-name)                  ; In file
         (derived-mode-p 'org-mode)          ; Org-mode
         (not (file-in-directory-p (buffer-file-name) krisb-org-agenda-directory))) ; Not agenda-dir
    (save-excursion
      (org-with-wide-buffer
       (funcall-interactively 'unpackaged/org-add-blank-lines whole-buffer)))))

(provide 'convenient-functions-rcp)
;;; convenient-functions-rcp.el ends here
