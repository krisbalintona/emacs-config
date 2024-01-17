;;; convenient-functions-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are small groups of code, many of which are self-defined, that I find
;; useful. Most of these functions are taken from elsewhere (e.g. Doom).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'custom-directories-rcp)

;;; Aj-toggle-fold
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line.
Taken from
https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1)))
      )))
(kb/toggle-keys "f" '(aj-toggle-fold :wk "aj-toggle-fold"))

;;; Indent whole buffer
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
      (kb/org-add-blank-lines (unless (region-active-p) :whole-buffer))
      (org-align-tags (not (region-active-p)))
      (kb/format-buffer-indentation--base beg end)
      (when (buffer-file-name)
        (save-buffer)))
     ((eq major-mode 'racket-mode)
      (kb/format-buffer-indentation--base beg end)
      (when (buffer-file-name)
        (save-buffer)))
     ((and (require 'apheleia-core nil t)
           (apheleia--get-formatters))    ; If available apheleia formatter
      (let* ((apheleia-mode t))           ; Save silently
        (apheleia-format-after-save)))
     ((derived-mode-p 'prog-mode)
      (kb/format-buffer-indentation--fill-column beg end))
     (t
      (kb/format-buffer-indentation--base beg end)))
    (message (format-time-string "Formatting buffer... Done. Took %3N milliseconds."
                                 (float-time (time-subtract start-time (current-time)))))))
(general-define-key [remap indent-region] 'kb/format-buffer-indentation)

;;; Yank current buffer's file-path
(defun kb/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (progn (kill-new filename)
             (message "Copied %s" filename))
    (error "Couldn't find filename in current buffer")))
(kb/file-keys "w" 'kb/yank-buffer-filename)

;;; Delete this file
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
(kb/file-keys "D" '(kb/delete-this-file :wk "Delete current file"))

;;; Empty trash
(defun kb/empty-trash ()
  "Empty the trash directory."
  (interactive)
  (if delete-by-moving-to-trash
      (save-window-excursion (async-shell-command (concat "rm -rf " trash-directory)))))

;;; Advice-unadvice
;; Thanks to
;; https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props)
                 (advice-remove sym advice)) sym))

;;; kb/org-add-blank-lines
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
`kb/agenda-dir'."
  (when (and
         ;; NOTE 2022-02-03: This next line is a very important check. It fixes
         ;; a persistent and annoying bug when using `org-roam-capture' and
         ;; sometimes its variants.
         (not (bound-and-true-p org-capture-mode)) ; Not in org-capture buffer
         (buffer-file-name)                  ; In file
         (derived-mode-p 'org-mode)          ; Org-mode
         (not (file-in-directory-p (buffer-file-name) kb/agenda-dir))) ; Not agenda-dir
    (save-excursion
      (org-with-wide-buffer
       (funcall-interactively 'unpackaged/org-add-blank-lines whole-buffer)))))

;;; kb/draft-subtree-to-file
;; Inspired by Palimpsest: https://github.com/danielsz/Palimpsest
(defvar kb/drafts-directory nil
  "Directory for drafts.")
(with-eval-after-load 'denote
  (setq kb/drafts-directory (expand-file-name "drafts/" org-directory)))

(defun kb/draft--draft-filename (filename &optional additional-suffix)
  "Provide FILENAME and return the trash file's name.

When ADDITIONAL-SUFFIX is provided, append it to the file base."
  (let ((draft-file-base (file-name-nondirectory (file-name-sans-extension filename)))
        (draft-extension (concat ".draft." (file-name-extension filename))))
    (if additional-suffix
        (concat draft-file-base additional-suffix draft-extension)
      (concat draft-file-base draft-extension))))

(defun kb/draft-send-region-to-trash (start end &optional filename)
  "Move text between START and END to associated trash FILENAME.

If FILENAME is not provided, the region's file name will be used
instead."
  (interactive "r")
  (if buffer-file-truename
      (let* ((draft-buffer (if filename
                               filename
                             (kb/draft--draft-filename (buffer-file-name))))
             (draft-file (expand-file-name draft-buffer kb/drafts-directory))
             (oldbuf (current-buffer)))
        (delay-mode-hooks
          (save-excursion
            (if (file-exists-p draft-file) (find-file draft-file))
            (set-buffer (get-buffer-create draft-buffer))
            (set-visited-file-name draft-file)
            (goto-char (point-min))
            (insert-buffer-substring oldbuf start end)
            (newline)
            (save-buffer)
            (write-file buffer-file-truename))
          (kill-region start end)
          (switch-to-buffer oldbuf)))
    (message "Please save buffer first")))

;; Much credit to https://hungyi.net/posts/org-mode-subtree-contents/
(defun kb/draft-subtree-to-file ()
  "Move current org subtree into a trash file.

The file name will be the file name of the current file with
\"--HEADING_TITLE\" appended to the file base. Its extension will
be \".draft.org\"."
  (interactive)
  (require 'org)
  (save-excursion
    (org-back-to-heading)
    (let ((beg-subtree (org-element-property :begin (org-element-at-point)))
          (end-subtree (org-element-property :end (org-element-at-point)))
          (draft-filename (kb/draft--draft-filename
                           (buffer-file-name)
                           (concat "--" (nth 4 (org-heading-components))))))
      (kb/draft-send-region-to-trash beg-subtree end-subtree draft-filename))))

;;; convenient-functions-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'convenient-functions-rcp)
