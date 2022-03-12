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

;;; Rename/move current file
(defun kb/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

      If FORCE-P, overwrite the destination file if it exists,
      without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    ;; (doom--update-files old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

(kb/file-keys
  "R" '(kb/move-this-file :wk "Rename current file")
  )

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
      ))
  )
(kb/toggle-keys
  "f" '(aj-toggle-fold :wk "aj-toggle-fold"))

;;; Indent whole buffer
(defun kb/format-buffer-indentation--base ()
  "Basic indentation fix using `indent-region'."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max)))
  )
(defun kb/format-buffer-indentation--fill-column ()
  "Basic indentation fix and wrap comments."
  (interactive)
  (kb/format-buffer-indentation--base)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward comment-start nil t)
      (call-interactively 'fill-paragraph)
      (forward-line 1)
      ))
  )
(defun kb/format-buffer-indentation ()
  "Properly indent the entire buffer."
  (interactive)
  (require 'apheleia)
  (cond
   ((not (buffer-file-name))
    (kb/format-buffer-indentation--base))
   ((eq major-mode 'latex-mode)    ; LaTeX
    (kb/format-buffer-indentation--base)
    (require 'latex-general-rcp)
    (kb/tabular-magic))
   ((eq major-mode 'org-mode)      ; Org-mode
    (let* ((modified-before (buffer-modified-p))
           (save-silently t))      ; Don't write to echo area when saving
      (kb/format-buffer-indentation--base)
      ;; Save buffer if modified and in `org-mode' because drawers are
      ;; annoying.
      (save-buffer)))
   ((or (eq major-mode 'emacs-lisp-mode) ; Emacs-lisp
        (eq major-mode 'lisp-interaction-mode))
    (kb/format-buffer-indentation--base))
   ((apheleia--get-formatters)     ; If available apheleia formatter
    (let* ((apheleia-mode t))      ; Save silently
      (apheleia--format-after-save)))
   ((derived-mode-p 'prog-mode)    ; Prog-mode
    (kb/format-buffer-indentation--fill-column))
   (t (kb/format-buffer-indentation--base))
   ))
(kb/general-keys
  "SPC" '(kb/format-buffer-indentation :wk "Format indentation"))

;;; Yank current buffer's file-path
(defun kb/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(kb/yank-kill-keys
  "f" '(kb/yank-buffer-filename :wk "Yank file-path")
  )

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

(kb/file-keys
  "D" '(kb/delete-this-file :wk "Delete current file")
  )

;;; Empty trash
(defun kb/empty-trash ()
  "Empty the trash directory."
  (interactive)
  (if delete-by-moving-to-trash
      (save-window-excursion (async-shell-command (concat "rm -rf " trash-directory))))
  )

;;; Unpackaged.el
;; These are a bunch of functions taken from
;; https://github.com/alphapapa/unpackaged.el. These are things which are useful
;; but don't warrant an entire package.

;;;; Reload-package
;; Simple function for reloading an entire package and all its feature. Useful
;; after upgrading
(defun unpackaged/reload-package (package &optional allp)
  "Reload PACKAGE's features.

  If ALLP is non-nil (interactively, with prefix), load all of its
  features; otherwise only load ones that were already loaded.

  This is useful to reload a package after upgrading it.  Since a
  package may provide multiple features, to reload it properly
  would require either restarting Emacs or manually unloading and
  reloading each loaded feature.  This automates that process.

  Note that this unloads all of the package's symbols before
  reloading.  Any data stored in those symbols will be lost, so if
  the package would normally save that data, e.g. when a mode is
  deactivated or when Emacs exits, the user should do so before
  using this command."
  (interactive
   (list (intern (completing-read "Package: "
                                  (mapcar #'car package-alist) nil t))
         current-prefix-arg))
  ;; This finds features in the currently installed version of PACKAGE, so if
  ;; it provided other features in an older version, those are not unloaded.
  (when (yes-or-no-p (format "Unload all of %s's symbols and reload its features? " package))
    (let* ((package-name (symbol-name package))
           (package-dir (file-name-directory
                         (locate-file package-name load-path (get-load-suffixes))))
           (package-files (directory-files package-dir 'full (rx ".el" eos)))
           (package-features
            (cl-loop for file in package-files
                     when (with-temp-buffer
                            (insert-file-contents file)
                            (when (re-search-forward (rx bol "(provide" (1+ space)) nil t)
                              (goto-char (match-beginning 0))
                              (cadadr (read (current-buffer)))))
                     collect it)))
      (unless allp
        (setf package-features (seq-intersection package-features features)))
      (dolist (feature package-features)
        (ignore-errors
          ;; Ignore error in case it's not loaded.
          (unload-feature feature 'force)))
      (dolist (feature package-features)
        (require feature))
      (message "Reloaded: %s" (mapconcat #'symbol-name package-features " ")))))

;;;; Org-add-blank-lines
;; Ensure that there are blank lines before and after org heading. Use with =universal-argument= to apply to whole buffer
(defun unpackaged/org-add-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.

  With PREFIX, operate on whole buffer. Ensures that blank lines
  exist after each heading's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
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
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))
(defun kb/org-add-blank-lines (&optional ARG)
  "Call `unpackaged/org-add-blank-lines' before saving in org files
which are not in `kb/agenda-dir'."
  (require 'org-capture)
  (when (and
         ;; NOTE 2022-02-03: This next line is a very important check. It fixes
         ;; a persistent and annoying bug when using `org-roam-capture' and
         ;; sometimes its variants.
         (not org-capture-mode)
         (buffer-file-name)
         (eq major-mode 'org-mode) ; Org-mode
         (not (string-equal default-directory (expand-file-name kb/agenda-dir))) ; Not agenda-dir
         )
    (let ((org-element-use-cache nil)) ; NOTE 2022-02-05: This is a shoddy fix for hanging when invoking in buffer with no headlines
      (funcall-interactively 'unpackaged/org-add-blank-lines '(4)) ; Emulate universal argument
      )))
(add-hook 'before-save-hook #'kb/org-add-blank-lines)

;;; convenient-functions-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'convenient-functions-rcp)
