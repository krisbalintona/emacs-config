;;; convenient-functions-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are small groups of code, many of which are self-defined, that I find
;; useful. Most of these functions are taken from elsewhere (e.g. Doom)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Rename/move current file
(defun kb/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

      If FORCE-P, overwrite the destination file if it exists, without confirmation."
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

(kb/leader-keys
  "fR" '(kb/move-this-file :which-key "Rename current file")
  )

;;;; Yank current buffer's file-path
(defun kb/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(kb/leader-keys
  "fy" '(kb/yank-buffer-filename :which-key "Yank file-path")
  )

;;;; Kill
;;;;; Kill all buffers
(defun kb/kill-all-buffers ()
  "Kill all existing buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;;;; Kill this file
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

;;;;; Keybinds
;; Keybinds for the aforementioned functions
(general-define-key "C-x K" 'kill-this-buffer)
(kb/leader-keys
  "bK" '(kill-this-buffer :which-key "Kill current buffer") ; Sets keybinds for kill-this-buffer function
  "fD" '(kb/delete-this-file :which-key "Delete current file")
  "qQ" '(kb/kill-all-buffers :which-key "Kill all buffers")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'faces-rcp)
;;;; Idle quote
;; Display a random quote in the minibuffer after a certain amount of idle time.
;; It's useful to get inspiration when stuck writing
(defconst kb/quotes
  '("You can't see paradise, if you don't pedal.  - Chicken Run "
    "He who who says he can and he who says he can’t are both usually right ― Confucius"
    "Why waste time proving over and over how great you are when you could be getting better? - Dweck The Mindset"
    "You’re not a failure until you start to assign blame. - The legendary basketball coach John Wooden"
    "I could hear my heart beating. I could hear everyone's heart. I could hear the human noise we sat there making, not one of us moving, not even when the room went dark. - Raymond Carver"
    "A writer is a sum of their experiences. Go get some - Stuck in Love (2012)"
    "If there is any one secret of success, it lies in the ability to get the other person's point of view and see things from that person's angle as well as from your own. - Henry Ford"
    "People who can put themselves in the place of other people who can understand the workings of their minds, need never worry about what the future has in store for them. - Owen D. Young"
    "Good quotes they can be useful for creative writers as well."
    ))

(defun kb/show-random-quotes ()
  "Show random quotes to minibuffer."
  (interactive)
  (message "%s" (nth (random (length kb/quotes))
                     kb/quotes)
           ))

(run-with-idle-timer 300 t 'kb/show-random-quotes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'convenient-functions-rcp)
;;;; Unpackaged.el
;; These are a bunch of functions taken from
;; https://github.com/alphapapa/unpackaged.el. These are things which are useful
;; but don't warrant an entire package.

;;;;; Reload-package
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

;;;;; Font-compare
(defvar lorem-ipsum-text)

  ;;;###autoload
(defun unpackaged/font-compare (text fonts)
  "Compare TEXT displayed in FONTS.

  If TEXT is nil, use `lorem-ipsum' text. FONTS is a list of font
  family strings and/or font specs.

  Interactively, prompt for TEXT, using `lorem-ipsum' if left
  empty, and select FONTS with `x-select-font', pressing Cancel to
  stop selecting fonts."
  (interactive (list (pcase (read-string "Text: ")
                       ("" nil)
                       (else else))
                     ;; `x-select-font' calls quit() when Cancel is pressed, so we use
                     ;; `inhibit-quit', `with-local-quit', and `quit-flag' to avoid that.
                     (let ((inhibit-quit t))
                       (cl-loop for font = (with-local-quit
                                             (x-select-font))
                                while font
                                collect font into fonts
                                finally do (setf quit-flag nil)
                                finally return fonts))))
  (setq text (or text (s-word-wrap 80 (s-join " " (progn
                                                    (require 'lorem-ipsum)
                                                    (seq-random-elt lorem-ipsum-text))))))
  (with-current-buffer (get-buffer-create "*Font Compare*")
    (erase-buffer)
    (--each fonts
      (let ((family (cl-typecase it
                      (font (symbol-name (font-get it :family)))
                      (string it))))
        (insert family ": "
                (propertize text
                            'face (list :family family))
                "\n\n")))
    (pop-to-buffer (current-buffer))))

;;;;; Org-add-blank-lines
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

;; Call this function before every save in an org file. Don't do this for
;; org-agenda files - it makes it ugly
(require 'custom-directories-rcp)
(add-hook 'before-save-hook (lambda ()
                              (if (and
                                   (eq major-mode 'org-mode) ; Org-mode
                                   (not (string-equal default-directory (expand-file-name kb/agenda-dir))) ; Not agenda-dir
                                   (not (string-equal buffer-file-name (expand-file-name "seedbox.org" org-roam-directory)))) ; Not seedbox
                                  (let ((current-prefix-arg 4)) ; Emulate C-u
                                    (call-interactively 'unpackaged/org-add-blank-lines)))
                              ))
;;; convenient-functions-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'conventient-functions-rcp)
