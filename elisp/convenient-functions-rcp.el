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
(require 'keybinds-frameworks-rcp)
(require 'custom-directories-rcp)

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

;;;; Aj-toggle-fold
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line. Taken from https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1)))
      ))
  )
(kb/leader-keys
  "of" '(aj-toggle-fold :which-key "aj-toggle-fold"))

;;;; Indent whole buffer
(defun kb/indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  )
(kb/leader-keys
  "TAB" '(kb/indent-whole-buffer :which-key "Indent whole buffer"))

;;;; Better comment-dwim
(with-eval-after-load 'smartparens
  (defun kb/comment-dwim (arg)
    "Call the comment command you want (Do What I Mean).
If the region is active and `transient-mark-mode' is on, call
`comment-region' (unless it only consists of comments, in which
case it calls `uncomment-region').
Else, if the current line is empty, call `comment-insert-comment-function'
if it is defined, otherwise insert a comment and indent it.
Else if a prefix ARG is specified, call `comment-kill'.
Else, call `comment-indent'.
You can configure `comment-style' to change the way regions are commented.

I've added a part at the end which puts me in insert mode with a
space between point and the comment character."
    (interactive "*P")
    (comment-normalize-vars)
    (if (use-region-p)
        (comment-or-uncomment-region (region-beginning) (region-end) arg)
      (if (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
          ;; FIXME If there's no comment to kill on this line and ARG is
          ;; specified, calling comment-kill is not very clever.
          (if arg (comment-kill (and (integerp arg) arg)) (comment-indent))
        ;; Inserting a comment on a blank line. comment-indent calls
        ;; c-i-c-f if needed in the non-blank case.
        (if comment-insert-comment-function
            (funcall comment-insert-comment-function)
          (let ((add (comment-add arg)))
            ;; Some modes insist on keeping column 0 comment in column 0
            ;; so we need to move away from it before inserting the comment.
            (indent-according-to-mode)
            (insert (comment-padright comment-start add))
            (save-excursion
              (unless (string= "" comment-end)
                (insert (comment-padleft comment-end add)))
              (indent-according-to-mode)))))
      ;; Add space and go into insert state when at the end of the line. The check
      ;; needed for when called within a comment, to be faithful to the
      ;; functionality of the original `comment-dwim', which makes it compatible
      ;; with other functions that call it.
      (when (sp--looking-at "[[:blank:]\n]")
        (insert " ")
        (evil-insert-state))
      ))
  (advice-add 'comment-dwim :override 'kb/comment-dwim)
  )

;;;; Prot-comment
;; helper code from prot-common.el, which is `require'd by
;; prot-comment.el
(defvar prot-common--line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `prot-common-line-regexp-p'.")

(defun prot-common-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`prot-common--line-regexp-alist'.  It matches a regular
expression.

With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (point-at-bol))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type prot-common--line-regexp-alist))))))

;; and what follows is from prot-comment.el
(defcustom prot-comment-comment-keywords-writing
  '("TODO" "COMMENT" "REVIEW" "FIXME")
  "List of strings with comment keywords."
  :type '(repeat string)
  :group 'prot-comment)

(defcustom prot-comment-comment-keywords-coding
  '("TODO" "NOTE" "REVIEW" "FIXME")
  "List of strings with comment keywords."
  :type '(repeat string)
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-concise "%F"
  "Specifier for date in `prot-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :type 'string
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-verbose "%F %T %z"
  "Like `prot-comment-timestamp-format-concise', but longer."
  :type 'string
  :group 'prot-comment)

(defvar prot-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun prot-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((def (car prot-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'prot-comment--keyword-hist def)))

;;;###autoload
(defun prot-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.

When called interactively, the list of possible keywords is that
of `prot-comment-comment-keywords', though it is possible to
input arbitrary text.

If point is at the beginning of the line or if line is empty (no
characters at all or just indentation), the comment is started
there in accordance with `comment-style'.  Any existing text
after the point will be pushed to a new line and will not be
turned into a comment.

If point is anywhere else on the line, the comment is indented
with `comment-indent'.

The comment is always formatted as 'DELIMITER KEYWORD DATE:',
with the date format being controlled by the variable
`prot-comment-timestamp-format-concise'.

With optional VERBOSE argument (such as a prefix argument
`\\[universal-argument]'), always insert newline above current one and write
comment there."
  (interactive
   (list
    (prot-comment--keyword-prompt
     (cond ((derived-mode-p 'prog-mode) prot-comment-comment-keywords-coding)
           ((derived-mode-p 'org-mode) prot-comment-comment-keywords-writing)
           (t nil)))
    current-prefix-arg))
  (let* ((date prot-comment-timestamp-format-concise)
         (string (format "%s %s: " keyword (format-time-string date)))
         (beg (point)))
    (cond ((equal current-prefix-arg '(4)) ; Universal argument case
           (progn (evil-insert-newline-above)
                  (indent-according-to-mode)
                  (insert
                   (concat comment-start
                           (make-string
                            (comment-add nil)
                            (string-to-char comment-start))
                           comment-padding
                           string
                           comment-end))
                  ))
          ((or (eq beg (point-at-bol)) ; Beginning of line or in indent whitespace case
               (prot-common-line-regexp-p 'empty))
           (let* ((maybe-newline (unless (prot-common-line-regexp-p 'empty 1) "\n")))
             ;; NOTE 2021-07-24: we use this `insert' instead of
             ;; `comment-region' because of a yet-to-be-determined bug that
             ;; traps `undo' to the two states between the insertion of the
             ;; string and its transformation into a comment.
             (insert
              (concat comment-start
                      ;; NOTE 2021-07-24: See function `comment-add' for
                      ;; why we need this.
                      (make-string
                       (comment-add nil)
                       (string-to-char comment-start))
                      comment-padding
                      string
                      comment-end))
             (indent-region beg (point))
             (when maybe-newline
               (save-excursion (insert maybe-newline)))))
          (t ; Within code (creates node at `comment-column')
           (comment-indent t)
           (insert (concat " " string))))
    ;; Immediately go into insert state afterward
    (evil-insert-state)
    ))

(kb/leader-keys
  "c" '(prot-comment-timestamp-keyword :which-key "Prot-comment")
  )

;;;; Insert date
(defun kb/insert-date (prefix)
  "Insert the current date. Accepts a PREFIX to change date format.
Mainly used for `ledger-mode'."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y/%m/%d")
                 ((equal prefix '(4)) "%d/%m/%Y"))) ; Other format
        (system-time-locale "de_DE"))
    (insert (format-time-string format)))
  (insert " ")
  (evil-insert-state)
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

;;;; Run command and return output as string without newlines
(defun kb/shell-command-to-string (command)
  "Execute shell command COMMAND and return its output as a string, removing any
newlines."
  (let* ((str (with-output-to-string
                (with-current-buffer
                    standard-output
                  (shell-command command t))))
         (len (length str)))
    (cond
     ((and (> len 0) (eql (aref str (- len 1)) ?\n))
      (substring str 0 (- len 1)))
     (t str))
    ))
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
(with-eval-after-load 'org-roam-general-rcp
  (add-hook 'before-save-hook (lambda ()
                                (if (and
                                     (eq major-mode 'org-mode) ; Org-mode
                                     (not (string-equal default-directory (expand-file-name kb/agenda-dir)))) ; Not agenda-dir
                                    (let ((current-prefix-arg 4)) ; Emulate C-u
                                      (call-interactively 'unpackaged/org-add-blank-lines)))
                                ))
  )

;;; convenient-functions-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'convenient-functions-rcp)
