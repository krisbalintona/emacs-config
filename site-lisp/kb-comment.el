;;; kb-comment.el --- Summary
;;
;;; Commentary:
;;
;; This package defines the main command `kb/comment-dwim' (also see
;; `kb/comment-dwim-todo-and-timestamp'). The DWIM functionality of the command
;; relies (i) on how many universal arguments are used and (ii) whether the
;; region is active or not.
;;
;; This package is heavily inspired from the built-in `comment-dwim' and
;; Protesilaos Stavrou's `prot-comment-timestamp-keyword' infrastructure. The
;; idea of including a timestamp alongside keyword is inspired from him. My
;; implementation for handling keyword faces is inspired by the `hl-todo'.
;;
;; An alternative package is `comment-dwim-2'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'newcomment)

;;; Variables
(defgroup kb/comment nil
  "Customized `comment-dwim'."
  :group 'comment)

(defcustom kb/comment-keyword-alist
  '((org-mode . ("TODO" "COMMENT" "REVIEW" "FIXME"))
    (prog-mode . ("TODO" "NOTE" "REVIEW" "FIXME")))
  "An alist from major-mode to keyword strings.

The keys can also be parent modes (e.g. `text-mode')."
  :group 'kb/comment
  :type 'alist)

;; NOTE 2022-06-15: Some of these faces are taken from `hl-todo-keyword-faces'.
(defcustom kb/comment-keyword-faces
  '(("TODO" . "orange")
    ("FIXME" . (error bold))
    ("REVIEW" . "orchid")
    ("NOTE" . (success bold))
    ("COMMENT" . "cornflower blue"))
  "An alist of todo keyword faces.

Read `kb/comment--propertize-keyword' for a description of
possible values.

If you use `hl-todo', then `hl-todo-keyword-faces' can be set to
this variable in order to highlight those words in the buffer
with these faces."
  :group 'kb/comment
  :type 'alist)

(defcustom kb/comment--keyword-hist nil
  "Input history of selected comment keywords."
  :group 'kb/comment
  :type 'list)

(defcustom kb/comment-use-suggested-keybinds nil
  "Whether to use the suggested keybinds:

`[remap comment-dwim]' `kb/comment-dwim'
`C-M-;' `kb/comment-dwim-todo-and-timestamp'"
  :group 'kb/comment
  :type 'boolean)

;;; Helper functions
(defun kb/comment--propertize-keyword (keyword face)
  "Return KEYWORD propertized with FACE.

Returns nil if KEYWORD is nil.

KEYWORD should be a string. If FACE is a string, it will be
recognized as the foreground color. If it is a symbol, it should
be a face. Otherwise, the value should be a list of face
properties (e.g. `(font-lock-doc-face bold)' to use a bold
version of `font-lock-doc-face'). If FACE is none of these, the
value of FACE will be ignored."
  (let ((face-properties
         (cond ((stringp face)
                (list
                 ;; Inherit from `hl-todo' face if the mode is enabled. (Copies
                 ;; behavior of `hl-todo--combine-face'.) Otherwise, don't inherit
                 ;; nil.
                 :inherit (when (bound-and-true-p hl-todo-mode) 'hl-todo)
                 :foreground face))
               ((listp face)
                face))))
    (when keyword
      (propertize keyword 'face face-properties))))

(defun kb/comment--select-todo-completing-read ()
  "Use `completing-read' to select a todo keyword.

The keyword selection is based on `kb/comment-keyword-alist'."
  (let* ((last-used (car kb/comment--keyword-hist)) ; Command history
         (propertized-last-used
          (kb/comment--propertize-keyword last-used
                                          (cdr (assoc-string last-used kb/comment-keyword-faces))))
         (keywords-list
          (cl-loop for alist in kb/comment-keyword-alist
                   when (funcall 'derived-mode-p (car alist))
                   return (cdr alist))))
    (completing-read
     (concat "Select keyword"
             (when last-used
               (concat
                " (Default is " propertized-last-used ")"))
             ": ")
     (mapcar
      (lambda (keyword)
        (let ((keyword-face (cdr (assoc-string keyword kb/comment-keyword-faces))))
          (kb/comment--propertize-keyword keyword keyword-face)))
      keywords-list)
     nil nil nil 'kb/comment--keyword-hist last-used)))

(defun kb/comment-insert--insertion-base (&rest string-args)
  "Insert a comment at point with STRING-ARGS appended, space-separated.

For example, `(kb/comment-insert--insertion-base \"hi\"
\"there\")' in an elisp buffer will insert:

;; hi there

This function is intended to be used on empty lines (not for
end-of-line comments).

Leaves point after a the comment and optional STRING-ARGS are
 inserted, ensuring that there is a space between the comment
 delimiter and STRING-ARGS text."
  (comment-normalize-vars)              ; Check comment-related variables

  (indent-according-to-mode)
  ;; Insert comment delimiter and string-args
  (insert
   (comment-padright comment-start (comment-add nil))
   (mapconcat 'identity
              ;; Flatten list in order to bypass inner lists. This is necessary,
              ;; for instance, when passing additional-strings from
              ;; `kb/comment-dwim' as an argument here (which would result in a
              ;; list within a list.
              (flatten-list string-args)))

  (save-excursion              ; Insert enclosing comment delimiter if it exists
    (unless (string-empty-p comment-end)
      (insert (comment-padleft comment-end (comment-add nil))))
    (indent-according-to-mode)))

(defun kb/comment--region-kill-comments (&optional beg end save-to-kill-ring keep-empty-lines)
  "Delete all comments in region.

This includes full-line comments, for example,

;; this is a full-line comment

and end-of-line comments, for example,

\(message \"This is code!\") ; This is an end-of-line comment

If BEG and END are non-nil, then delete all comments from BEG
point to END point instead.

Additionally, if SAVE-TO-KILL-RING is non-nil, kill the comments
rather than delete them. Each line will be its own entry in the
`kill-ring'.

Finally, unless KEEP-EMPTY-LINES is non-nil, then also
delete the lines which have become empty as a result of removing
the comments."
  ;; TODO 2022-06-17: Have the comments saved to the `kill-ring' with each
  ;; comment "bunch" in a single entry. A group of comments is a "bunch" when
  ;; they (i) consist of a single comment block or (ii) when they are
  ;; end-of-line comments but indented as a paragraph as `fill-paragraph' would
  ;; arrange.
  (let* ((rbeg (or beg (region-beginning)))
         (rend (or end (region-end)))
         (traverse-lines (count-lines rbeg rend)))
    ;; Modified `comment-lines'. Considers keep-empty-lines functionality. We
    ;; do this to go line by line and delete the empty line only when a
    ;; comment has been removed (rather than removing all empty lines in
    ;; region entirely). Additionally, we want to delete region rather than
    ;; kill it
    (comment-normalize-vars)
    (save-excursion
      (goto-char rbeg)     ; Always start at beginning of region, then move down
      (dotimes (_i traverse-lines)
        ;; TODO 2022-06-17: When point is on a full-line comment, since it
        ;; will be deleted, the point is moved rather than restored to the
        ;; expected location. Fix this if possible.
        (save-excursion
          (beginning-of-line)
          (let ((cs (comment-search-forward (line-end-position) t)))
            (when cs
              (goto-char cs)
              (skip-syntax-backward " ")
              (setq cs (if (bolp)       ; To delete line when full-line comment
                           (1- (point))
                         (point)))
              (comment-forward)
              (if save-to-kill-ring
                  (kill-region cs (if (bolp) (1- (point)) (point)))
                (delete-region cs (if (bolp) (1- (point)) (point))))
              (indent-according-to-mode))))
        (forward-line 1)))))

;;; Commands
;;;###autoload
(defun kb/comment-dwim (prefix &rest additional-strings)
  "Based on PREFIX, insert either a normal comment or a comment with a timestamp.

This function is inspired by `comment-dwim'. The behavior is as
follows, with priority in this order (Note: when using this
command elsewhere, PREFIX should be a number, e.g. \"16\", not
`current-prefix-arg 'in raw-form):

If called with any number of postive universal aguments and
region is active, then kill all comments in the lines the region
is active in.

If region is active, follow the behavior of
`comment-or-uncomment-region'.

If called with `C-u', then comment in a new line above.

If called with `C-u' `C-u', then comment in a new line below.

If called with `C-u' `C-u' `C-u', then kill comment on current
line.

If called without a universal argument and in the middle of a
line, add comment to the end of current line.

If called without a universal argument and on an empty line,
insert a comment on the line.

If a comment already exists on this line, then move point to the
beginning of that comment's contents and indent that comment if
necessary (see `comment-indent').

Additionally, if ADDITIONAL-STRINGS is a string, then append that
string after the comment is inserted (with a space separating
ADDITIONAL-STRINGS and the comment delimiter).

After inserting a comment, if `evil-mode' is enabled in the
current buffer, end in `evil-insert-state'."
  (interactive "*p")
  (comment-normalize-vars)
  (let ((end-evil-insert (and (not buffer-read-only)
                              (bound-and-true-p evil-local-mode))))
    ;; When called from code, prefix will be nil rather than one when no prefix.
    ;; So manually set the prefix to 1 in that case
    (unless prefix (setq prefix 1))
    (cond
     ;; Any number of positive universal arguments and region active = Call
     ;; `kb/comment--region-kill-comments'
     ((and (< 1 prefix) (use-region-p))
      (setq end-evil-insert nil) ; In this case, don't force ending in insert-mode
      (kb/comment--region-kill-comments nil nil nil))
     ;; Region active = Comment those lines. However, uncomment if there are
     ;; only comments in the region or if called with universal argument. See
     ;; `comment-or-uncomment-region' for the behavior.
     ((use-region-p)
      (setq end-evil-insert nil) ; In this case, don't force ending in insert-mode
      (comment-or-uncomment-region (region-beginning) (region-end)))
     ;; C-u = Comment above
     ((equal prefix 4)
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (kb/comment-insert--insertion-base additional-strings))
     ;; C-u C-u = Comment below
     ((equal prefix 16)
      (end-of-line)
      (newline)
      (kb/comment-insert--insertion-base additional-strings))
     ;; C-u C-u C-u = Kill comment on line
     ((equal prefix 64)
      (comment-kill nil))
     ;; No universal argument and empty line with no comment = Insert comment,
     ;; or move point to comment if it already exists
     ((save-excursion (beginning-of-line) (looking-at "\\s-*$"))
      (kb/comment-insert--insertion-base additional-strings))
     ;; No universal argument and on non-empty line = Insert comment at
     ;; `comment-column', or move point to comment if it already exists on line.
     ((equal prefix 1)
      (comment-indent)
      (just-one-space) ; Ensure only one space is between comment delimiter and point.
      (insert (mapconcat 'identity additional-strings)))
     (t
      (error "Something has gone wrong in `kb/comment-dwim'! The prefix is %s" prefix)))

    ;; End in insert state if evil-mode is enabled and buffer isn't in
    ;; read-only mode
    (when (and (bound-and-true-p evil-mode)
               end-evil-insert)
      (require 'evil)
      (evil-insert-state))))

;;;###autoload
(defun kb/comment-dwim-todo-and-timestamp (prefix &optional todo timestamp time-format)
  "Insert a DWIM comment with a keyword and timestamp based on PREFIX.

For a description of the DWIM behavior, see `kb/comment-dwim'.

Additionally,if TODO is non-nil, then append a todo keyword based
on major-mode (see `kb/comment-keyword-alist').

And if TIMESTAMP is t, also append a timestamp. The format of the
timestamp follows `format-time-string'. Uses the format
TIME-FORMAT if provided, otherwise \"%F\" is used to format the
timestamp."
  (interactive (list (car current-prefix-arg) t t "%F"))
  (let ((keyword (when todo (kb/comment--select-todo-completing-read)))
        (time (when timestamp
                (format-time-string time-format))))
    (kb/comment-dwim prefix keyword " " time ": ")))

;; NOTE 2022-06-16: Taken heavily from `comment-line'
;;;###autoload
(defun kb/comment-line (prefix &optional save-to-kill-ring)
  "Comment line(s) based on PREFIX.

The behavior is as follows, with priority in this order:

If called with `C-u' `C-u' and the region is active, then call
`kb/comment--region-kill-comments' on region. When
SAVE-TO-KILL-RING is non-nil, also save the deleted text to the
`kill-ring' with each line as an individual entry (follows the
behavior of `kb/comment--region-kill-comments').

If called with `C-u', insert the region's text as commented lines
above the current line; if the region is not active, do this with
the current line instead. When SAVE-TO-KILL-RING is non-nil, also
save the region to the `kill-ring'.

If called without a universal argument, call
`comment-or-uncomment-region' on the region if active or current
line if not."
  (interactive "*p")
  (let* ((range (if (use-region-p)
                    (list (save-excursion
                            (goto-char (region-beginning))
                            (line-beginning-position))
                          (save-excursion
                            (goto-char (region-end))
                            (line-end-position)))
                  (list (line-beginning-position)
                        (line-end-position))))
         (beg (apply #'min range))
         (end (apply #'max range))
         (relevant-contents (buffer-substring beg end))) ; Line or region
    (cond
     ;; Any number of universal arguments and region is active = Call
     ;; `kb/comment--region-kill-comments'
     ((and (= prefix 16) (use-region-p))
      (kb/comment--region-kill-comments beg end save-to-kill-ring nil))
     ;; Active region and C-u = insert region above as commented text
     ((= prefix 4)
      (when save-to-kill-ring
        (kill-new relevant-contents))
      (save-excursion
        (goto-char beg)
        (open-line 1)
        (insert-for-yank relevant-contents)
        (comment-or-uncomment-region beg end)))
     ;; Active region = `comment-or-uncomment-region' on region; if no active
     ;; region = `comment-or-uncomment-region' on this line
     (t
      (comment-or-uncomment-region beg end)))))

;;; Keybinds
(when kb/comment-use-suggested-keybinds
  (define-key global-map [remap comment-dwim] 'kb/comment-dwim)
  (define-key global-map [remap comment-line] 'kb/comment-line)
  (define-key global-map (kbd "C-M-;") 'kb/comment-dwim-todo-and-timestamp))

;;; kb-comment.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-comment)
