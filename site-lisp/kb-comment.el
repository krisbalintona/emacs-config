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
;;; Variables
;;;###autoload
(defgroup kb/comment nil
  "Customized `comment-dwim'.")

;;;###autoload
(defcustom kb/comment-keyword-alist
  '((org-mode . ("TODO" "COMMENT" "REVIEW" "FIXME"))
    (prog-mode . ("TODO" "NOTE" "REVIEW" "FIXME")))
  "An alist from major-mode to keyword strings.

Can also be parent modes (e.g. `text-mode').")

;; NOTE 2022-06-15: Some of these faces are taken from `hl-todo-keyword-faces'.
;;;###autoload
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
with these faces.")

;;;###autoload
(defcustom kb/comment--keyword-hist nil
  "Input history of selected comment keywords."
  :group 'kb/comment)

;;;###autoload
(defcustom kb/comment-use-suggested-keybinds nil
  "Whether to use the suggested keybinds:

`[remap comment-dwim]' `kb/comment-dwim'
`C-M-;' `kb/comment-dwim-todo-and-timestamp'"
  :group 'kb/comment)

(defvar kb/comment-use-suggested-keybinds nil
  "Whether to use the suggested keybinds:

`[remap comment-dwim]' `kb/comment-dwim'
`C-M-;' `kb/comment-dwim-todo-and-timestamp'")

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
          (cl-loop for (key . value) in kb/comment-keyword-alist
                   thereis (derived-mode-p 'text-mode)
                   finally return value)))
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
    (unless (string= "" comment-end)
      (insert (comment-padleft comment-end (comment-add nil))))
    (indent-according-to-mode)))

;;; Commands
;;;###autoload
(defun kb/comment-dwim (prefix &rest additional-strings)
  "Based on PREFIX, insert either a normal comment or a comment with a timestamp.

This function is inspired by `comment-dwim'. The behavior is as
follows, with priority in this order (Note: when using this
command elsewhere, PREFIX should be a number, e.g. \"16\", not
`current-prefix-arg 'in raw-form):

If in visual-mode, follow the behavior of
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
beginning of that comment's contents.

Additionally, if ADDITIONAL-STRINGS is a string, then append that
string after the comment is inserted (with a space separating
ADDITIONAL-STRINGS and the comment delimiter).

After inserting a comment, if `evil-mode' is enabled in the
current buffer, end in `evil-insert-state'."
  (interactive "*p")
  (let ((end-evil-insert (and (not buffer-read-only)
                              (bound-and-true-p evil-local-mode))))
    (cond
     ;; If marking a region, comment those lines. However, uncomment if there
     ;; are only comments in the region or if called with universal argument.
     ;; See `comment-or-uncomment-region' for the behavior.
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
    (when end-evil-insert
      (require 'evil)
      (evil-insert-state))))

;;;###autoload
(defun kb/comment-dwim-todo-and-timestamp (prefix &optional todo timestamp time-format)
  "Insert a DWIM comment with a keyword and timestamp based on PREFIX.

For a description of the DWIM behavior, see `kb/comment-dwim'.

Additionally,if TODO is non-nil, then append a todo keyword based
on major-mode (see `kb/comment-keywords-coding' and
`kb/comment-keywords-writing').

And if TIMESTAMP is t, also append a timestamp. The format of the
timestamp follows `format-time-string'. Uses the format
TIME-FORMAT if provided, otherwise \"%F\" is used to format the
timestamp."
  (interactive (list (car current-prefix-arg) t t "%F"))
  (let ((keyword (when todo (kb/comment--select-todo-completing-read)))
        (time (when timestamp
                (format-time-string time-format))))
    (kb/comment-dwim prefix keyword " " time ": ")))

;;; Keybinds
(when kb/comment-use-suggested-keybinds
  (define-key global-map [remap comment-dwim] 'kb/comment-dwim)
  (define-key global-map (kbd "C-M-;") 'kb/comment-dwim-todo-and-timestamp))

;;; kb-comment.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-comment)
