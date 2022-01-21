;;; kb-comment.el --- Summary
;;
;;; Commentary:
;;
;; Code related to how I make commenting easier for myself. Heavily taken from
;; the built-in `comment-dwim' and Prot's `prot-comment-timestamp-keyword'
;; infrastructure. The idea of including a timestamp alongside keyword is
;; inspired from him.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;; (require 'smartparens)
;; (require 'hl-todo)
(require 'keybinds-general-rcp)

;;; Variables
;;;; Comment lists
(defvar kb/comment-keywords-writing
  '("TODO" "COMMENT" "REVIEW" "FIXME")
  "List of strings with comment keywords.

Make sure these words have a matching face listed in `hl-todo-keyword-faces',
otherwise those words will not appear in any calls to `kb/comment-dwim'.")

(defvar kb/comment-keywords-coding
  '("TODO" "NOTE" "REVIEW" "FIXME")
  "List of strings with comment keywords.

Make sure these words have a matching face listed in `hl-todo-keyword-faces',
otherwise those words will not appear in any calls to `kb/comment-dwim'.")

;;;; Other
(defvar kb/comment-dwim--keyword-hist '()
  "Input history of selected comment keywords.")

;;; Helper functions
;;;; Base comment insertion
(defun kb/comment-insert--insertion-base (&rest args)
  "A helper function for `kb/comment-dwim'.

If in the middle of a line, then append comment. If on blank line, then comment.
End in `evil-insert-state'."
  (comment-normalize-vars)    ; Check comment-related variables first
  (indent-according-to-mode)  ; Ensure you begin at the proper indentation level
  (insert
   (comment-padright comment-start (comment-add nil)) ; Insert comment delimiter
   (mapconcat 'identity                 ; Any additional appendages
              (cl-remove-if-not (lambda (elt) (stringp elt)) args)) ; Remove non-strings from args
   )
  (save-excursion ; Ensuring enclosing comment delimiter is inserted if it exists
    (unless (string= "" comment-end)
      (insert (comment-padleft comment-end (comment-add nil))))
    (indent-according-to-mode))
  ;; Finally, end in the insert state, but only if evil mode is active
  (when (bound-and-true-p evil-mode)
    (evil-insert-state))
  )

;;;; Timestamp insertion
(defun kb/comment-insert--insertion-timestamp ()
  "Insert a timestamp at point, preceded by a keyword, defined in
`kb/comment-keywords-writing' and `kb/comment-keywords-coding',
depending on major-mode."
  (let* ((last-used (car kb/comment-dwim--keyword-hist))
         (keywords-list (cond ((derived-mode-p 'prog-mode) kb/comment-keywords-coding)
                              ((derived-mode-p 'org-mode) kb/comment-keywords-writing)
                              (t nil)))
         (keyword (completing-read                  ; Query for keyword
                   (concat "Select keyword ["
                           (propertize last-used 'face
                                       (hl-todo--combine-face
                                        (alist-get last-used hl-todo-keyword-faces nil nil #'equal)))
                           "]: ")
                   (cl-mapcan (pcase-lambda (`(,word . ,face))
                                (and (equal (regexp-quote word) word)
                                     (list (propertize word 'face
                                                       (hl-todo--combine-face face)))))
                              (cl-remove-if (lambda (row)
                                              (not (cl-member (car row) keywords-list
                                                              :test #'string-match)))
                                            hl-todo-keyword-faces))
                   nil nil nil 'kb/comment-dwim--keyword-hist last-used))
         )
    (kb/comment-insert--insertion-base (format "%s %s: " keyword (format-time-string "%F")))
    ))

;;;; Versatile comment insertion
(defun kb/comment-insert--insertion-versatile (prefix timestamp)
  "Call the comment command you want (Do What I Mean).

If in visual-mode, comment region. If with `C-u', then uncomment region.
If called without prefix argument, then append comment to the end of the line.
If called with `C-u', then comment in new line above.

If called with `C-u' `C-u', then comment in new line below.

Additionally, append a timestamp preceded by a chosen keyword if
TIMESTAMP is t."
  (let ((comment-func (if timestamp
                          'kb/comment-insert--insertion-timestamp
                        'kb/comment-insert--insertion-base))
        )
<<<<<<< variant A
    ;; Choose which case I'm in
    (cond
>>>>>>> variant B
    (cond ; Choose which case I'm in
======= end
     ;; First, check if highlighting a region (visual-mode). If so, comment
     ;; those lines. However, uncomment if also called with universal argument.
     ((use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end) arg))
     ;; Next, check case when on empty line with no comment
     ((save-excursion (beginning-of-line) (looking-at "\\s-*$"))
      (funcall comment-func))
     ;; Then go onto non-empty line cases. Reliant on (interactive "*p")
     ;; C-u = Comment above
     ((= prefix 4)
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (funcall comment-func))
     ;; C-u C-u = Comment below
     ((= prefix 16)
      (end-of-line)
      (newline)
      (funcall comment-func))
     ;; C-u C-u C-u = Remove any comments from line
     ((= prefix 64)
      (comment-kill (and (stringp prefix) prefix)))
     ;; If without universal argument. Default by commenting at the end of the
     ;; current line
     (t
      (comment-indent) ; Insert comment, or move point to comment if it already exists on line
      (when (looking-at "\\s-*$") ; If comment doesn't already exists on line, then go into insert mode
        (insert " ")
        (when (bound-and-true-p evil-mode) ; Finally, end in the insert state, but only if evil mode is active
          (evil-insert-state))))
     )))

;;; Commands
(defun kb/comment-dwim-simple (prefix)
  " TODO "
  (interactive "*p")            ; Show prefix arg as number rather than raw form
  (kb/comment-insert--insertion-versatile prefix nil)
  )

(defun kb/comment-dwim-timestamp (prefix)
  " TODO "
  (interactive "*p")            ; Show prefix arg as number rather than raw form
  (kb/comment-insert--insertion-versatile prefix t)
  )

;;; Keybinds
(general-define-key
 "M-;" '(kb/comment-dwim-simple :which-key "Comment-dwim-simple")
 "M-:" '(kb/comment-dwim-timestamp :which-key "Comment-dwim-timestamp")
 )

;;; kb-comment.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-comment)
