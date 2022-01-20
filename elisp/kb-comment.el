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
(defun kb/comment-dwim-timestamp--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((last-used (car kb/comment-dwim--keyword-hist)))
    (completing-read
     (concat "Select keyword ["
             (propertize last-used 'face
                         (hl-todo--combine-face
                          (alist-get last-used hl-todo-keyword-faces nil nil #'equal)
                          ))
             "]: ")
     (if (featurep 'hl-todo)
         (cl-mapcan (pcase-lambda (`(,keyword . ,face))
                      (and (equal (regexp-quote keyword) keyword)
                           (list (propertize keyword 'face
                                             (hl-todo--combine-face face)))))
                    (cl-remove-if (lambda (row)
                                    (not (cl-member (car row) keywords
                                                    :test #'string-match)))
                                  hl-todo-keyword-faces)) ;
       keywords)
     nil nil nil 'kb/comment-dwim--keyword-hist last-used
     )))

(defun kb/comment-insert-timestamp ()
  "Insert a timestamp at point, preceded by a keyword, defined in
`kb/comment-keywords-writing' and `kb/comment-keywords-coding', depending on
major-mode."
  (let* ((date-style "%F"))
    (insert
     (format "%s %s: "
             (kb/comment-dwim-timestamp--keyword-prompt
              (cond ((derived-mode-p 'prog-mode) kb/comment-keywords-coding)
                    ((derived-mode-p 'org-mode) kb/comment-keywords-writing)
                    (t nil)))
             (format-time-string date-style))
     )))

(defun kb/comment-dwim-insert-comment ()
  "A helper function for `kb/comment-dwim'.

If in the middle of a line, then append comment. If on blank
line, then comment. End in `evil-insert-state'."
  (if comment-insert-comment-function
      (funcall comment-insert-comment-function)
    (progn
      (indent-according-to-mode)
      (insert (comment-padright comment-start (comment-add nil)))
      (save-excursion
        (unless (string= "" comment-end)
          (insert (comment-padleft comment-end (comment-add nil))))
        (indent-according-to-mode)
        )))
  ;; Finally, end in insert state
  (evil-insert-state)
  )

;;; Commands
(defun kb/comment-dwim (arg timestamp)
  "Call the comment command you want (Do What I Mean).

If in visual-mode, comment region. If with `C-u', then uncomment region.
If called without prefix argument, then append comment to the end of the line.
If called with `C-u', then comment in new line above.

If called with `C-u' `C-u', then comment in new line below.

Additionally, append a timestamp preceded by a chosen keyword if
TIMESTAMP is t."
  (interactive "*P")
  (comment-normalize-vars)
  (if (use-region-p)
      ;; If highlighting a region (visual-mode) then comment those lines
      (cond (t
             (comment-or-uncomment-region (region-beginning) (region-end) arg))) ; If with arg then uncomment
    ;; If in the middle of a line with no comment
    (if (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
        (cond (;; If with C-u
               (equal arg '(4)) ; Comment above
               (beginning-of-line)
               (insert "\n")
               (forward-line -1)
               (kb/comment-dwim-insert-comment))
              ;; If with C-u C-u
              ((equal arg '(16)) ; Comment below
               (end-of-line)
               (insert "\n")
               (kb/comment-dwim-insert-comment))
              ;; If with C-u C-u C-u
              ((equal arg '(64)) ; Remove any comments from line
               (comment-kill (and (integerp arg) arg)))
              ;; If without universal argument
              (t ; Comment at the end of the current line
               (comment-indent)
               (when (looking-at "\\s-*$")
                 (insert " ")
                 (evil-insert-state))))
      ;; When in an empty line
      (kb/comment-dwim-insert-comment))
    ;; When timestamp is t
    (if timestamp (kb/comment-insert-timestamp))
    ))

;;; Keybinds
(general-define-key
 "M-;" '((lambda (arg) (interactive "P") (kb/comment-dwim arg nil)) :which-key "Comment no timestamp")
 "M-:" '((lambda (arg) (interactive "P") (kb/comment-dwim arg t)) :which-key "Comment with timestamp")
 )

;;; kb-comment.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-comment)
