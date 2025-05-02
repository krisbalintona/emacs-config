fundamental-mode

(t (completing-read "Keyword: " (split-string (key-description nil hl-todo-keyword-faces))) " ")
(d (format-time-string "%F"))
(td (p (if (equal major-mode 'emacs-lisp-mode) ";;" comment-start) comment-delimiter noinsert)
    (when comment-delimiter (concat (string-trim-right comment-delimiter) " "))
    (include t) (include d) ": ")
