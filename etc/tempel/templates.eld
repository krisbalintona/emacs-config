fundamental-mode

(t (completing-read "Keyword: " (split-string (key-description nil hl-todo-keyword-faces))) " ")
(d (format-time-string "%F"))
(td (p (if (equal major-mode 'emacs-lisp-mode) ";;" comment-start) comment-delimiter noinsert)
    (when (and comment-delimiter (not (nth 4 (syntax-ppss))))
      (concat (string-trim-right comment-delimiter)))
    (just-one-space) (include t) (include d) ":" (just-one-space))
