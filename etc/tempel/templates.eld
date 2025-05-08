fundamental-mode

(t (p (if (featurep 'hl-todo)
          (split-string (key-description nil hl-todo-keyword-faces))
        ("TODO" "FIXME" "NOTE"))
      possible-todo-keywords noinsert)
   (completing-read "Keyword: " possible-todo-keywords) (just-one-space))
(d (format-time-string "%F"))
(td (p (if (equal major-mode 'emacs-lisp-mode) ";;" comment-start) comment-delimiter noinsert)
    (when (and comment-delimiter (not (nth 4 (syntax-ppss))))
      (concat (string-trim-right comment-delimiter)))
    (just-one-space) (include t) (include d) ":" (just-one-space))
