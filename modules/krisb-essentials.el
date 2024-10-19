;;; Keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Allow minibuffer commands in minibuffer
(setq enable-recursive-minibuffers t)

;;; Ignore case basically everywhere
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)
(setq-default case-fold-search t)

;;; `indent-for-tab-command' functionality.
(setopt tab-always-indent 'complete
        tab-first-completion 'word)

;;; Provide
(provide 'krisb-essentials)
