;;; Flymake
(use-package flymake
  :diminish
  :hook ((prog-mode org-mode) . flymake-mode)
  :custom
  (elisp-flymake-byte-compile-load-path (append '("./") load-path)) ; Recognize files Emacs knows about
  (flymake-wrap-around nil)
  (flymake-fringe-indicator-position nil)       ; Disable fringe indicators
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-suppress-zero-counters t)
  (flymake-mode-line-format
   '(flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format     ; Remove surrounding brackets
   '(:eval
     ;; NOTE 2024-02-12: Need to have first and last elements be strings!
     ;; Otherwise a counter might be hidden
     (let ((counters '(""
                       flymake-mode-line-error-counter
                       flymake-mode-line-warning-counter
                       flymake-mode-line-note-counter
                       "")))
       (if (mode-line-window-selected-p)
           counters
         (propertize (format-mode-line counters)
                     'face '(:inherit (bold mode-line-inactive))))))))

;;; Flymake-collection
(use-package flymake-collection
  :after flymake
  :custom
  (flymake-collection-hook-inherit-config t)
  (flymake-collection-hook-ignore-modes nil)
  :config
  ;; NOTE 2024-10-05: Set `flymake-collection-config' immediately when the
  ;; package loads, so the first invocation of `flymake-collection-hook-setup'
  ;; uses my configured value.
  ;; NOTE 2024-10-05: I configure vale to use proselint to my liking, so I
  ;; disable the proselint checker. One reason that motivates this decision is
  ;; vale's performance compared to proselint (see
  ;; https://github.com/errata-ai/vale?tab=readme-ov-file#benchmarks).
  (setf (alist-get 'org-mode flymake-collection-config)
        '((flymake-collection-vale
           :depth -20)
          (flymake-collection-proselint
           :depth -1
           :disabled t))
        (alist-get 'markdown-mode flymake-collection-config)
        '((flymake-collection-markdownlint
           :depth -50)
          (flymake-collection-vale
           :depth -20)
          (flymake-collection-proselint
           :disabled t
           :depth -1))
        (alist-get 'notmuch-message-mode flymake-collection-config)
        '((flymake-collection-vale
           :depth -20)
          (flymake-collection-proselint
           :depth -1
           :disabled t)))

  (flymake-collection-hook-setup))

;;; Provide
(provide 'krisb-flymake)