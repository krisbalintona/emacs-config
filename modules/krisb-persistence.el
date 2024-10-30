;;; Savehist
;; Make history of certain things (e.g. minibuffer) persistent across sessions
(use-package savehist
  :ensure nil
  :custom
  (history-length 10000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 30)
  :config
  (dolist (var '(kill-ring
                 Info-history-list
                 last-kbd-macro
                 kmacro-ring
                 shell-command-history))
    (add-to-list 'savehist-additional-variables var))
  (savehist-mode 1))

;;; Saveplace
;; Save and restore the point's location in files
(use-package saveplace
  :ensure nil
  :hook (on-first-file . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t)
  (save-place-limit 3000))

;;; Desktop
;; Save buffers across Emacs sessions
(use-package desktop
  :ensure nil
  :custom
  (desktop-load-locked-desktop 'check-pid)
  (desktop-save 'ask-if-exists)
  (desktop-auto-save-timeout 3)
  (desktop-files-not-to-save
   (rx (or (regexp "\\(\\`/[^/:]*:\\|(ftp)\\'\\)")
           ;; Don't save files from other Emacs repos because sometimes they
           ;; have local variables that mess with desktop's loading of files
           (literal (expand-file-name "emacs-repos/" "~"))
           ;; Don't want to open my large org-agenda files which I'll open
           ;; eventually anyway
           (literal krisb-org-agenda-directory))))
  (desktop-globals-to-save '(desktop-missing-file-warning
                             tags-file-name
                             tags-table-list
                             search-ring
                             regexp-search-ring
                             ;; REVIEW 2024-10-13: The previews in
                             ;; `jump-to-register' cause errors when trying to
                             ;; visit a buffer or window which no longer exists.
                             ;; Removing it from the saved globals list is the
                             ;; workaround I choose for now.
                             ;; register-alist
                             file-name-history))
  (desktop-locals-to-save '(desktop-locals-to-save
                            truncate-lines
                            case-fold-search
                            case-replace
                            fill-column

                            overwrite-mode
                            change-log-default-name
                            line-number-mode

                            column-number-mode
                            size-indication-mode

                            buffer-file-coding-system
                            buffer-display-time

                            indent-tabs-mode
                            tab-width
                            indicate-buffer-boundaries

                            indicate-empty-lines
                            show-trailing-whitespace))

  (desktop-restore-eager 10)
  (desktop-restore-forces-onscreen nil)
  (desktop-restore-frames t)
  (desktop-restore-in-current-display nil)
  :config
  ;; (desktop-save-mode 1)
  )

;;; Easysession
(use-package easysession
  :diminish easysession-save-mode
  :custom
  (easysession-directory (no-littering-expand-var-file-name "easysession"))
  (easysession-save-interval (* 2 60))
  (easysession-mode-line-misc-info nil) ; I manually add to `global-mode-string' instead
  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 102)
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'easysession--current-session-name))

  ;; Add session infor to `global-mode-string'
  (add-to-list 'global-mode-string `easysession-mode-line-misc-info-format)

  ;; Kill old session buffers before loading a new session.  Taken from
  ;; https://github.com/jamescherti/easysession.el?tab=readme-ov-file#how-to-make-easysession-kill-all-buffers-before-loading-a-session
  (defun krisb-easysession-kill-old-session-buffers ()
    (save-some-buffers t)
    (mapc #'kill-buffer
          (cl-remove-if
           (lambda (buffer)
             (string= (buffer-name buffer) messages-buffer-name))
           (buffer-list)))
    (delete-other-windows))
  (add-hook 'easysession-before-load-hook #'krisb-easysession-kill-old-session-buffers)
  (add-hook 'easysession-new-session-hook #'krisb-easysession-kill-old-session-buffers))

;;; Bookmark
(use-package bookmark
  :ensure nil
  :hook (on-buffer-file . bookmark-maybe-load-default-file)
  :custom
  (bookmark-save-flag 1)                 ; Save bookmarks file every new entry
  (bookmark-watch-bookmark-file 'silent) ; Reload bookmarks file without query
  (bookmark-fringe-mark 'bookmark-mark)
  (bookmark-sort-flag 'last-modified)
  (bookmark-use-annotations nil)
  (bookmark-version-control 'nospecial))

;;; Provide
(provide 'krisb-persistence)
