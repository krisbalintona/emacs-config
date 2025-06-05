;; -*- lexical-binding: t; -*-

;;; Saveplace
;; Save and restore the point's location in files
(use-package saveplace
  :ensure nil
  :hook (on-first-file . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t)
  (save-place-limit 3000))

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

;;; Persistent desktops
;;;; Easysession
(use-package easysession
  :disabled t
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

;;;; Psession
(use-package psession
  :disabled t
  :custom
  (psession-auto-save-delay 60)
  :config
  (psession-mode 1)
  (psession-autosave-mode 1))

;;; Provide
(provide 'krisb-persistence)
