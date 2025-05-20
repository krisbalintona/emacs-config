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

;;; Activities
(use-package activities
  :pin gnu-elpa-devel
  :hook (kill-emacs . activities-save-all)
  :bind (("C-c a d" . activities-define)
         ("C-c a n" . activities-new)
         ("C-c a a" . activities-resume)
         ("C-c a g" . activities-revert)
         ("C-c a r" . activities-rename)
         ("C-c a D" . activities-discard)
         ("C-c a b" . activities-switch-buffer)
         ("C-c a B" . activities-switch)
         ("C-c a s" . activities-suspend)
         ("C-c a k" . activities-kill)
         ("C-c a l" . activities-list))
  :custom
  (activities-kill-buffers t)
  (activities-bookmark-store nil)
  (activities-bookmark-warnings t)
  :config
  (activities-mode 1)
  (activities-tabs-mode 1))

;;; Savefold
(use-package savefold
  :vc (:url "https://github.com/jcfk/savefold.el.git")
  :custom
  (savefold-backends '(outline
                       org
                       ;; origami
                       ;; hideshow
                       ))
  (savefold-directory (no-littering-expand-var-file-name "savefold"))
  :config
  (savefold-mode 1)

  ;; Hash file names.  Instead of using the absolute path of a file, turn that
  ;; absolute path into a hash.  This resolves the issue of file paths being
  ;; longer than what the OS permits.  See also `krisb-auto-save-hash-file-name'
  ;; and `krisb-backup-file-name-hash'.
  (el-patch-defun savefold-utils--get-attr-table-fpath (fpath)
    "Return the fpath of the attribute table file for FPATH.

This naively replaces path slashes with ! (/a/b/c -> !a!b!c) leading to a chance
of collision."
    (el-patch-remove
      (let* ((fpath (expand-file-name fpath))
             (fpath (string-replace "/" "!" fpath))
             (fpath (string-replace ":" "!" fpath))) ; For windows
        (expand-file-name fpath savefold-directory)))
    (el-patch-add
      (expand-file-name (sha1 (expand-file-name fpath)) savefold-directory))))

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
