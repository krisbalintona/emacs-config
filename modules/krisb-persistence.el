;; -*- lexical-binding: t; -*-

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
;;;; Desktop
;; Save buffers across Emacs sessions
(use-package desktop
  :ensure nil
  :custom
  (desktop-load-locked-desktop 'check-pid)
  (desktop-save 'ask-if-new)
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
  (desktop-save-mode 1)

  (defun krisb-desktop--save-narrowing ()
    "Save narrowed information.
Taken from
https://www.reddit.com/r/emacs/comments/162cjki/comment/jxzrthx/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1."
    (setq desktop-save-buffer
          (lambda (_d) (if (buffer-narrowed-p)
                           (list 'narrowed (point-min) (point-max))))))
  (add-hook 'text-mode-hook #'krisb-desktop--save-narrowing)
  (add-hook 'prog-mode-hook #'krisb-desktop--save-narrowing)

  (defun krisb-desktop--restore-narrowing (_f n misc &rest _)
    "Restore narrowing of buffer.
Taken from
https://www.reddit.com/r/emacs/comments/162cjki/comment/jxzrthx/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1."
    (when (and misc (eq (car misc) 'narrowed))
      (apply #'narrow-to-region (cdr misc))
      (message "Narrowed %s" n)))
  (advice-add 'desktop-restore-file-buffer :after #'krisb-desktop--restore-narrowing))

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
