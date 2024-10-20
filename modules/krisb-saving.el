;;; Register
(use-package register
  :ensure nil
  :custom
  (register-preview-delay 0)
  (register-separator "  ")
  (register-use-preview nil)            ; Highlighting + navigation?
  (register-preview-display-buffer-alist
   '(display-buffer-at-bottom
     (window-height . fit-window-to-buffer)
     (preserve-size . (nil . t))
     (window-parameters . ((mode-line-format . none)
                           (no-other-window . t)))))
  :config
  (with-eval-after-load 'consult
    ;; Better than `consult-register'
    (setq register-preview-function #'consult-register-format)
    ;; Adds thin lines, sorting and hides the mode line of the register preview
    ;; window. Copied from https://github.com/minad/consult#use-package-example
    (advice-add #'register-preview :override #'consult-register-window)))

;;; Files
;; No-littering's `no-littering-theme-backups' sets the values for
;; `auto-save-file-name-transforms', `backup-directory-alist', and
;; `undo-tree-history-directory-alist'. Read its docstring for more information.

;;;; Backup
;; Backup files. "Emacs makes a backup for a file only the first time the file
;; is saved from the buffer that visits it."
(setopt make-backup-files t
        backup-by-copying nil       ; See (info "(emacs) Backup Copying")
        vc-make-backup-files t)     ; Still backup even if under version control

;; Numbering backups
(setopt version-control t
        kept-new-versions 6
        kept-old-versions 2
        delete-old-versions t)

;; Modified from Doom Emacs. Backup files have names that are hashed.
(defun krisb-backup-file-name-hash (file)
  "Hash the backup file name.
A few places use the backup file name so paths don't get too long.

Takes any FILE and return a hashed version."
  (let ((alist backup-directory-alist)
        backup-directory)
    (while alist
      (let ((elt (car alist)))
        (if (string-match (car elt) file)
            (setq backup-directory (cdr elt)
                  alist nil)
          (setq alist (cdr alist)))))
    (let ((file (make-backup-file-name--default-function file)))
      (if (or (null backup-directory)
              (not (file-name-absolute-p backup-directory)))
          file
        (expand-file-name (sha1 (file-name-nondirectory file))
                          (file-name-directory file))))))
(setopt make-backup-file-name-function #'krisb-backup-file-name-hash)


;;;; Auto-save
(setopt auto-save-default t ; Only a local minor mode exists; this variable influences the global value
        auto-save-timeout 3
        auto-save-interval 150
        auto-save-no-message t
        auto-save-include-big-deletions t)

(setopt delete-auto-save-files t
        kill-buffer-delete-auto-save-files nil)

;; Prevent auto-save from complaining about long file names by hashing them.
;; Copied from Doom Emacs.
(defun krisb-auto-save-hash-file-name (&rest args)
  "Turn `buffer-file-name' into a hash.
Then apply ARGS."
  (let ((buffer-file-name
         (if (or
              ;; Don't do anything for non-file-visiting buffers. Names
              ;; generated for those are short enough already.
              (null buffer-file-name)
              ;; If an alternate handler exists for this path, bow out. Most of
              ;; them end up calling `make-auto-save-file-name' again anyway, so
              ;; we still achieve this advice's ultimate goal.
              (find-file-name-handler buffer-file-name
                                      'make-auto-save-file-name))
             buffer-file-name
           (sha1 buffer-file-name))))
    (apply args)))
(advice-add 'make-auto-save-file-name :around #'krisb-auto-save-hash-file-name)

;;;; Autosave-visited
;; Save visited files after an idea time
(setopt auto-save-visited-interval 8
        auto-save-visited-predicate ; Value Inspired by `super-save'
        (lambda ()
          (or (< (save-restriction (widen) (count-lines (point-min) (point-max)))
                 5000)
              (derived-mode-p 'pdf-view-mode)))
        remote-file-name-inhibit-auto-save-visited nil)
(add-hook 'on-first-file-hook #'auto-save-visited-mode)

;;; Provide
(provide 'krisb-saving)
