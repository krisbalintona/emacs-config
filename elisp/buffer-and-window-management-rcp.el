;;; buffer-and-window-management-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages for traversing and managing windows and buffers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Winner-mode
;; Reverting and traversing window configurations across time
(use-package winner
  :functions general-define-key
  :commands winner-mode
  :hook (after-init . winner-mode)
  :custom
  (winner-dont-bind-my-keys t) ; Don't bind keys because I bind them myself
  :config
  (general-define-key
   "C-<left>" 'winner-undo
   "C-<right>" 'winner-redo
   )
  )

;;;; Eyebrowse
;; Provide a simple way to have workspaces
(use-package eyebrowse
  :after (evil evil-collection)
  :hook (after-init . eyebrowse-mode)
  :commands eyebrowse-mode
  :custom
  (eyebrowse-default-workspace-slot 0) ; Start at 0
  (eyebrowse-keymap-prefix (kbd "C-c C-w"))
  (eyebrowse-mode-line-left-delimiter " ")
  (eyebrowse-mode-line-right-delimiter " ")
  (eyebrowse-mode-line-separator " ")
  (eyebrowse-tagged-slot-format "%t") ; Only show workspace name (tag) if avail
  (eyebrowse-wrap-around t) ; Cycle back to beginning when at the end and vice versa
  (eyebrowse-switch-back-and-forth t) ; Select current workspace to go to last used one
  :config
  (set-face-attribute 'eyebrowse-mode-line-active nil :weight 'semi-bold)

  (general-define-key
   :states '(visual normal motion)
   "gt" 'eyebrowse-next-window-config
   "ga" 'eyebrowse-prev-window-config
   "gz" 'eyebrowse-last-window-config
   )

  (general-define-key
   :keymaps 'eyebrowse-mode-map
   "C-c C-w r" 'eyebrowse-rename-window-config
   "C-c C-w c" 'eyebrowse-close-window-config
   )

  (general-define-key
   "M-1" 'eyebrowse-switch-to-window-config-1
   "M-2" 'eyebrowse-switch-to-window-config-2
   "M-3" 'eyebrowse-switch-to-window-config-3
   "M-4" 'eyebrowse-switch-to-window-config-4
   "M-5" 'eyebrowse-switch-to-window-config-5
   "M-6" 'eyebrowse-switch-to-window-config-6
   "M-7" 'eyebrowse-switch-to-window-config-7
   "M-8" 'eyebrowse-switch-to-window-config-8
   "M-9" 'eyebrowse-switch-to-window-config-9
   "M-0" 'eyebrowse-switch-to-window-config-0
   )
  )

;;;; Shackle
;; Control the behavior of popup and side windows
(use-package shackle
  :commands shackle-mode
  :hook (after-init . shackle-mode)
  :custom
  (shackle-rules '((flycheck-verify-mode :inhibit-window-quit t :same t)
                   (helpful-mode :inhibit-window-quit t :same t)
                   ;; (help-mode :inhibit-window-quit t :same t) ; Messes with org-roam-doctor buffer
                   (process-menu-mode :inhibit-window-quit t :same t)
                   ("magit:" :regexp t :align t :same t)
                   ("\\*org-roam\\*" :regexp t :align right :same nil :size 0.2)
                   ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)
                   ("*Flycheck errors*" :select t :align below :size 0.33)
                   ))
  (shackle-select-reused-windows t)
  )

;;;; Bufler
;; Manage buffers better
(use-package bufler
  :disabled t ; Don't really use
  :after (evil evil-collection)
  :hook (after-init . bufler-mode)
  :config
  (pretty-hydra-define hydra:bufler
    (:hint t :foreign-keys run :quit-key "?" :exit t)
    ("Bufler"
     (("r" #'bufler "Refresh")
      ("m" #'bufler-mode "Toggle bufler-mode")
      ("q" #'quit-window "Quit"))
     "Buffer"
     (("SPC" #'bufler-list-buffer-peek "Peek at")
      ("RET" #'bufler-list-buffer-switch "Switch to buffer")
      ("K" #'bufler-list-buffer-kill "Kill buffer or group of buffers")
      ("s" #'bufler-list-buffer-save "Save buffer or group of buffers")
      ("N" #'bufler-list-buffer-name-workspace "Add buffers to named workspace (prefix removes)"))
     "Workspace"
     (("f" #'bufler-list-group-frame "Current frame focus")
      ("F" #'bufler-list-group-make-frame "Make frame and set focus"))))

  (general-define-key
   :keymaps 'bufler-list-mode
   :states 'normal
   "SPC" 'bufler-list-buffer-peek
   "m" 'bufler-mode
   "r" 'bufler
   "s" 'bufler-list-buffer-save
   "K" 'bufler-list-buffer-kill
   "f" 'bufler-list-group-frame
   "F" 'bufler-list-group-make-frame
   "q" 'quit-window
   )

  (general-define-key
   :keymaps 'bufler-list-mode
   :states 'motion
   "?" 'hydra:bufler/body
   "RET" 'bufler-list-buffer-switch
   "N" 'bufler-list-buffer-name-workspace
   )

  (kb/leader-keys
    "bb" '(bufler-switch-buffer :which-key "Bufler switch")
    "bs" '(bufler-workspace-frame-set :which-key "Set bufler workspace for this frame")
    "bl" '(bufler-list :which-key "Bufler buffer list")
    )
  )

;;;; Desktop
;; Save and restore open buffers and frames
(use-package desktop
  :disabled t ; More inconvenient that useful right now
  :hook ((window-setup . desktop-save-mode)
         (window-setup . session-restore))
  :custom
  (desktop-save t) ; Always save when quitting Emacs or changing desktop
  (desktop-path `(,user-emacs-directory))
  (desktop-dirname user-emacs-directory)
  (desktop-base-file-name "emacs-save-desktop")
  (desktop-restore-eager nil) ; Lazily restore buffers to reduce startup time
  (desktop-restore-eager 13) ; Lazily restore buffers to reduce startup time
  (desktop-lazy-verbose nil) ; Don't be verbose when lazy loading buffers
  (desktop-lazy-idle-delay 10) ; Wait 10 seconds until lazy loading buffers
  (desktop-auto-save-timeout 20) ; Idle time until save
  (desktop-restore-frames t) ; Restore frames too
  (desktop-load-locked-desktop t) ; Always load so that it is compatible with daemon
  ;; (desktop-restore-reuses-frames 'keep) ; Keep current frames when restoring session
  (history-length 1000) ; This is what Doom uses
  :config
  (setq desktop-buffers-not-to-save ;; Don't save these buffers
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$"))
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

  ;; Remove desktop after it's been read
  (add-hook 'desktop-after-read-hook
            '(lambda ()
               ;; desktop-remove clears desktop-dirname
               (setq desktop-dirname-tmp desktop-dirname)
               (desktop-remove)
               (setq desktop-dirname desktop-dirname-tmp)
               (org-mode-restart)
               ))

  (defun saved-session ()
    (file-exists-p (concat desktop-dirname desktop-base-file-name)))

  ;; Use session-restore to restore the desktop manually
  (defun session-restore ()
    "Restore a saved emacs session."
    (interactive)
    (if (saved-session)
        (desktop-read)
      (message "No desktop found.")))

  ;; Use session-save to save the desktop manually
  (defun kb/prompt-session-save ()
    "Save an emacs session."
    (interactive)
    (if (saved-session)
        (if (y-or-n-p "Overwrite existing desktop? ")
            (desktop-save-in-desktop-dir)
          (message "Session not saved."))
      (desktop-save-in-desktop-dir)
      ))

  (defun kb/auto-session-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save-in-desktop-dir)))
  ;; (add-hook 'after-save-hook 'kb/auto-session-save) ; Save constantly
  (run-with-timer 0 480 'kb/auto-session-save) ; Save every 10 minutes

  ;; Restore when I want it to
  ;; (add-hook 'after-init-hook
  ;;           '(lambda ()
  ;;              (if (saved-session)
  ;;                  ;; (if (y-or-n-p "Restore desktop? ")    )
  ;;                  (session-restore)
  ;;                (progn (session-restore)
  ;;                       (if (daemonp) (org-mode-restart))) ; Restart org-mode to properly set faces for org files after loading with daemon
  ;;                )))


  ;; Prevent desktop file from being locked on system and
  ;; Emacs crashes
  (defun emacs-process-p (pid)
    "If pid is the process ID of an emacs process, return t, else nil.
              Also returns nil if pid is nil."
    (when pid
      (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
        (when (file-exists-p cmdline-file)
          (with-temp-buffer
            (insert-file-contents-literally cmdline-file)
            (goto-char (point-min))
            (search-forward "emacs" nil t)
            pid)))))

  (defadvice desktop-owner (after pry-from-cold-dead-hands activate)
    "Don't allow dead emacsen to own the desktop file."
    (when (not (emacs-process-p ad-return-value))
      (setq ad-return-value nil)))
  (defun kb/desktop-owner-advice (original &rest args)
    (let ((owner (apply original args)))
      (if (and owner (/= owner (emacs-pid)))
          (and (car (member owner (list-system-processes)))
               (let (cmd (attrlist (process-attributes owner)))
                 (if (not attrlist) owner
                   (dolist (attr attrlist)
                     (and (string= "comm" (car attr))
                          (setq cmd (car attr))))
                   (and cmd (string-match-p "[Ee]macs" cmd) owner))))
        owner)))

  ;; Ensure that dead system processes don't own it.
  (advice-add #'desktop-owner :around #'kb/desktop-owner-advice)
  )

;;;; Burly
(use-package burly
  :custom
  (bookmark-save-flag 1) ; Save bookmarks file every time there is a changed or added bookmark
  :config
  (kb/leader-keys
    "Bw" '(burly-bookmark-windows :which-key "Burly windows")
    "Bm" '(burly-open-bookmark :which-key "Open burly bookmark")
    "BM" '(burly-open-last-bookmark :which-key "Open last bookmark")
    )
  )

;;; buffer-and-window-management-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffer-and-window-management-rcp)
