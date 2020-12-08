;;; convenient-functions-rcp.el --- Summary
;;
;; These are small groups of code, many of which are self-defined, that I find
;; useful
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Sudo
;;;;; Sudo find-file
;; Find a file with sudo
(defun doom--sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(defun kb/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (doom--sudo-file-path file)))

;;;;; Sudo the current file
;; Get sudo privileges for the current file
(defun kb/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-file
   (doom--sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

;;;;; Keybinds
(kb/leader-keys
  "fu" '(kb/sudo-find-file :which-key "Sudo find file")
  "fU" '(kb/sudo-this-file :which-key "Sudo save this file")
  )

;;;; Rename/move current file
(defun kb/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

      If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    ;; (doom--update-files old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

(kb/leader-keys
  "fR" '(kb/move-this-file :which-key "Rename current file")
  )

;;;; Yank current buffer's file-path
(defun kb/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(kb/leader-keys
  "fy" '(kb/yank-buffer-filename :which-key "Yank file-path")
  )

;;;; Kill
;;;;; Kill all buffers
(defun kb/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;;;; Kill this file
(defun kb/delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.

      If PATH is not specified, default to the current buffer's file.

      If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          ;; ;; Ensures that windows displaying this buffer will be switched to
          ;; ;; real buffers (`doom-real-buffer-p')
          ;; (doom/kill-this-buffer-in-all-windows buf t)
          ;; (doom--update-files path)
          (kb/kill-current-buffer)
          (message "Deleted %S" short-path))))))

;;;;; Keybinds
;; Keybinds for the aforementioned functions
(general-define-key "C-x K" 'kill-this-buffer)
(kb/leader-keys
  "bK" '(kill-this-buffer :which-key "Kill current buffer") ; Sets keybinds for kill-this-buffer function
  "fD" '(kb/delete-this-file :which-key "Delete current file")
  "qQ" '(kb/kill-all-buffers :which-key "Kill all buffers")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'faces-rcp)
;;;; Idle quote
;; Display a random quote in the minibuffer after a certain amount of idle time.
;; It's useful to get inspiration when stuck writing
(defconst kb/quotes
  "Good quotes they can be useful for creative writers as well."
  '("You can't see paradise, if you don't pedal.  - Chicken Run "
    "He who who says he can and he who says he can’t are both usually right ― Confucius"
    "Why waste time proving over and over how great you are when you could be getting better? - Dweck The Mindset"
    "You’re not a failure until you start to assign blame. - The legendary basketball coach John Wooden"
    "I could hear my heart beating. I could hear everyone's heart. I could hear the human noise we sat there making, not one of us moving, not even when the room went dark. - Raymond Carver"
    "A writer is a sum of their experiences. Go get some - Stuck in Love (2012)"
    "If there is any one secret of success, it lies in the ability to get the other person's point of view and see things from that person's angle as well as from your own. - Henry Ford"
    "People who can put themselves in the place of other people who can understand the workings of their minds, need never worry about what the future has in store for them. - Owen D. Young"
    ))

(defun kb/show-random-quotes ()
  "Show random quotes to minibuffer."
  (interactive)
  (message "%s" (nth (random (length kb/quotes))
                     kb/quotes)
           ))

(run-with-idle-timer 300 t 'kb/show-random-quotes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'convenient-functions-rcp)
;;; Commentary:
;;
;; Most of these functions are taken from elsewhere (e.g. Doom)
;;
;;; convenient-functions-rcp.el ends here
