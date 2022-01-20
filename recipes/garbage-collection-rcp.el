;;; garbage-collection-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Faster Emacs startup and session.
;; Other things that can help:
;; 1) Native compilation (`libgccjit')
;; 2) Native (`libjansson') JSON support (alternative Elisp parser)
;;
;; The flags I use when manually compiling Emacs are:
;; ./configure --with-dbus --with-gif --with-jpeg --with-png --with-rsvg \
;; --with-tiff --with-xft --with-xpm --with-gpm=no --with-xwidgets \
;; --with-modules --with-native-compilation --with-pgtk --with-mailutils \
;; --with-json
;; This follows ./autogen.sh. Then I run make -j$(nproc) and then make install.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;; Diagnose memory usage
;; See how Emacs is using memory. From
;; https://www.reddit.com/r/emacs/comments/ck4zb3/comment/evji1n7/?utm_source=share&utm_medium=web2x&context=3
(defun kb/diagnose-garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))

;;; Set GC threshold
;; Set the GC threshold (for our Emacs session) higher than the default. The
;; default is 800 kilobytes. This variable's units is in bytes.
(defvar better-gc-cons-threshold (round (* 1024 1024
                                           150 ; Number of megabytes
                                           ))
  "The default value to use for `gc-cons-threshold'.

  If you experience freezing, decrease this. If you experience stuttering,
  increase this.")
(add-hook 'emacs-startup-hook #'(lambda () (setq gc-cons-threshold better-gc-cons-threshold)))

;;; Increasing GC threshold
;;;; Minibuffer
;; Garbage Collect when Emacs is out of focus and try to avoid garbage
;; collection when using minibuffer
(defvar kb/gc-allow-minibuffer-gc t
  "Helper variable to make sure `gc-cons-threshold' isn't lowered in Magit by
  opening minibuffer.")

(defun kb/gc-minibuffer-setup-hook ()
  "GC threshold for when minibuffer opened."
  (when kb/gc-allow-minibuffer-gc
    (setq gc-cons-threshold (* better-gc-cons-threshold 4))
    ))
(defun kb/gc-minibuffer-exit-hook ()
  "GC threshold for when minibuffer closed."
  (garbage-collect)
  (when kb/gc-allow-minibuffer-gc
    (setq gc-cons-threshold better-gc-cons-threshold)
    ))

(add-hook 'minibuffer-setup-hook #'kb/gc-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'kb/gc-minibuffer-exit-hook)

;;;; Magit
;; Makes large repo changes in magit bearable.
(with-eval-after-load 'magit
  (defun kb/gc-magit-enter-hook ()
    "GC threshold for when magit opened."
    ;; (message (concat "ENTER BEGIN: " (number-to-string gc-cons-threshold)))
    (setq kb/gc-allow-minibuffer-gc nil)
    (setq gc-cons-threshold (* better-gc-cons-threshold 10))
    ;; (message (concat "ENTER END: " (number-to-string gc-cons-threshold)))
    )
  (defun kb/gc-magit-exit-hook (&optional KILL-BUFFER)
    "GC threshold for when magit closed."
    (when (string-match (rx (and "magit: " (*? anything) eol)) (buffer-name))
      ;; (message (concat "EXIT BEGIN: " (number-to-string gc-cons-threshold)))
      (setq kb/gc-allow-minibuffer-gc t)
      (garbage-collect)
      (setq gc-cons-threshold better-gc-cons-threshold)
      ;; (message (concat "EXIT END: " (number-to-string gc-cons-threshold)))
      ))

  (add-hook 'magit-status-mode-hook #'kb/gc-magit-enter-hook)
  (advice-add magit-bury-buffer-function :after #'kb/gc-magit-exit-hook)
  )

;;; garbage-collection-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'garbage-collection-rcp)
