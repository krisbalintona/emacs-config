;;; performance-rcp.el --- Summary
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

;;; Set GC threshold
;; Set the GC threshold (for our Emacs session) higher than the default
(defvar better-gc-cons-threshold (round (* 1024 1024 225)) ; In mb
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
  (interactive)
  (when kb/gc-allow-minibuffer-gc
    (setq gc-cons-threshold (* better-gc-cons-threshold 4))
    ))
(defun kb/gc-minibuffer-exit-hook ()
  "GC threshold for when minibuffer closed."
  (interactive)
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
    (interactive)
    ;; (message (concat "ENTER BEGIN: " (number-to-string gc-cons-threshold)))
    (setq kb/gc-allow-minibuffer-gc nil)
    (setq gc-cons-threshold (* better-gc-cons-threshold 10))
    ;; (message (concat "ENTER END: " (number-to-string gc-cons-threshold)))
    )
  (defun kb/gc-magit-exit-hook (&optional KILL-BUFFER)
    "GC threshold for when magit closed."
    (interactive)
    ;; (message (concat "EXIT BEGIN: " (number-to-string gc-cons-threshold)))
    (setq kb/gc-allow-minibuffer-gc t)
    (garbage-collect)
    (unless KILL-BUFFER (setq gc-cons-threshold better-gc-cons-threshold))
    ;; (message (concat "EXIT END: " (number-to-string gc-cons-threshold)))
    )

  (add-hook 'magit-pre-display-buffer-hook #'kb/gc-magit-enter-hook)
  (advice-add magit-bury-buffer-function :after #'kb/gc-magit-exit-hook)
  )

;;; More leeway for Emacs subprocesses
;; Let Emacs subprocesses read more data per chunk
(setq read-process-output-max (* 1024 1024)) ; 1mb

;;; Native-compilations settings
;; Allow async compilations occupy all the cores minus 1
(setq native-comp-async-jobs-number (- (string-to-number (string-trim-right (shell-command-to-string "nproc"))) 1))

;;; performance-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'performance-rcp)
