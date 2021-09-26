;;; performance-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Faster Emacs startup and session.
;; Other things that can help:
;; 1) Native compilation (GCCemacs)
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

;;;; Set GC threshold
;; Set the GC threshold (for our Emacs session) higher than the default
(defvar better-gc-cons-threshold (round (* 1024 1024 300)) ; In mb
  "The default value to use for `gc-cons-threshold'.

  If you experience freezing, decrease this. If you experience stuttering,
  increase this.")
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold better-gc-cons-threshold)))

;;;; Increase GC threshold for minibuffer
;; Garbage Collect when Emacs is out of focus and try to avoid garbage
;; collection when using minibuffer
(defun gc-minibuffer-setup-hook ()
  "GC threshold for when minibuffer opened."
  (setq gc-cons-threshold (* better-gc-cons-threshold 4)))
(defun gc-minibuffer-exit-hook ()
  "GC threshold for when minibuffer closed."
  (garbage-collect)
  (setq gc-cons-threshold better-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)

;;;; More leeway for Emacs subprocesses
;; Let Emacs subprocesses read more data per chunk
(setq read-process-output-max (* 1024 1024)) ; 1mb

;;;; Native-compilations settings
;; Allow async compilations occupy all the cores minus 1
;; (setq native-comp-async-jobs-number (- (string-to-number (string-trim-right (shell-command-to-string "nproc"))) 1))
(setq native-comp-async-jobs-number 0)  ; Don't compile asynchronously

;;; performance-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'performance-rcp)
