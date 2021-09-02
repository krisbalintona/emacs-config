;;; auto-gc-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Faster startup from better garbage collection timing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Set GC threshold
;; Set the GC threshold (for our Emacs session) higher than the default
(defvar better-gc-cons-threshold (round (* 1024 1024 200)) ; In mb
  "The default value to use for `gc-cons-threshold'.

  If you experience freezing, decrease this. If you experience stuttering,
  increase this.")
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold better-gc-cons-threshold)))

;;;; Increase GC threshold for minibuffer
;; Garbage Collect when Emacs is out of focus and try to avoid garbage
;; collection when using minibuffer
(defun gc-minibuffer-setup-hook ()
  "GC threshold for when minibuffer opened."
  (setq gc-cons-threshold (* better-gc-cons-threshold 3)))
(defun gc-minibuffer-exit-hook ()
  "GC threshold for when minibuffer closed."
  (garbage-collect)
  (setq gc-cons-threshold better-gc-cons-threshold))

(add-hook 'after-focus-change-hook (lambda ()
                                     (unless (frame-focus-state)
                                       (garbage-collect))
                                     ))
(add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)

;;; auto-gc-rcp.el ends here
;;;; More leeway for Emacs subprocesses
;; Let Emacs subprocesses read more data per chunk
(setq read-process-output-max (* 1024 1024)) ; 1mb

;;; performance-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'auto-gc-rcp)
