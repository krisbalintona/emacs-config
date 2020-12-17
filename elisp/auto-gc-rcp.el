;;; auto-gc-rcp.el --- Summary
;;
;; Set better garbage collection timing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Set the GC threshold
(defvar better-gc-cons-threshold (round (* 1024 1024 0.8)) ; In mb
  "The default value to use for `gc-cons-threshold'.

  If you experience freezing, decrease this. If you experience stuttering,
  increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            ))

;;;; AutoGC
;; Garbage Collect when Emacs is out of focus and try to avoid garbage
;; collection when using minibuffer
(defun gc-minibuffer-setup-hook ()
  "GC threshold for when minibuffer opened."
  (setq gc-cons-threshold (* better-gc-cons-threshold 3)))
(defun gc-minibuffer-exit-hook ()
  "GC threshold for when minibuffer closed."
  (garbage-collect)
  (setq gc-cons-threshold better-gc-cons-threshold))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function '(lambda ()
                                                        (unless (frame-focus-state)
                                                          (garbage-collect))))

              (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
              (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'auto-gc-rcp)
;;; Commentary:
;;
;;; auto-gc-rcp.el ends here
