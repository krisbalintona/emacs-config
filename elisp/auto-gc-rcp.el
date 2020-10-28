;;; auto-gc-rcp.el
;;
;;; Code:

;;; BetterGCThreshold
(defvar better-gc-cons-threshold (round (* 1024 1024 0.8)) ; In mb
  "The default value to use for `gc-cons-threshold'.

  If you experience freezing, decrease this. If you experience stuttering,
  increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            ))
;;; BetterGCThreshold

;; AutoGC
;; Garbage Collect when Emacs is out of focus and try to avoid garbage collection when using minibuffer
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))
            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)
            ))
;; AutoGC

(provide 'auto-gc-rcp)
;;; Commentary:
;; Set better garbage collection timing
;;
;;; auto-gc-rcp.el ends here
