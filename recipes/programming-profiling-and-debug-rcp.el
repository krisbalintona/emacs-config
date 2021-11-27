;;; programming-profiling-and-debug-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Profile Emacs startup and Emacs performance as well as debugging startup.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)

;;; Emacs-startup-profiler
;; Profile my startup time without leaving Emacs
(use-package esup
  :custom
  (esup-user-init-file (expand-file-name "init.el" user-emacs-directory))
  (esup-depth 10)
  (esup-child-max-depth 10)
  )

;;; Explain-pause-mode
;; Profile what's causing your Emacs to slow down
(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  )

;;; Bug-hunter
;; Easy way to see if there is an error in your config files
;; NOTE: Not sure if this looks through literate configs?
(use-package bug-hunter)

;;; programming-profiling-and-debug-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-profiling-and-debug-rcp)
