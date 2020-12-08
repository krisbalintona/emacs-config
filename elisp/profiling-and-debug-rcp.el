;;; profiling-and-debug-rcp.el --- Summary
;;
;; Install and configure profiling and debug packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Emacs-startup-profiler
;; Profile my startup time without leaving Emacs
(use-package esup
  :commands esup)

;;;; Explain-pause-mode
;; Profile what's causing your Emacs to slow down
(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  )

;;;; Bug-hunter
;; Easy way to see if there is an error in your config files
;; NOTE: Not sure if this looks through literate configs?
(use-package bug-hunter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'profiling-and-debug-rcp)
;;; Commentary:
;;
;; Profile Emacs startup and Emacs performance as well as debugging startup
;;
;;; profiling-and-debug-rcp.el ends here
