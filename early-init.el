;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el, before
;; package and UI initialization happens.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; NOTE 2024-09-13: Suggested by "minimal emacs." By default, Emacs "updates"
;; its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Prevent loading any packages prior to init.el. Speeds up startup but packages
;; must be initizlied by `package-initialize' before any are needed.
(setq package-enable-at-startup nil)

;; Faster to disable these graphical elements before they've been initialized
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Default coding system to UTF-8
(set-language-environment "UTF-8")

;; Increase performance for buffers with lots of special characters at the
;; expense of memory
(setq inhibit-compacting-font-caches t)

;;; Native-compilations settings
;; Basic settings
(setq native-comp-jit-compilation t
      native-comp-async-report-warnings-errors 'silent ; Show in *Warnings*  buffer but don't show buffer
      native-comp-async-jobs-number
      (- (string-to-number (string-trim-right (shell-command-to-string "nproc"))) 1)) ; Use as many cores as possible

;; Make sure `eln-cache' is set. Sometimes gets set to .emacs.d directory,
;; meaning chemacs2 gets in the way.
(unless (version-list-<
         (version-to-list emacs-version)
         '(28 0 1 0))
  (when (boundp 'native-comp-eln-load-path)
    (add-to-list 'native-comp-eln-load-path
                 (expand-file-name "eln-cache/" user-emacs-directory))))

;;; early-init.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
