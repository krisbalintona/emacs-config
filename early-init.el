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

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Preview loading any packages prior to init.el being loading
(setq package-enable-at-startup nil)

;; Faster to disable these graphical elements before they've been initialized
(menu-bar-mode -1)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; Native-compilations settings
;; Basic settings
(setq native-comp-deferred-compilation t
      native-comp-async-report-warnings-errors 'silent ; Show in *Warnings*  buffer but don't show buffer
      native-comp-async-jobs-number (- (string-to-number (string-trim-right (shell-command-to-string "nproc"))) 1) ; Use as many cores as possible
      )

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
