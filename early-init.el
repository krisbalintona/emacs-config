;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el, before
;; package and UI initialization happens.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Defer garbage collection
;; Set as high as possible threshold for GC as early as possible in order to
;; reduce load time. This value is then lowered to a normal threshold later
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

;;; early-init.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
