;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el, before
;; package and UI initialization happens.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Runtime optimizations
;; NOTE 2024-09-16: From Doom Emacs. PERF: A second, case-insensitive pass over
;; `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; NOTE 2024-09-16: From Doom Emacs. PERF: Disable bidirectional text scanning
;; for a modest performance boost. I've set this to `nil' in the past, but the
;; `bidi-display-reordering's docs say that is an undefined state and suggest
;; this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; NOTE 2024-09-16: From Doom Emacs. PERF: Disabling BPA makes redisplay faster,
;; but might produce incorrect reordering of bidirectional text with embedded
;; parentheses (and other bracket characters whose 'paired-bracket' Unicode
;; property is non-nil).
(setq bidi-inhibit-bpa t)               ; Emacs 27+ only

;; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; NOTE 2024-09-13: Suggested by "minimal emacs." By default, Emacs "updates"
;; its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Prevent loading any packages prior to init.el. Speeds up startup but packages
;; must be initizlied by `package-initialize' before any are needed.
(setq package-enable-at-startup nil)

;; NOTE 2024-09-16: From Doom Emacs. PGTK builds only: there's a timeout that
;; adds latency to frame operations, like `make-frame-invisible', which Emacs
;; frequently calls without a guard because it's inexpensive in non-PGTK builds.
;; Lowering the timeout from the default 0.1 should make childframes and
;; packages that manipulate them (like `lsp-ui', `company-box', and `posframe')
;; feel much snappier. See emacs-lsp/lsp-ui#613.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; NOTE 2024-09-16: From Doom Emacs. Font compacting can be terribly expensive,
;; especially for rendering icon fonts on Windows. Whether disabling it has a
;; notable affect on Linux and Mac hasn't been determined, but do it anyway,
;; just in case. This increases memory usage, however!
(setq inhibit-compacting-font-caches t)

;; These are the three ways to increase scrolling performance.
;; See (info "(emacs) Scrolling") for details.
;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)
;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;;;; UI
;; Faster to disable these graphical elements before they've been initialized
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message "Hello 👋")

;;;; Native-compilations settings
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
