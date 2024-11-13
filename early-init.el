;; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum)

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

;; NOTE 2024-09-16: From Doom Emacs.
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; NOTE 2024-09-16: From Doom Emacs.
;; PERF: Disable bidirectional text scanning for a modest performance boost.
;; I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;; say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; NOTE 2024-09-16: From Doom Emacs.
;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;; reordering of bidirectional text with embedded parentheses (and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)               ; Emacs 27+ only

;; NOTE 2024-09-16: From Doom Emacs.
;; PGTK builds only: there's a timeout that adds latency to frame operations,
;; like `make-frame-invisible', which Emacs frequently calls without a guard
;; because it's inexpensive in non-PGTK builds. Lowering the timeout from the
;; default 0.1 should make childframes and packages that manipulate them (like
;; `lsp-ui', `company-box', and `posframe') feel much snappier. See
;; emacs-lsp/lsp-ui#613.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; NOTE 2024-09-16: From Doom Emacs.
;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; NOTE 2024-09-13: Suggested by "minimal emacs." By default, Emacs "updates"
;; its UI more often than it needs to
(setq idle-update-delay 1.0)

;; I defer garbage collection until after starting Emacs. See my configuration
;; of `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum)

;; NOTE 2024-10-03: From Doom Emacs.
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)

;; NOTE 2024-10-03: From Doom Emacs. If we're disabling these graphical
;; elements, it's faster to do so before they've been initialized.
;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because their manipulation of frame parameters can
;;   trigger/queue a superfluous (and expensive, depending on the window system)
;;   frame redraw at startup. The variables must be set to `nil' as well so
;;   users don't have to call the functions twice to re-enable them.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Frame defaults
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t                  ; For mouse events
      use-file-dialog nil
      use-short-answers nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t     ; REVIEW 2024-10-03: Not sure the precise effect
      inhibit-startup-echo-area-message user-login-name ; Read the docstring
      inhibit-startup-buffer-menu t)
