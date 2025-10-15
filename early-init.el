;; -*- lexical-binding: t; -*-

;; Redirect the .eln cache to a directory that adheres to
;; no-littering's convention of placing data files in the var
;; subdirectory.  Taken from
;; https://github.com/emacscollective/no-littering?tab=readme-ov-file#native-compilation-cache
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Graphical elements of Emacs frame
;; NOTE 2024-10-03: From Doom Emacs.  If we're disabling these
;; graphical elements, it's faster to do so before they've been
;; initialized.
;; HACK: I intentionally avoid calling `menu-bar-mode',
;;   `tool-bar-mode', and `scroll-bar-mode' because their manipulation
;;   of frame parameters can trigger/queue a superfluous (and
;;   expensive, depending on the window system) frame redraw at
;;   startup. The variables must be set to `nil' as well so users
;;   don't have to call the functions twice to re-enable them.
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
      use-dialog-box t                  ; For mouse events
      use-file-dialog nil
      inhibit-x-resources t) ; REVIEW 2024-10-03: Not sure the precise effect

(add-to-list 'default-frame-alist '(alpha-background . 100))

;; Startup screen
(setopt inhibit-splash-screen t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name ; Read the docstring
        inhibit-startup-buffer-menu t)

;; Native-compilations settings
(setq native-comp-jit-compilation t
      native-comp-async-report-warnings-errors 'silent ; Show in *Warnings*  buffer but don't show buffer
      native-comp-async-jobs-number
      (- (string-to-number (string-trim-right (shell-command-to-string "nproc"))) 1)) ; Use as many cores as possible

;; NOTE 2024-09-16: From Doom Emacs.  Usually, a second,
;; case-insensitive pass over `auto-mode-alist' is time wasted.
(setopt auto-mode-case-fold nil)

;; Defer garbage collection until after starting Emacs by raising
;; `gc-cons-threshold' to as high a number as possible.  See also my
;; configuration of `gcmh-mode'.
(setopt gc-cons-threshold most-positive-fixnum)
