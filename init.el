;; -*- lexical-binding: t; -*-

;;; Add modules and personal lisp files to `load-path'
(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))

;;; Custom
;;; Set `custom-file' but do not load it
;; Write `custom-file' in a temporary directory but don't load it.
;; This is just to avoid writing Custom options to the
;; `user-init-file'.  See
;; https://protesilaos.com/emacs/dotemacs#h:f2ffe0e9-a58d-4bba-9831-cc35940ea83f
;; for a more detailed explanation.
(setopt custom-file (make-temp-file "emacs-custom-file-" nil ".el"))

;; Make all themes safe
(setopt custom-safe-themes t)

;; TODO 2025-05-24: Document these options:
;; - `custom-buffer-style’
;; - `custom-search-field' -- useful for Android and other touchscreen
;;                            devices though
(setopt custom-theme-allow-multiple-selections t
        custom-unlispify-tag-names nil)

;;; Package.el
(setopt package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                           ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-archive-priorities '(("gnu-elpa" . 4)
                                     ("nongnu" . 3)
                                     ("gnu-elpa-devel" . 2)
                                     ("melpa" . 1))
        package-install-upgrade-built-in t

        load-prefer-newer t)

;;; Setup.el
(unless (package-installed-p 'setup)
  (package-install 'setup))

(setup (:package setup)
  ;; Mimic use-package's :after keyword.  Taken from
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-10
  (setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
    :documentation "Load the current feature after FEATURES."
    :debug '(symbolp listp))

  ;; Make adding advice easier.  Taken from
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-13
  (setup-define :advice-add
    (lambda (symbol where arglist &rest body)
      (let ((name (gensym "setup-advice-")))
        `(progn
           (defun ,name ,arglist ,@body)
           (advice-add ',symbol ,where #',name))))
    :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
    :after-loaded t
    :debug '(sexp sexp function-form)
    :indent 3)

  ;; Mide a minor mode from the mode-line.  Modified from
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-11
  (setup-define :hide-mode
    (lambda (&optional mode)
      (let* ((mode (or mode (setup-get 'mode)))
             (mode (if (string-match-p "-mode\\'" (symbol-name mode))
                       mode
                     (intern (format "%s-mode" mode)))))
        (if (boundp 'mode-line-collapse-minor-modes) ; Added in Emacs 31.1
            `(add-to-list 'mode-line-collapse-minor-modes ',mode)
          `(setq minor-mode-alist
                 (delq (assq ',mode minor-mode-alist)
                       minor-mode-alist)))))
    :documentation "Hide the mode-line lighter of the current mode.
Alternatively, MODE can be specified manually, and override the current
mode.

If `mode-line-collapse-minor-modes' exists, add the current mode to
that.  Otherwise, remove it from `minor-mode-alist'."
    :after-loaded t))

;;; No-littering.el
;; Have packages write their files in locations adhering to a
;; convention.
(setup no-littering
  (:package no-littering)

  ;; Set the package options according to the instructions found in
  ;; the package commentary.
  (eval-and-compile       ; Ensure values don't differ at compile time
    (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory) ; Config files
          no-littering-var-directory (expand-file-name "var/" user-emacs-directory))) ; Persistent files
  ;; Ensure the directories exist
  (mkdir no-littering-etc-directory t)
  (mkdir no-littering-var-directory t)

  (:require no-littering)

  ;; Sets more secure values for `auto-save-file-name-transforms',
  ;; `backup-directory-alist', and
  ;; `undo-tree-history-directory-alist'.  Read docstring for a more
  ;; detailed explanation.
  (no-littering-theme-backups))

;;; On.el
;; Package exposes a number of utility hooks and functions ported from
;; Doom Emacs.  The hooks make it easier to speed up Emacs startup by
;; providing finer-grained control of the timing at which packages are
;; loaded.  Provides the following hooks:
;; - `on-first-input-hook'
;; - `on-init-ui-hook'
;; - `on-first-file-hook'
;; - `on-switch-frame-hook'
;; - `on-first-buffer-hook'
;; - `on-switch-buffer-hook'
;; - `on-switch-window-hook'
(setup on
  (:package on)
  (:require on))

;;; Garbage collection
;; We set `gc-cons-threshold’ to a high value in early-init.el.  We
;; reset the value after initialization.
(defun krisb-gc-set-options ()
  "Set garbage collection options."
  ;; NOTE 2024-02-11: Please reference
  ;; https://emacsconf.org/2023/talks/gc/ for a statistically-informed
  ;; recommendation for GC variables
  (setopt garbage-collection-messages t
          gc-cons-threshold (* 16 1024 1024) ; 16 mb
          gc-cons-percentage 0.15))
(add-hook 'after-init-hook #'krisb-gc-set-options)

;; Diagnose memory usage: see how Emacs is using memory. From
;; https://www.reddit.com/r/emacs/comments/ck4zb3/comment/evji1n7/?utm_source=share&utm_medium=web2x&context=3
(defun krisb-diagnose-garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))

;;;; GCMH
;; Garbage collect when idle.
;; TODO 2025-05-23: Document
;; - `gcmh-low-cons-threshold’
(setup gcmh
  ;; Don't load gcmh if this Emacs uses the incremental garbage
  ;; collector (IGC) from the MPS branch.
  (:only-if (not (string-match-p "--with-mps=yes" system-configuration-options)))
  (:package gcmh)

  (:hide-mode)

  (:hook-into on-first-buffer-hook)
  (add-hook 'minibuffer-setup-hook #'krisb-gcmh-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'krisb-gcmh-minibuffer-exit)

  (setopt gcmh-high-cons-threshold gc-cons-threshold
          ;; If the idle delay is too long, we run the risk of runaway
          ;; memory usage in busy sessions.  And if it's too low, then
          ;; we may as well not be using gcmh at all.  See
          ;; https://emacsconf.org/2023/talks/gc/ for a
          ;; statistically-informed analysis of GC in Emacs.
          gcmh-idle-delay 5
          gcmh-verbose garbage-collection-messages)

  ;; Increase GC threshold when in minibuffer
  (defvar krisb-gc-minibuffer--original gcmh-high-cons-threshold
    "Temporary variable to hold `gcmh-high-cons-threshold'")

  (defun krisb-gcmh-minibuffer-setup ()
    "Temporarily have \"limitless\" `gc-cons-threshold'."
    (when gcmh-mode
      (setq gcmh-high-cons-threshold most-positive-fixnum)))

  (defun krisb-gcmh-minibuffer-exit ()
    "Restore value of `gc-cons-threshold'."
    (setq gcmh-high-cons-threshold krisb-gc-minibuffer--original))

  ;; Increase `gc-cons-threshold' while using corfu too, like we do
  ;; for the minibuffer
  (with-eval-after-load 'corfu
    (advice-add 'completion-at-point :before #'krisb-gcmh-minibuffer-setup)
    (advice-add 'corfu-quit :after #'krisb-gcmh-minibuffer-exit)
    (advice-add 'corfu-insert :after #'krisb-gcmh-minibuffer-exit)))

;;; Themes

(defun krisb-enable-theme-time-of-day (light-theme dark-theme &optional day-start night-start)
  "Enables LIGHT-THEME or DARK-THEME based on time of day.
LIGHT-THEME and DARK-THEME are a symbol for the name of a theme.

Night time begins at NIGHT-START hour and daytime begins at DAY-START
hour.  If NIGHT-START is nil, default to 19.  If DAY-START is nil,
default to 8."
  (interactive)
  (let ((hour (string-to-number (format-time-string "%H")))
        (day-start (or day-start 8))
        (night-start (or night-start 19)))
    ;; Dark theme between NIGHT-START and DAY-START
    (load-theme (if (or (<= night-start hour) (<= hour day-start))
                    dark-theme light-theme)))
  ;; Disable the remainder of the enabled themes.  We do this at the
  ;; end to prevent going from a state of having a theme to having no
  ;; theme, which would often cause a sudden drastic but momentary
  ;; change in color (e.g. dark theme to light theme)
  (mapc #'disable-theme (cdr custom-enabled-themes)))

;;;; Ef-themes
(setup ef-themes
  (:package ef-themes)
  (:require ef-themes)

  (bind-keys ("<f8>" . ef-themes-toggle)
             ("C-<f8>" . ef-themes-select)
             ("M-<f8>" . ef-themes-rotate))
  (setopt ef-themes-to-toggle '(ef-duo-light ef-duo-dark))
  (krisb-enable-theme-time-of-day (car ef-themes-to-toggle) (cadr ef-themes-to-toggle)))
