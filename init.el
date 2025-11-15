;; -*- lexical-binding: t; -*-

(setq force-load-messages t)

(setopt user-full-name "Kristoffer Balintona"
        user-mail-address "krisbalintona@gmail.com")

;;; Add bespoke lisp modules to `load-path'
(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))

;;; Add WIP "monkey patches" to `load-path'
(defvar krisb-wip-monkeypatches-dir (expand-file-name "wip" user-emacs-directory)
  "Directory holding my WIP \"monkey patches.\"
\"Monkey patches\" are files that are different version of upstream
files.  Either for third-party packages or Emacs itself.

This directory is added to `load-path' in order for these WIP files to
be loaded instead of their upstream versions.  This allows me to work on
patches to, say, org-mode or Emacs, by loading those versions at
startup.")

(unless (file-exists-p krisb-wip-monkeypatches-dir)
  (message "Making directory %s..." krisb-wip-monkeypatches-dir)
  (make-directory krisb-wip-monkeypatches-dir))

(add-to-list 'load-path krisb-wip-monkeypatches-dir)

;; Symbolically link my monkeypatches to `krisb-wip-monkeypatches-dir'
(defmacro krisb-wip-monkeypatch-symlink (path)
  "Symlink PATH into `krisb-wip-monkeypatches-dir'.
If PATH is already symlinked into `krisb-wip-monkeypatches-dir', then do
nothing."
  `(let* ((patch-path (expand-file-name ,path))
          (link-path
           (expand-file-name (file-name-nondirectory patch-path) krisb-wip-monkeypatches-dir)))
     (unless (file-exists-p link-path)
       (message "Symlinking %s to %s..." patch-path link-path)
       (make-symbolic-link patch-path link-path))))

;; Until the vtable work I’ve been testing is upstreamed, we manually
;; load that file
(krisb-wip-monkeypatch-symlink "~/emacs-repos/packages/emacs-vtable-ship-mints/lisp/emacs-lisp/vtable.el")

;; Load my bespoke version before org's version of org-capture
(krisb-wip-monkeypatch-symlink "~/emacs-repos/packages/org-mode/lisp/org-capture.el")

;;; Meta-configuration

;;;; Bespoke helpers
;;;;; Variables
;; FIXME 2025-05-20: If the path denoted by `krisb-folio-directory'
;; does not exist, other packages that depend on this value are given
;; a non-existing path, likely resulting in errors.  We might solve
;; this by turning this into a function instead, returning nil if it
;; doesn't exist, thereby avoiding passing a non-existing file path to
;; these packages.
(defvar krisb-folio-directory (expand-file-name "org-database" "~/Documents")
  "The directory holding my org files.")

(defvar krisb-notes-directory (expand-file-name "notes" krisb-folio-directory)
  "My notes directory.")

(defvar krisb-org-agenda-directory (expand-file-name "agenda" krisb-folio-directory)
  "The directory holding my main org-agenda files.")

(defvar krisb-org-agenda-main-file (expand-file-name "todo.org" krisb-org-agenda-directory)
  "My main org-agenda file.")

(defvar krisb-org-archive-directory (expand-file-name "archive" krisb-folio-directory)
  "The archive directory for my org files.")

(defvar krisb-bibliography-files (list (expand-file-name "master-lib.bib" krisb-folio-directory))
  "A list of my bibliography (.bib) files.")

;;;;; Directories
(defvar krisb-blog-manuscripts-directory (expand-file-name "manuscripts/blog" krisb-notes-directory)
  "The directory for my pre-export blog files.")

(defvar krisb-email-directory (expand-file-name "emails/" "~/Documents/")
  "Directory that houses my local email files.")

(defvar krisb-email-drafts-directory (expand-file-name "drafts" krisb-email-directory)
  "Directory that houses my local email drafts.")

;;;;; Functions
(defun krisb-wayland-p ()
  "Return non-nil if Emacs is under Wayland."
  (getenv "WAYLAND_DISPLAY"))

;;;;; Keymaps
(defvar-keymap krisb-note-keymap
  :doc "Prefix for my note-taking needs.")
(bind-key "C-c n" krisb-note-keymap 'global-map)

(defvar-keymap krisb-open-keymap
  :doc "Prefix for opening various hings.")
(bind-key "C-c o" krisb-open-keymap 'global-map)

(defvar-keymap krisb-toggle-keymap
  :doc "Prefix for toggling stuff.")
(bind-key "C-c t" krisb-toggle-keymap 'global-map)

;;;; Package.el
(setopt package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                           ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-archive-priorities '(("gnu-elpa" . 4)
                                     ("nongnu" . 3)
                                     ("gnu-elpa-devel" . 2)
                                     ("melpa" . 1))
        package-install-upgrade-built-in t)

;; 2025-10-23 TODO: Revisit this.  Ideally, should be more safe than
;; setting this value to an outright t
(setopt package-vc-allow-build-commands t)

;;;; Setup.el
(unless (package-installed-p 'setup)
  (package-install 'setup))

;; Define my own setup.el macros
(with-eval-after-load 'setup
  ;; Mimic use-package's :after keyword.  Taken from
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-10.
  (setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
    :documentation "Load the current feature after FEATURES."
    :debug '(symbolp listp))

  ;; Make adding advice easier.  Modified from
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-13.
  (setup-define :advice-def
    (lambda (symbol where suffix arglist &rest body)
      (let ((name (concat "setup-advice-" suffix)))
        `(progn
           (defun ,name ,arglist ,@body)
           (advice-add ',symbol ,where #',name))))
    :documentation "Add a piece of advice on a function.
Provide a function symbol, a HOW from `advice-add', a string that acts
as the suffix for the advice name, and the advice function definition.
See `advice-add' for more details."
    :debug '(sexp sexp sexp function-form)
    :after-loaded t
    :indent 3)

  (setup-define :advice
    (lambda (symbol where)
      `(advice-add ',symbol ,where #',(setup-get 'func)))
    :documentation "Advise a function the current function.
Provide a function symbol and a HOW from `advice-add'.  See `advice-add'
for more details."
    :debug '(sexp sexp)
    :after-loaded t)

  ;; Mide a minor mode from the mode-line.  Modified from
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-11.
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
    :after-loaded t)

  ;; Customize faces.  Taken from
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-22.
  (setup-define :face
    (lambda (face spec) `(custom-set-faces (backquote (,face ,spec))))
    :documentation "Customize FACE to SPEC."
    :signature '(face spec ...)
    :debug '(setup)
    :repeatable t
    :after-loaded t)

  ;; Macro for `bind-keys'
  (setup-define :bind-keys
    (lambda (&rest args)
      `(bind-keys ,@args))
    :documentation "Bind KEY to COMMAND in current map."
    :after-loaded t)

  ;; Macro for `executable-find'.  Taken from
  ;; https://github.com/hiecaq/guix-config?tab=readme-ov-file#setupel
  (setup-define :needs
    (lambda (executable)
      `(unless (executable-find ,executable)
         ,(setup-quit)))
    :documentation "If EXECUTABLE is not in the path, stop here."
    :debug '(form))

  ;; Redefine :require macro to use current feature.
  (setup-define :require
    (lambda () `(require ',(setup-get 'feature) nil t))
    :documentation "Try to require the current feature, or stop evaluating body.")

  ;; Catch `setup-quit' within a scope.  Taken from
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-9
  (setup-define :with-local-quit
    (lambda (&rest body)
      `(catch ',(setup-get 'quit)
         ,@body))
    :documentation "Prevent any reason to abort from leaving beyond BODY."
    :debug '(setup))
  
  ;; Unconditional quit.  Taken from
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-8.
  (setup-define :quit
    #'setup-quit
    :documentation "Unconditionally abort the evaluation of the current body.")

  ;; Macro for adding to `package-pinned-packages'.  To use properly,
  ;; make sure this is placed before any calls to `package-install'
  ;; (e.g., via :package) for every pinned package.
  (setup-define :pin
    (lambda (package archive)
      `(add-to-list 'package-pinned-packages '(,package ,archive)))
    :documentation "Pin PACKAGE to ARCHIVE.
PACKAGE is a symbol representing a package.  ARCHIVE is a string
representing an archive (it should be the car of an element in
package-archives, e.g. \"gnu\")."))

;; Add setup entries to imenu.  Modified from
;; https://www.emacswiki.org/emacs/SetupEl#h5o-31.
(with-eval-after-load 'setup
  (with-eval-after-load 'imenu
    (defun krisb-setup-with-imenu ()
      "Configure imenu to identify setup.el forms."
      (setf (map-elt imenu-generic-expression "Setup")
            (list (rx line-start (0+ blank)
                      "(setup" (1+ blank)
                      ;; Add here items that can define a feature:
                      (or (group-n 1 (1+ (or (syntax word)
                                             (syntax symbol))))
                          (seq "(:" (or "package" "elpaca" "require")
                               (1+ blank)
                               (group-n 1 (1+ (or (syntax word)
                                                  (syntax symbol)))))))
                  1)))
    (add-hook 'emacs-lisp-mode-hook #'krisb-setup-with-imenu)

    (with-eval-after-load 'consult-imenu
      (push '(?s "Setup")
            (plist-get (cdr (assoc 'emacs-lisp-mode consult-imenu-config)) :types)))))

;;;; No-littering.el
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

  ;; Require only after setting `no-littering-etc-directory' and
  ;; `no-littering-var-directory'
  (:require)

  ;; Sets more secure values for `auto-save-file-name-transforms',
  ;; `backup-directory-alist', and
  ;; `undo-tree-history-directory-alist'.  Read docstring for a more
  ;; detailed explanation.
  (no-littering-theme-backups))

;;;; On.el
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
  (:require))

;;;; El-patch
(setup el-patch
  ;; Elpaca: :ensure (:wait t)
  (:package el-patch)
  (:require))

;;; Miscellaneous options for built-ins
(setup emacs
  ;; Always load newest byte code
  (setopt load-prefer-newer t)
  ;; Enable all disabled commands
  (setopt disabled-command-function nil)
  
  ;; Don’t warn when advising
  (setopt ad-redefinition-action 'accept)
  
  ;; Set `cursor-type' based on major mode
  (setup emacs
    (defun krisb-set-cursor-code ()
      "Set cursor settings for `prog-mode'."
      (setq-local cursor-type 'box
                  cursor-in-non-selected-windows 'hollow))
    (add-hook 'prog-mode-hook #'krisb-set-cursor-code)
  
    (defun krisb-set-cursor-prose ()
      "Set cursor settings for prose major modes."
      (setq-local cursor-type '(bar . 2)
                  cursor-in-non-selected-windows 'hollow))
    (dolist (hook '(org-mode-hook
                    markdown-mode-hook
                    git-commit-setup-hook
                    log-edit-mode-hook
                    message-mode-hook))
      (add-hook hook #'krisb-set-cursor-prose)))
  
  ;; Word wrapping.  Continue wrapped lines at whitespace rather than
  ;; breaking in the middle of a word.
  (setopt word-wrap t)
  
  ;; Disable the ring-bell (it's annoying)
  (setopt ring-bell-function #'ignore)
  
  ;; Hide `buffer-face-mode' minor-mode lighter
  (setup face-remap
    
    (:hide-mode buffer-face-mode))
  
  ;; Follow symlinks when opening files
  (setopt find-file-visit-truename t)
  
  ;; Move files into trash directory
  (setopt trash-directory (no-littering-expand-var-file-name "trash")
          delete-by-moving-to-trash t)
  
  (defun krisb-empty-trash ()
    "Empty the trash directory."
    (interactive)
    (if delete-by-moving-to-trash
        (let ((size (string-trim (shell-command-to-string (concat"du -sh " trash-directory " | cut -f1")))))
          (when (yes-or-no-p (format "Empty trash directory of %s size? " size))
            (save-window-excursion (async-shell-command (concat "rm -rf " trash-directory "/*")))))
      (message "delete-by-moving-to-trash is nil; not emptying trash")))
  
  ;; Don't create lock files
  (setopt create-lockfiles nil)
  
  ;; TODO 2025-05-22: Document the `duplicate-line-final-position'and
  ;; `duplicate-region-final-position' user options
  (bind-key "C-x ;" #'duplicate-dwim)
  
  ;; Show context menu from right-click
  (when (display-graphic-p) (context-menu-mode 1))
  
  ;; Don’t wait until yanking to put clipboard text into `kill-ring’
  (setopt save-interprogram-paste-before-kill t)
  
  ;; Wayland compatibility
  (when (krisb-wayland-p)
    (setopt interprogram-cut-function
            (lambda (text)
              (start-process "wl-copy" nil "wl-copy"
                             "--trim-newline" "--type" "text/plain;charset=utf-8" text))))
  
  ;; Enable `delete-selection-mode'.  When selecting text, if typing new
  ;; text, replace the selected text with the new text
  (delete-selection-mode t)
  
  ;; Insert spaces instead of tab characters.  The below disables
  ;; `indent-tabs-mode' globally
  (setopt indent-tabs-mode nil)
  
  ;; Use DWIM case commands
  (bind-keys
   ([remap upcase-word] . upcase-dwim)
   ([remap downcase-word] . downcase-dwim)
   ([remap capitalize-word] . capitalize-dwim))
  
  ;; Set `sentence-end-double-space' conditionally
  (defun krisb-sentence-end-double-space-setup ()
    "Set up the value for `sentence-end-double-space'."
    (setq-local sentence-end-double-space
                (cond ((derived-mode-p '(prog-mode conf-mode log-edit-mode)) t)
                      ((derived-mode-p '(text-mode wombag-show-mode)) nil))))
  (add-hook 'after-change-major-mode-hook #'krisb-sentence-end-double-space-setup))

;;; Custom
;;; Set and load custom file
(setopt custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Make all themes safe
(setopt custom-safe-themes t)

;; TODO 2025-05-24: Document these options:
;; - `custom-buffer-style’
;; - `custom-search-field' -- useful for Android and other touchscreen
;;                            devices though
(setopt custom-theme-allow-multiple-selections t
        custom-unlispify-tag-names nil)

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

  (add-hook 'on-first-buffer-hook #'gcmh-mode)

  (setopt gcmh-high-cons-threshold gc-cons-threshold
          ;; If the idle delay is too long, we run the risk of runaway
          ;; memory usage in busy sessions.  And if it's too low, then
          ;; we may as well not be using gcmh at all.  See
          ;; https://emacsconf.org/2023/talks/gc/ for a
          ;; statistically-informed analysis of GC in Emacs.
          gcmh-idle-delay 5
          gcmh-verbose garbage-collection-messages))

;; Increase GC threshold when in minibuffer
(with-eval-after-load 'gcmh
  (defvar krisb-gc-minibuffer--original gcmh-high-cons-threshold
    "Temporary variable to hold `gcmh-high-cons-threshold'")

  (defun krisb-gcmh-minibuffer-setup ()
    "Temporarily have \"limitless\" `gc-cons-threshold'."
    (when gcmh-mode
      (setq gcmh-high-cons-threshold most-positive-fixnum)))

  (defun krisb-gcmh-minibuffer-exit ()
    "Restore value of `gc-cons-threshold'."
    (when gcmh-mode
      (setq gcmh-high-cons-threshold krisb-gc-minibuffer--original)))

  (add-hook 'minibuffer-setup-hook #'krisb-gcmh-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'krisb-gcmh-minibuffer-exit)

  ;; Increase `gc-cons-threshold' while using corfu too, like we do
  ;; for the minibuffer
  (with-eval-after-load 'corfu
    (advice-add 'completion-at-point :before #'krisb-gcmh-minibuffer-setup)
    (advice-add 'corfu-quit :after #'krisb-gcmh-minibuffer-exit)
    (advice-add 'corfu-insert :after #'krisb-gcmh-minibuffer-exit)))

;;; Try
(setup try
  (:package try))

;;; Fontaine
;; Define and apply face presets.
(setup fontaine
  ;; Elpaca: :ensure (:wait t) ; To have faces set ASAP during startup
  (:package fontaine)
  (:require)

  (setopt fontaine-latest-state-file
          (no-littering-expand-var-file-name "fontaine/fontaine-latest-state.eld")
          fontaine-presets
          '((default-wsl2
             :default-height 180
             :inherit iosevka-variants)
            (iosevka-variants
             ;; NOTE 2025-04-14: On Arch Linux, Iosevka fonts have associated packages
             ;; for each variant in the AUR (though not necessarily the Nerd Fonts
             ;; versions).
             :default-family "IosevkaTermSS 11 Nerd Font" ; 2025-04-14: Must be a bug that there is a space between "SS" and "11" in the font name
             :fixed-pitch-family "Iosevka Nerd Font"
             :mode-line-active-family "Iosevka Aile Nerd Font"
             :mode-line-inactive-family "Iosevka Aile Nerd Font")
            ;; Below are the shared fallback properties. I leave them there also as
            ;; reference for all possible properties
            (t
             ;; Alternatives:
             :default-family "IosevkaSS04 Nerd Font"
             :default-height 165

             ;; Alternatives
             ;; "Hack Nerd Font Mono"
             :fixed-pitch-family "Iosevka"

             ;; 2025-04-21: This is my own bespoke setting.  Fontaine works fine with
             ;; it set; I use it elsewhere (e.g., eat.el).
             :term-family "IosevkaTermSS04 Nerd Font" ; For terminals

             ;; Alternatives:
             ;; "LiterationSerif Nerd Font"       ; Variable
             ;; "Latin Modern Mono Prop"          ; Monospace
             ;; "Sans Serif"
             ;; "Open Sans" (1.1 height)
             :variable-pitch-family "Overpass Nerd Font Propo"
             :variable-pitch-height 1.2

             :mode-line-active-family "JetBrainsMono Nerd Font"
             :mode-line-active-height 0.93

             :mode-line-inactive-family "JetBrainsMono Nerd Font"
             :mode-line-inactive-height 0.93

             :header-line-height 1.0

             :tab-bar-family "Overpass Nerd Font"
             :tab-bar-height 0.93)))

  ;; 2025-04-14: I manually create the parent directory if it doesn't
  ;; already exist; this is not yet implemented upstream, so I do it
  ;; manually here for fresh installs of Emacs.
  (make-directory (file-name-directory fontaine-latest-state-file) t)

  ;; Set the last preset or fall back to desired style from
  ;; `fontaine-presets'
  (when (file-exists-p fontaine-latest-state-file)
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default)))

  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  (fontaine-mode 1))

;; Leverage with pulsar
(with-eval-after-load 'fontaine
  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

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


;; Frame background transparency toggle
(defun krisb-frame-toggle-transparency (&optional arg)
  "Toggle the value of `alpha-background'.
Toggles between 100 and 72 by default.  Can choose which value to change
to if called with ARG, or any prefix argument."
  (interactive "P")
  (let ((transparency (pcase arg
                        ((pred numberp) arg)
                        ((pred car) (read-number "Change the transparency to which value (0-100)? "))
                        (_
                         (pcase (frame-parameter nil 'alpha-background)
                           (72 100)
                           (100 72)
                           (t 100))))))
    (set-frame-parameter nil 'alpha-background transparency)))
(bind-key "<f9>" #'krisb-frame-toggle-transparency)

;;;; Ef-themes
(setup ef-themes
  (:package ef-themes)
  (:require)

  (:bind-keys ("<f8>" . ef-themes-toggle)
              ("C-<f8>" . ef-themes-select)
              ("M-<f8>" . ef-themes-rotate))
  (setopt ef-themes-to-toggle '(ef-duo-light ef-duo-dark))
  (krisb-enable-theme-time-of-day (car ef-themes-to-toggle) (cadr ef-themes-to-toggle)))

;;; Tab-bar
(setup tab-bar

  (:bind-keys :map tab-prefix-map
              ("w" . tab-bar-move-window-to-tab)
              ("w" . tab-bar-move-window-to-tab)
              ("c" . tab-bar-change-tab-group)
              ("C-S-g" . tab-bar-move-tab-to-group)
              ("D" . tab-bar-close-group-tabs)
              :repeat-map krisb-tab-bar-repeat-map
              ("C-c <left>" . tab-bar-history-back)
              ("C-c <right>" . tab-bar-history-forward)
              :continue
              ("<left>" . tab-bar-history-back)
              ("<right>" . tab-bar-history-forward))

  (setopt tab-bar-close-button-show nil
          tab-bar-close-last-tab-choice 'delete-frame
          tab-bar-new-tab-choice 'clone
          tab-bar-select-tab-modifiers '(meta)
          tab-bar-tab-hints t
          tab-bar-show t
          tab-bar-separator " "
          tab-bar-show-inactive-group-tabs t
          tab-bar-format
          '(tab-bar-format-tabs-groups
            tab-bar-separator
            tab-bar-format-align-right
            tab-bar-format-global))

  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;;; Completion and minibuffer
;; TODO 2025-05-27: Document this advice by vertico to show
;; `completing-read-multiple' separator on Emacs versions below 31:
;; https://github.com/minad/vertico#completing-read-multiple.
(setup minibuffer

  (setopt completion-styles '(substring initials flex)
          ;; TODO 2025-10-17: Revisit this
          completion-pcm-leading-wildcard t) ; Emacs 31

  ;; A non-exhaustive list of known completion categories:
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;;
  ;; You can find out the category of the completion canddiate at
  ;; point by evaluating the form below in the minibuffer when the
  ;; point is on a completion candidate:
  ;;  (completion-metadata-get (completion-metadata (minibuffer-contents) minibuffer-completion-table minibuffer-completion-predicate) 'category)
  ;;
  ;; TODO 2025-10-15: Revisit these:
  ;; (setopt completion-category-defaults
  ;;         '((calendar-month (display-sort-function . identity)))
  ;;         completion-category-overrides
  ;;         '((file (styles . (partial-completion flex)))))
                                        ;  Include `partial-completion' to enable wildcards and partial paths.

  ;; How do we want to treat case for varioust types of completion?
  (setopt completion-ignore-case t
          read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t))

(setup minibuffer

  ;; TODO 2025-10-14: Document:
  ;; `history-delete-duplicates'
  (setopt history-length 1000)

  ;; Enable recursive minibuffers
  (setopt enable-recursive-minibuffers t)

  ;; Minibuffer prompts
  (setopt minibuffer-default-prompt-format " [%s]")
  (minibuffer-electric-default-mode 1))  ; Show default value

;;;; Hotfuzz
;; Hotfuzz is a fuzzy completion style that also can be used with its
;; bundled dynamic module for improved performance.  (See
;; https://github.com/axelf4/hotfuzz?tab=readme-ov-file#dynamic-module
;; for how to do so.)
;;
;; When done so, is a much, much faster version of the built-in flex
;; style (although it is non-greedy and has a different scoring
;; algorithm).  See
;; https://github.com/axelf4/emacs-completion-bench#readme for a
;; comparison of fuzzy completion style packages
(setup hotfuzz
  (:package (hotfuzz :url "https://github.com/axelf4/hotfuzz.git"
                     ;; Compile the dynamic module.  See
                     ;; https://github.com/axelf4/hotfuzz?tab=readme-ov-file#dynamic-module
                     :shell-command "cmake -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_FLAGS=-march=native && cmake --build build"))
  (:require)

  ;; This setting applies only to completion UIs that don't support
  ;; `completion-lazy-hilit' (vertico and corfu do, so this setting is
  ;; ignored when using those UIs)
  (setopt hotfuzz-max-highlighted-completions most-positive-fixnum)

  ;; Replace the flex completion style with the hotfuzz style after
  ;; I've set `completion-styles'
  (with-eval-after-load 'minibuffer
    (cl-nsubstitute 'hotfuzz 'flex completion-styles))

  ;; We have to change some internal variables consult uses if the
  ;; dynamic module is compiled.  See
  ;; https://github.com/axelf4/hotfuzz?tab=readme-ov-file#dynamic-module
  (when (featurep 'hotfuzz-module)
    (with-eval-after-load 'consult
      (setq consult--tofu-char #x100000
            consult--tofu-range #x00fffe))))

;;;; Orderless
(setup orderless
  (:package orderless)

  (with-eval-after-load 'orderless
    (setopt orderless-matching-styles
            '(orderless-regexp
              orderless-prefixes
              orderless-initialism
              ;; orderless-literal
              ;; orderless-literal-prefix
              ;; orderless-flex
              )
            orderless-component-separator 'orderless-escapable-split-on-space))

  (add-to-list 'completion-styles 'orderless :append)

  ;; TODO 2025-05-20: Revisit this.
  ;; ;; Eglot forces `flex' by default.
  ;; (add-to-list 'completion-category-overrides '(eglot (styles . (orderless flex))))
  )

;;;; Completions buffer
;; TODO 2025-05-20: Document the following options below in the
;; literate configuration:
;; - `completion-cycle-threshold'
;; - `completion-flex-nospace’
(setup minibuffer
  ;; Completions buffer
  (setopt completions-max-height 20 ; Otherwise the completions buffer can grow to fill the entire frame
          completion-auto-help 'visible
          completion-lazy-hilit t ; Lazy highlighting for drastic performance increase; added Emacs 30.1
          completion-auto-select 'second-tab
          completions-format 'one-column
          completions-detailed t ; Show annotations for candidates (like `marginalia')
          completions-group t    ; Emacs 28
          completions-sort 'historical)) ; Emacs 30.1

;;;; Vertico and extensions
(setup vertico
  (:package vertico)

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (:bind-keys
   ("C-c v r" . vertico-repeat)
   :map vertico-map
   ("C-c v s" . vertico-suspend))

  (vertico-mode 1))

;; More convenient path modification commands
(setup vertico-directory
  (:load-after vertico)

  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;; On-demand change the type of UI
(setup vertico-multiform
  (:load-after vertico)
  (vertico-multiform-mode 1)

  (setopt vertico-multiform-categories
          '((buffer flat (vertico-sort-function . nil))
            (project-buffer flat (vertico-sort-function . nil))
            (file grid (:keymap . vertico-directory-map))
            (project-file grid)                ; For `project-find-file'
            (command flat (vertico-flat-annotate . nil))
            (symbol-help flat)
            (kill-ring (vertico-sort-function . nil))
            (color (vertico-sort-function . vertico-sort-history-length-alpha))
            (jinx grid
                  (vertico-grid-annotate . 20)
                  (vertico-grid-max-columns . 12)
                  (vertico-grid-separator
                   . #("    |    " 4 5 (display (space :width (1)) face (:inherit shadow :inverse-video t))))))
          vertico-multiform-commands
          `((pdf-view-goto-label
             (vertico-sort-function . nil))
            (".+-history" (vertico-sort-function . nil))
            (,(rx bol (or (seq "recentf" (* (any alnum))) "consult-recent-file"))
             (vertico-sort-function . nil))
            (,(rx bol (literal "customize-"))
             flat)
            (,(rx bol (or (seq (zero-or-one (literal "krisb-")) (literal "find-library"))
                          (literal "load-library")))
             flat)
            (,(rx bol (literal "consult-history"))
             (vertico-sort-function . nil))
            ;; NOTE 2025-06-18: Should these commands have a category like
            ;; e.g. `descibe-function' and `describe-keymap' do?  If so,
            ;; perhaps I can propose a patch for it.
            (,(rx (or "find-function"
                      "find-library"
                      "find-variable"))
             flat))))

;; A minimal, Ido-like UI
(setup vertico-flat
  (:load-after vertico)

  (setopt vertico-flat-annotate t
          vertico-flat-format
          `( :multiple #("\n{%s}" 0 2 (face minibuffer-prompt) 4 5 (face minibuffer-prompt))
             :single #("\n[%s]" 0 2 (face minibuffer-prompt) 2 4 (face success) 4 5 (face minibuffer-prompt))
             :prompt #("(%s)" 0 1 (face minibuffer-prompt) 3 4 (face minibuffer-prompt))
             :separator #("  |  " 0 5 (face minibuffer-prompt))
             :ellipsis ,(propertize "…" 'face 'minibuffer-prompt)
             :no-match ,(propertize "\n[No match]" 'face 'shadow)
             :spacer #(" " 0 1 (cursor t)))))

;;;; Dabbrev
(setup dabbrev
  (with-eval-after-load 'dabbrev
    (setopt dabbrev-abbrev-char-regexp "\\sw\\|\\s_" ; Look for symbols
            dabbrev-abbrev-skip-leading-regexp "[$*/=~']"
            dabbrev-case-fold-search nil ; Case is significant; see also `dabbrev-case-distinction'
            dabbrev-upcase-means-case-search t)

    (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
    (dolist (mode '(doc-view-mode
                    pdf-view-mode
                    tags-table-mode))
      (add-to-list 'dabbrev-ignored-buffer-modes mode))))

;;; Exec-path-from-shell
;; Ensure Emacs inherits specified variables from the user environment
(setup exec-path-from-shell
  (:package exec-path-from-shell)
  (:require)

  (setopt exec-path-from-shell-variables
          '("PATH" "MANPATH" "BROWSER"
            ;; `ssh-agent' environment variables.  See
            ;; https://wiki.archlinux.org/title/SSH_keys#Start_ssh-agent_with_systemd_user
            "SSH_AGENT_PID" "SSH_AUTH_SOCK"))

  (exec-path-from-shell-initialize))

;;; Bespoke editing commands
;; Joining and inserting newlines
(defun krisb-open-line-above-goto ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode.
Credit to https://emacsredux.com/blog/2013/06/15/open-line-above/"
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun krisb-open-line-below-goto ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
Credit to https://emacsredux.com/blog/2013/03/26/smarter-open-line/"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun krisb-join-line-above ()
  "Join the current line with the line above."
  (interactive)
  (save-excursion (delete-indentation))
  (when (string-match-p "\\`\\s-*$" (thing-at-point 'line))
    (funcall indent-line-function)))

(defun krisb-join-line-below ()
  "Join the current line with the line below."
  (interactive)
  (save-excursion (delete-indentation t))
  (when (bolp)
    (funcall indent-line-function)))

(bind-keys
 ("C-S-p" . krisb-open-line-above-goto)
 ("C-S-n" . krisb-open-line-below-goto)
 ("C-S-k" . krisb-join-line-above)
 ("C-S-j" . krisb-join-line-below))

;;; Repeat.el
(setup repeat
  (add-hook 'on-first-input-hook #'repeat-mode))

;;; Savehist
;; Make history of certain things (e.g. minibuffer) persistent across sessions
(setup savehist
  (:require)

  (setopt savehist-save-minibuffer-history t
          savehist-autosave-interval 30)

  (savehist-mode 1)

  (dolist (var '((Info-history-list . 250)))
    (add-to-list 'savehist-additional-variables var)))

;;; Saveplace
;; Save and restore the point's location in files
(setup saveplace

  (setopt save-place-forget-unreadable-files t
          save-place-limit 3000)

  (add-hook 'on-first-file-hook #'save-place-mode))

;;; Auto-save
;; TODO 2025-05-23: Document:
;; - `auto-save-no-message'
;; - `auto-save-include-big-deletions’
;; - `delete-auto-save-files’ and
;;   `kill-buffer-delete-auto-save-files’
;; - `remote-file-name-inhibit-auto-save-visited’
;; TODO 2025-05-23: Note that auto-save is distinct from
;; `auto-save-visited-mode’
(setup files

  (setopt auto-save-default t ; Only a local minor mode exists; this variable influences the global value
          auto-save-timeout 5
          auto-save-interval 150)

  ;; Modified from Doom Emacs.  Auto save files have names that are
  ;; hashed.
  (defun krisb-auto-save-hash-file-name (&rest args)
    "Turn `buffer-file-name' into a hash.
Then apply ARGS."
    (let ((buffer-file-name
           (if (or
                ;; Don't do anything for non-file-visiting
                ;; buffers. Names generated for those are short enough
                ;; already.
                (null buffer-file-name)
                ;; If an alternate handler exists for this path, bow
                ;; out. Most of them end up calling
                ;; `make-auto-save-file-name' again anyway, so we
                ;; still achieve this advice's ultimate goal.
                (find-file-name-handler buffer-file-name
                                        'make-auto-save-file-name))
               buffer-file-name
             (sha1 buffer-file-name))))
      (apply args)))
  (advice-add 'make-auto-save-file-name :around #'krisb-auto-save-hash-file-name))

;;; Auto-save-visited
(setup files

  (setopt auto-save-visited-interval 8
          auto-save-visited-predicate ; Value Inspired by `super-save'
          (lambda ()
            (or
             ;; Don’t auto save buffers that are too long, since that
             ;; may lead to noticeable pauses
             (< (save-restriction (widen) (count-lines (point-min) (point-max)))
                500000)
             ;; Don’t auto-save `pdf-view-mode’ buffers
             (derived-mode-p 'pdf-view-mode))))

  (add-hook 'on-first-file-hook #'auto-save-visited-mode))

;;; Recentf
;; Track recently opened files
(setup recentf

  (bind-keys :map ctl-x-map
             ("M-f" . recentf-open))

  (setopt recentf-auto-cleanup 600
          recentf-max-saved-items 1000
          recentf-max-menu-items 15)
  (setopt recentf-show-messages nil)

  (add-hook 'on-first-file-hook #'recentf-mode))

;;; Help.el
;; TODO 2025-10-14: Document:
;; `help-at-pt-display-when-idle'
(setup help

  (add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function)

  (:bind-keys :map help-map
              ("C-k" . describe-keymap))

  ;; Displaying buffer
  (setopt help-window-select t
          help-window-keep-selected t)

  ;; Help buffers
  (setopt help-enable-variable-value-editing t
          help-clean-buttons t
          help-enable-symbol-autoload t

          describe-bindings-outline t
          describe-bindings-show-prefix-commands t)

  (with-eval-after-load 'help
    (add-to-list 'display-buffer-alist
                 '((major-mode . help-mode)
                   (display-buffer-reuse-window display-buffer-pop-up-window display-buffer-below-selected)
                   (window-height . shrink-window-if-larger-than-buffer)))))

;;; Apropos
(setup apropos
  
  (bind-keys ("C-h u" . apropos-user-option)))

;;; Messages buffer
;; Visual-line-mode in *Messages* buffer
(add-hook 'messages-buffer-mode-hook #'visual-line-mode)

;; Mesages buffer positioning
(add-to-list 'display-buffer-alist
             `(,(rx (literal messages-buffer-name))
               (display-buffer-in-side-window)
               (window-height . 0.36)
               (side . top)
               (slot . 1)
               (post-command-select-window . t)))

;;; Elisp-demos
;; Add example code snippets to some of the help windows
(setup elisp-demos
  (:package elisp-demos)

  (add-hook 'help-fns-describe-function-functions #'elisp-demos-advice-describe-function-1)
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;; Window.el
;; TODO 2025-07-21: Document:
;; - `quit-window-kill-buffer'
(setup window

  (:bind-keys ("M-o" . other-window))

  (setopt switch-to-buffer-obey-display-actions t
          window-resize-pixelwise t
          quit-restore-window-no-switch t ; Emacs 31
          ;; TODO 2025-10-17: Revisit this
          ;; kill-buffer-quit-windows t    ; Emacs 31
          ))

;;; Corfu
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;; - `corfu-auto'
;; - `corfu-cycle'
;; Faster, minimal, and more lightweight autocomplete that is more
;; faithful to the Emacs infrastructure
(setup corfu
  (:package corfu)
  (:require)

  (:bind-keys
   ;; TODO 2025-05-20: Revisit this.
   ;; ("M-i" . completion-at-point) ; For harmony with "M-i" in `completion-preview-active-mode-map'
   :map corfu-map
   ("M-d" . corfu-info-documentation))

  (setopt corfu-count 14
          corfu-scroll-margin 3
          ;; Always have the same width
          corfu-min-width 75
          corfu-max-width corfu-min-width)

  ;; Allow spaces and don't quit on boundary to leverage orderless's
  ;; space-separated components
  (with-eval-after-load 'orderless
    (setopt corfu-quit-at-boundary nil
            corfu-separator ?\s         ; Use space
            corfu-quit-no-match 'separator)) ; Don't quit if there is `corfu-separator' inserted

  ;; Always use a fixed-pitched font for corfu; variable pitch fonts
  ;; (which will be adopted in a variable pitch buffer) have
  ;; inconsistent spacing
  (:face corfu-default ((t (:inherit 'default))))

  (global-corfu-mode 1))

;; Extras
(setup corfu
  ;; Enable corfu in minibuffer if `vertico-mode' is disabled.  From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun krisb-corfu-enable-in-minibuffer-conditionally ()
    "Enable Corfu in the minibuffer if vertico is not active."
    (when (and global-corfu-mode (not (bound-and-true-p vertico-mode)))
      (setq-local corfu-auto nil) ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'krisb-corfu-enable-in-minibuffer-conditionally)

  (with-eval-after-load 'consult
    ;; Transfer completion of corfu to the minibuffer.  Taken from
    ;; https://github.com/minad/corfu?tab=readme-ov-file#transfer-completion-to-the-minibuffer.
    (defun krisb-corfu-move-to-minibuffer ()
      "Transfer corfu completion to the minibuffer."
      (interactive)
      (pcase completion-in-region--data
        (`(,beg ,end ,table ,pred ,extras)
         (let ((completion-extra-properties extras)
               completion-cycle-threshold completion-cycling)
           (consult-completion-in-region beg end table pred)))))
    (:bind-keys :map corfu-map ("M-m" . krisb-corfu-move-to-minibuffer))
    (add-to-list 'corfu-continue-commands #'krisb-corfu-move-to-minibuffer)))

;; Extension that comes with corfu.  Popup documentation window for
;; corfu candidates
(setup corfu-popupinfo
  (:load-after corfu)

  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)

  (:bind-keys :map corfu-map
              ([remap corfu-info-documentation] . corfu-popupinfo-toggle)
              ("M-l" . corfu-popupinfo-location))

  (setopt corfu-popupinfo-delay '(nil . 0.4)  ; Don't display initially
          corfu-popupinfo-direction '(right left vertical)
          corfu-popupinfo-hide t
          corfu-popupinfo-resize t
          corfu-popupinfo-max-height 70
          corfu-popupinfo-max-width 80
          corfu-popupinfo-min-height 1
          corfu-popupinfo-min-width 25))

;;; Electric
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;; - `electric-quote-comment'
;; - `electric-quote-string'
;; - `electric-quote-inhibit-functions'
;; - `electric-pair-delete-adjacent-pairs'
;; NOTE: 2025-05-22: For some reason lisp-mode sets these buffer
;; locally.  See `lisp-mode-variables'.
;; - `electric-pair-skip-whitespace'
;; - `electric-pair-open-newline-between-pairs'
(setup electric

  (setopt electric-quote-context-sensitive t
          electric-quote-replace-double t)

  (electric-pair-mode 1)
  (electric-indent-mode 1))

;;; Autorevert
;; Automatically update buffers as files are externally modified
;; TODO 2025-05-22: Document:
;; - `auto-revert-verbose’
;; - `auto-revert-avoid-polling'
;; - `auto-revert-check-vc-info'
(setup autorevert

  (:hide-mode auto-revert-mode)

  (setopt auto-revert-interval 3))

;;; Newcomment
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;; - `comment-fill-column'
;; - `comment-multi-line'
;; - `comment-style'
(setup newcomment

  (setopt comment-empty-lines t))

;;; Vc.el
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;; - `vc-annotate-display-mode'
;; - `vc-revert-show-diff'
(setup vc

  (setopt vc-follow-symlinks t
          vc-allow-rewriting-published-history 'ask ; Emacs 31
          vc-async-checkin t
          vc-allow-async-diff t         ; Emacs 31
          vc-revert-show-diff t
          vc-find-revision-no-save t           ; Emacs 31
          vc-dir-hide-up-to-date-on-revert t   ; Emacs 31
          vc-dir-save-some-buffers-on-revert t ; Emacs 31
          vc-use-incoming-outgoing-prefixes t) ; Emacs 31

  (if (fboundp 'vc-auto-revert-mode)
      (vc-auto-revert-mode 1)           ; Emacs 31
    (global-auto-revert-mode 1))

  (add-to-list 'display-buffer-alist
               '((or . ((major-mode . vc-dir-mode)
                        (major-mode . vc-git-log-view-mode)
                        (major-mode . vc-git-region-history-mode)))
                 (display-buffer-same-window))))

;; Dispatcher between `vc-diff’ and `diff-buffer-with-file’
(setup vc
  (defun krisb-vc-diff-dwim ()
    "Call `vc-diff’ or `diff-buffer-with-file’.
Calls `vc-diff’ if the buffer is unmodified.  If buffer is modified,
call `diff-buffer-with-file’ instead."
    (interactive)
    (if (and (not (eq major-mode 'vc-dir-mode)) (buffer-modified-p))
        (diff-buffer-with-file (current-buffer))
      (vc-diff)))
  (:bind-keys ([remap vc-diff] . krisb-vc-diff-dwim)))

;;;; Vc-git
;; TODO 2025-07-10: Document:
;; - `vc-git-revision-complete-only-branches'
(setup vc-git

  (setopt vc-git-log-edit-summary-target-len (+ 50 (length "Summary"))
          vc-git-log-edit-summary-max-len (+ 70 (length "Summary"))
          vc-git-diff-switches '("--patch-with-stat" "--histogram")
          vc-git-root-log-format
          `("%h %ad (%ar) %aN%d%n  %s"
            ;; The first shy group matches the characters drawn by
            ;; --graph. We use numbered groups because
            ;; `log-view-message-re' wants the revision number to be
            ;; group 1.
            ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                     "\\(?1:[0-9a-z]+\\)"      ; %h
                     " "
                     "\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} (.*? ago)\\)?" ; %ad (%ar)
                     " "
                     "\\(?3:\\(?:[[:alpha:]]+\\.?[\n ]\\)+\\)" ; %aN
                     "\\(?2:([^)]+)\\)?")                      ; %d
            ((1 'log-view-message)
             (2 'change-log-list nil lax)
             (3 'change-log-name)
             (4 'change-log-date)))))

;;;; Vc-jj
;; Integration between vc.el and the jujutsu (JJ) version control
;; system.  Best jj integration with vc currently (2025-10-17).
(setup vc-jj
  ;; Elpaca: (:repo "https://codeberg.org/krisbalintona/vc-jj.el.git" :branch "merge")
  (:package (vc-jj :url "https://codeberg.org/krisbalintona/vc-jj.el.git"
                   :branch "merge"))
  ;; Require vc-jj and project-jj alongside vc and project since some
  ;; operation results are cached by them, and we don't want them to
  ;; cache information that disagrees with vc-jj
  (:load-after vc)
  (:load-after project)
  (require 'project-jj)

  (setopt vc-jj-diff-switches '("--git" "--stat")))

;;; Log-edit
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;; - `log-edit-headers-alist'
;; - `log-edit-setup-add-author'
(setup log-edit

  ;; Evaluate after log-edit defines `log-edit-hook' first, since the
  ;; hook already has several functions.  (Otherwise, we are adding to
  ;; an empty hook.)
  (with-eval-after-load 'log-edit
    (add-hook 'log-edit-hook #'auto-fill-mode)
    (add-hook 'log-edit-hook #'log-edit-maybe-show-diff))

  (:face log-edit-summary ((t (:family ,(face-attribute 'variable-pitch :family))))))

;;; Outline.el
(setup outline

  (:hide-mode outline-minor-mode)

  (setopt outline-minor-mode-cycle t
          outline-minor-mode-cycle-filter nil
          outline-minor-mode-highlight 'append
          outline-blank-line t))

;;; Outli.el
;; TODO 2025-05-20: Document that I prefer this over the heavier,
;; less-compatible outshine.el as well as outline-indent.
;; Coding language-agnostic file outlines.  Lightweight and close to
;; the built-in outline.el.
(setup outli
  ;; Elpaca: :ensure (:repo "https://github.com/jdtsmith/outli")
  (:package outli)

  (with-eval-after-load 'outli
    (require 'outline)
    (bind-keys :map outline-minor-mode-map
               ;; 2025-04-02: Assumes `outline-minor-mode-prefix' is
               ;; "C-c @"
               ("C-c @ C-<return>" . outli-insert-heading-respect-content)
               ("C-c @ ?" . outli-speed-command-help)
               ("C-c @ s" . outli-toggle-narrow-to-subtree)))

  (setopt outli-allow-indented-headlines t
          outli-default-nobar nil       ; Show a horizontal rule?
          outli-blend nil)

  (add-hook 'prog-mode-hook #'outli-mode)
  (add-hook 'text-mode-hook #'outli-mode))

;; Add "Heading" (which outli headings are categorized as) imenu
;; group.  Taken from
;; https://github.com/jdtsmith/outli?tab=readme-ov-file#faq
(setup consult-imenu
  (with-eval-after-load 'consult-imenu
    (push '(?h "Headings")
          (plist-get (cdr (assoc 'emacs-lisp-mode consult-imenu-config)) :types))))

;;; Imenu
(setup imenu

  (setopt imenu-auto-rescan t
          imenu-flatten 'group))

(setup pulsar
  (with-eval-after-load 'pulsar
    (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)))

(setup org
  (setopt org-imenu-depth 7))      ; Show more than just 2 levels...

;;; Isearch
;; TODO 2025-11-15: Document:
;; - `lazy-highlight-initial-delay'
;; - `lazy-highlight-interval'
;; Incremental search
(setup isearch

  (with-eval-after-load 'isearch
    (setopt isearch-repeat-on-direction-change t
            isearch-allow-scroll 'unlimited
            isearch-allow-motion t
            isearch-wrap-pause 'no-ding)

    (setopt isearch-lazy-count t
            lazy-highlight-no-delay-length 5))
  
  ;; Treatment of whitespace.  Also be aware of the
  ;; `isearch-toggle-lax-whitespace' command during isearch searches
  (setopt isearch-lax-whitespace t
          isearch-regexp-lax-whitespace nil
          ;; Make a whitespace greedily matches everything, meaning
          ;; the search string "foo bar" matches "foo" and "bar" with
          ;; any number of anything in between
          search-whitespace-regexp ".*?"))

;;; Pulsar
;; Alternative to `pulse.el'
(setup pulsar
  (:package pulsar)

  (pulsar-global-mode 1)

  (setopt pulsar-pulse t
          pulsar-face 'pulsar-red
          pulsar-delay 0.05
          pulsar-iterations 5))

;;; Org
;;;; Org built-ins
;;;;; Org-mode
(setup org
  (:pin org-mode "gnu-elpa-devel")
  (:package org)

  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'variable-pitch-mode)
    (add-hook 'org-mode-hook #'visual-line-mode)
    (add-hook 'org-mode-hook (lambda ()
                               (setq-local line-spacing 0.2
                                           fill-column 100))))

  (setopt org-directory krisb-folio-directory)

  (with-eval-after-load 'org
    ;; TODO 2025-10-15: Document these:
    ;; - `org-use-fast-todo-selection'
    ;; Todos
    (setopt org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n)" "HOLD(h@/!)" "MAYBE(m)" "|"
                        "DONE(d!/@)" "CANCELED(c@/!)"))
            org-todo-keyword-faces
            '(("NEXT" . (bold success))
              ("TODO" . org-todo)
              ("HOLD" . (shadow error))
              ("MAYBE" . (shadow org-todo))
              ("DONE" . (bold org-done))
              ("CANCELED" . error))
            org-enforce-todo-dependencies t
            org-agenda-dim-blocked-tasks t)
    
    ;; TODO 2025-05-22: Document:
    ;; - `org-special-ctrl-k'
    ;; Command special behaviors
    (setopt org-blank-before-new-entry
            '((heading . auto)
              (plain-list-item . nil))
            org-M-RET-may-split-line
            '((table . nil)
              (default . t))
            org-special-ctrl-a/e t
            org-ctrl-k-protect-subtree 'error)

    ;; TODO 2025-05-22: Document:
    ;; - `org-hide-leading-stars'
    ;; - `org-n-level-faces'
    ;; - `org-cycle-separator-lines'
    ;; - `org-cycle-level-faces'
    ;; - `org-insert-heading-respect-content'
    ;; - `org-fontify-todo-headline’
    ;; - `org-ellipsis'
    ;; Headlines
    (setopt org-fontify-done-headline nil)

    ;; Priorities
    (setopt org-priority-highest ?A
            org-priority-default ?E
            org-priority-lowest ?F
            org-priority-faces
            '((?A . (bold org-priority))
              (?B . (bold org-priority))
              (?C . org-priority)
              (?D . org-priority)
              (?E . (shadow org-priority))
              (?F . (shadow org-priority))))
    
    ;; Org blocks
    (setopt org-structure-template-alist
            '(("s" . "src")
              ("S" . "src emacs-lisp")
              ("q" . "quote")
              ("c" . "comment")
              ("C" . "center")
              ("e" . "export")
              ("E" . "example")
              ("v" . "verse"))
            org-fontify-quote-and-verse-blocks t
            org-fontify-whole-block-delimiter-line nil)
    
    ;; TODO 2025-05-22: Document:
    ;; - `org-hide-macro-markers'
    ;; - `org-pretty-entities-include-sub-superscripts' - see also `org-export-with-sub-superscripts'
    ;; - `org-hidden-keywords'
    ;; Fancy markup
    (setopt org-hide-emphasis-markers t
            org-pretty-entities t ; Show as UTF-8 characters (useful for math)
            org-use-sub-superscripts '{}) ; Requires brackets to recognize superscripts and subscripts
    
    ;; Logging
    (setopt org-log-done 'time
            org-log-into-drawer t
            org-log-refile 'time
            org-log-reschedule 'time
            org-log-redeadline 'time)

    ;; Time and timestamps
    (setopt org-edit-timestamp-down-means-later t
            org-extend-today-until 4
            org-use-effective-time t)

    ;; Tags
    (setopt org-tags-column 0
            org-tags-exclude-from-inheritance
            '("PROJECT" "__journal" "__log" "__top_of_mind"))

    ;; Properties
    (setopt org-use-property-inheritance '("CATEGORY" "ARCHIVE"))

    ;; TODO 2025-05-22: Document the "Org Plain List" customize group
    ;; as well as these options:
    ;; - `org-list-use-circular-motion'
    ;; Plain lists
    (setopt org-list-allow-alphabetical t
            org-list-demote-modify-bullet
            '(("+" . "-")
              ("-" . "*")
              ("*" . "+")))))

;;;;; Org-agenda
;; TODO 2025-05-24: Document these options:
;; - `org-agenda-start-on-weekday’
;; - `org-enforce-todo-checkbox-dependencies’
;; - `org-agenda-show-inherited-tags’
;; - `org-use-tag-inheritance’
;; TODO 2025-05-24: Also mention these options that are elevant to org-agenda:
;; - `org-extend-today-until’
;; - `org-use-effective-time’
;; - `org-use-property-inheritance’
;; TODO 2025-05-24: Explain that `org-todo-keywords’ does not work
;; when set directory locally.  Some relevant mailing list
;; discussions:
;; - https://lists.gnu.org/archive/html/emacs-orgmode/2020-05/msg00426.html,
;; - https://lists.gnu.org/archive/html/emacs-orgmode/2022-10/msg01174.html.
;; We either must set it via file-local keywords or using #+SETUPFILE.
(setup org-agenda
  (:if-package org)

  (bind-keys :map krisb-open-keymap
             ("a" . org-agenda))

  (with-eval-after-load 'org-agenda
    (add-hook 'org-agenda-mode-hook #'hl-line-mode))

  (setopt org-agenda-inhibit-startup t
          org-agenda-files (list krisb-org-agenda-directory))

  (with-eval-after-load 'org-agenda
    ;; Agenda buffer/window
    (setopt org-agenda-window-setup 'only-window
            org-agenda-restore-windows-after-quit t
            org-agenda-sticky t)
    
    ;; Effort
    (setopt org-agenda-sort-noeffort-is-high nil))

  (with-eval-after-load 'org-agenda
    (add-to-list 'display-buffer-alist
                 '("\\*\\(?:Org Select\\|Agenda Commands\\)\\*"
                   (display-buffer-in-side-window)
                   (window-height . fit-window-to-buffer)
                   (side . top)
                   (slot . -2)
                   (preserve-size . (nil . t))
                   (window-parameters . ((mode-line-format . none)))
                   (post-command-select-window . t)))))

(with-eval-after-load 'org-agenda
  ;; `org-agenda-skip-function' functions constructed by
  ;; `krisb-org-agenda-skip-org-ql', which uses org-ql.  These
  ;; functions, effectively, let me query org todos instead of with the
  ;; typical syntax.
  (defun krisb-org-agenda-skip-demand ()
    "Filter tasks for demand agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (not (deadline))
           (not (scheduled))
           (not (tags-local "PROJECT" "INBOX"))
           (or (priority "A")))))
  
  (defun krisb-org-agenda-skip-focus ()
    "Filter tasks for focus agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (not (tags-local "PROJECT" "INBOX"))
           (or (scheduled :to today)
               (deadline :to today)))))
  
  (defun krisb-org-agenda-skip-routine ()
    "Filter tasks for routine agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (not (tags-local "PROJECT" "INBOX"))
           (or (and (or (habit)
                        (path "recurring\\.org"))
                    (ts-active :to today))))))
  
  (defun krisb-org-agenda-skip-radar ()
    "Filter tasks for radar agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (not (tags-local "PROJECT" "INBOX"))
           (or (and (todo "NEXT")
                    (not (or (scheduled)
                             (deadline))))))))
  
  (defun krisb-org-agenda-skip-review ()
    "Filter tasks for review agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (or (todo "MAYBE")
               (tags-local "REVIEW")))))
  
  (defun krisb-org-agenda-skip-inbox ()
    "Filter tasks for inbox agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (tags-local "INBOX"))))
  
  (setopt org-agenda-custom-commands
          '(("n" "Agenda and all TODOs"
             ((agenda "")
              (alltodo "")))
            ("d" "Demand"
             ((alltodo ""
                       ((org-agenda-overriding-header "Demand")
                        (org-agenda-skip-function 'krisb-org-agenda-skip-demand)))))
            ("f" "Focus"
             ((alltodo ""
                      ((org-agenda-overriding-header "Focus")
                       (org-agenda-skip-function 'krisb-org-agenda-skip-focus)))))
            ("r" "Radar"
             ((alltodo ""
                       ((org-agenda-overriding-header "Radar")
                        (org-agenda-skip-function 'krisb-org-agenda-skip-radar)))
              (alltodo ""
                       ((org-agenda-overriding-header "Routine")
                        (org-agenda-skip-function 'krisb-org-agenda-skip-routine)))))
            ("R" "Review"
             ((alltodo ""
                       ((org-agenda-overriding-header "Review")
                        (org-agenda-skip-function 'krisb-org-agenda-skip-review)))
              (alltodo ""
                       ((org-agenda-overriding-header "Inbox")
                        (org-agenda-skip-function 'krisb-org-agenda-skip-inbox))))))))

;;;;; Org-clock
;; TODO 2025-05-24: Document:
;; - `org-clock-out-when-done’
(setup org-clock
  (:if-package org)
  
  (setopt org-clock-in-resume t
          org-clock-out-remove-zero-time-clocks t))

;;;;; Org-capture
(setup org-capture
  (:if-package org)
  
  (bind-keys ("C-c c" . org-capture))

  (setopt org-capture-use-agenda-date t)

  ;; Helperes for `org-capture-templates'
  (defun krisb-org-capture--org-node-by-tags (tags)
    "Interactively prompt for an org-node candidate matching TAGS.
TAGS is a list of regexps that match org-node tags.

This function will use `completing-read' whose candidates are the
org-node nodes that match all of TAGS.  It will return a candidate (see
`org-node--candidate<>entry')."
    (require 'org-node)
    (gethash (completing-read "Select node: "
                              #'org-node-collection-basic
                              (lambda (_title node)
                                (cl-every (lambda (re)
                                            (cl-some (lambda (str)
                                                       (string-match-p re str))
                                                     (org-node-get-tags node)))
                                          tags))
                              t nil 'org-node-hist)
             org-node--candidate<>entry))

  ;; See also `org-capture-templates-contexts'
  (setopt org-capture-templates
          '(("t" "Todo" entry
             (file krisb-org-agenda-main-file)
             "* TODO %? :inbox:%^g\n"
             :empty-lines 1)
            ("T" "Todo (without processing)" entry
             (file krisb-org-agenda-main-file)
             "* TODO %? %^g\n"
             :empty-lines 1)
            ("j" "Journal" entry
             (file+olp+datetree
              (lambda ()
                (let* ((node (krisb-org-capture--org-node-by-tags `(,(rx bol (or "__journal" "__top_of_mind") eol)))))
                  (org-capture-put :krisb-node node)
                  (org-node-get-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-node-is-subtree node)
                    (org-node-get-olp-with-self node)))))
             "* %<%c>\n"
             :tree-type (year quarter month)
             :jump-to-captured t
             :immediate-finish t
             :empty-lines 1
             :hook org-expiry-insert-created
             :clock-in t
             :clock-resume t)
            ("w" "Just write" entry
             (file+olp+datetree
              (lambda ()
                (let* ((node (org-mem-entry-by-id "20241006T214800.000000")))
                  (org-capture-put :krisb-node node)
                  (org-node-get-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-node-is-subtree node)
                    (org-node-get-olp-with-self node)))))
             "* %<%c>\n\n*P:* %(car (krisb-oblique-strategies--random))\n\n"
             :tree-type (year quarter month)
             :jump-to-captured t
             :immediate-finish t
             :empty-lines 1
             :clock-in t
             :clock-resume t)
            ("l" "Log" item
             (file+olp+datetree
              (lambda ()
                (let* ((node (krisb-org-capture--org-node-by-tags '("^__log$"))))
                  (org-capture-put :krisb-node node)
                  (org-node-get-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-node-is-subtree node)
                    (org-node-get-olp-with-self node)))))
             "%U %?"
             :tree-type (quarter week)
             :clock-in t
             :clock-resume t)
            ("m" "Work meeting notes" entry
             (file+olp+datetree
              (lambda ()
                (let* ((node (org-mem-entry-by-id "20241114T091749.707997")))
                  (org-capture-put :krisb-node node)
                  (org-node-get-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-node-is-subtree node)
                    (org-node-get-olp-with-self node)))))
             "* (%<%c>)%?\n\n"
             :tree-type (year quarter month)
             :jump-to-captured t
             :immediate-finish t)
            ("r" "New reference" entry
             (file+olp+datetree
              (lambda ()
                (let* ((node (krisb-org-capture--org-node-by-tags '("^__references$"))))
                  (org-capture-put :krisb-node node)
                  (org-node-get-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-node-is-subtree node)
                    (org-node-get-olp-with-self node)))))
             "* %?\n"
             :tree-type (year month)
             :jump-to-captured t
             :immediate-finish t
             :empty-lines 1
             :hook (org-id-get-create org-expiry-insert-created)
             :before-finalize (org-node-add-refs
                               (lambda () (org-set-property "ROAM_BOX" "references"))))
            ("b" "Blog post" plain
             (function (lambda ()
                         (let ((org-node-ask-directory krisb-blog-directory))
                           (org-node-capture-target))))
             "#+filetags: :__blog_draft:
#+hugo_bundle:
#+export_file_name: index
#+hugo_tags:
#+hugo_categories:
#+hugo_publishdate:
#+hugo_lastmod:
#+hugo_custom_front_matter: :TableOfContents true
#+hugo_draft: true
#+hugo_paired_shortcodes:\n\n%?"
             :jump-to-captured t
             :immediate-finish t)
            ("g" "Game review" entry
             (file+olp+datetree
              (lambda ()
                (let* ((candidate-ids
                        (list "20250809T050805.074803"   ; Mid
                              "20250809T050803.643974")) ; ADC
                       (node
                        (gethash
                         (completing-read "Select node: "
                                          #'org-node-collection-basic
                                          (lambda (_title node)
                                            (member (org-mem-id node) candidate-ids))
                                          t nil 'org-node-hist)
                         org-node--candidate<>entry)))
                  (org-capture-put :krisb-node node)
                  (org-node-get-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-node-is-subtree node)
                    (org-node-get-olp-with-self node)))))
             "* %?
** Successes
** Mistakes"
             :tree-type (month day)
             :empty-lines 1))))

;;;;; Org-id
(setup org-id
  (:if-package org)

  (setopt org-id-track-globally t
          org-id-method 'ts
          org-id-link-to-org-use-id 'use-existing
          org-id-link-consider-parent-id t))

;; Standardize creation of IDs and targets with bespoke commands
(setup org-id
  (with-eval-after-load 'org
    (defun krisb-org-create-custom-id ()
      "Get the CUSTOM_ID of the current entry.
If the entry already has a CUSTOM_ID, return it as-is, else create a new
one.

This function is a copy of `denote-link-ol-get-id'."
      (interactive nil org-mode)
      (let* ((pos (point))
             (id (org-entry-get pos "CUSTOM_ID")))
        (if (and (stringp id) (string-match-p "\\S-" id))
            id
          (setq id (org-id-new "h"))
          (org-entry-put pos "CUSTOM_ID" id)
          id)))

    (defun krisb-org-create-dedicated-target ()
      "Return a unique dedicated target as a string.
Based on the current time.  See (info \"(org) Internal Links\") for more
information on dedicated targets.

If called interactively, then insert the target into the buffer.
Otherwise, just return the target as a string.

In either case, also store the target as an org link that can be
inserted with e.g. `org-insert-last-stored-link' or
`org-insert-all-links'."
      (interactive)
      (let* ((id (format-time-string "%Y%m%dT%H%M%S"))
             (target (concat "")))
        (if (called-interactively-p 'interactive)
            (insert target)
          target)
        (let ((org-link-context-for-files t))
          (org-link--add-to-stored-links (org-store-link '(16)) id))
        target))))

;;;;; Org-footnote
;; TODO 2025-05-23: Document:
;; - `org-footnote-define-inline’
(setup org-footnote

  (setopt org-footnote-section nil    ; Don't create footnote headline
          org-footnote-auto-adjust t))

;;;;; Org-src
;; TODO 2025-05-23: Document:
;; - `org-edit-src-turn-on-auto-save'
;; - `org-edit-src-auto-save-idle-delay’
;; - `org-src-block-faces’
(setup org-src
  (:if-package org)

  (setopt org-src-fontify-natively t
          org-src-window-setup 'current-window))

;;;;; Org-cite
;; Built-in citations in org-mode
(setup oc
  (:if-package org)

  (setopt org-cite-global-bibliography krisb-bibliography-files
          org-cite-csl-locales-dir nil
          ;; TODO 2025-10-19: Avoid hardcoding this path?
          org-cite-csl-styles-dir (expand-file-name "~/Zotero/styles/"))

  (setopt org-cite-export-processors
          '((md . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
            (latex biblatex)            ; For humanities
            (odt . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
            (docx . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
            (t . (csl "modern-language-association.csl")))) ; Fallback

  ;; Have citation link faces look closer to as they were for
  ;; `org-ref'
  (:face org-cite ((t (:foreground "DarkSeaGreen4"))))
  (:face org-cite-key ((t (:foreground "forest green" :slant italic))))

  ;; 2025-03-30: For the biblatex cite export processor.  Otherwise,
  ;; `org-cite-supported-styles' errors because
  ;; (org-cite-get-processor 'biblatex) returns nil.
  (with-eval-after-load 'oc
    (require 'oc-biblatex)))

;;;; Other org packages
;;;;; Org-contrib
;; Collection of org packages
(setup org-contrib
  (:package org-contrib))

;;;;; Org-modern
(setup org-modern
  (:package org-modern)

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  ;; Keywords
  (setopt org-modern-keyword nil)

  ;; TODO 2025-05-23: Also document:
  ;; - `org-modern-fold-stars’
  ;; Headlines
  (setopt org-modern-hide-stars "· " ; Is affected by the value of `org-hide-leading-stars'
          org-modern-star 'replace
          org-modern-replace-stars "✦⦾‣⬢")

  ;; Todos
  (setopt org-modern-todo t
          ;; See my value for `org-todo-keyword-faces'
          org-modern-todo-faces
          '(("NEXT" :inherit (bold success org-modern-todo))
            ("TODO" :inherit (org-todo org-modern-todo))
            ("HOLD" :inherit (shadow error org-modern-todo))
            ("MAYBE" :inherit (shadow org-todo org-modern-todo))
            ("DONE" :inherit (bold org-done org-modern-todo))
            ("CANCELED" :inherit (error org-modern-todo))))

  ;; Priorities
  (setopt org-modern-priority t
          ;; See my value for `org-priority-faces'
          org-modern-priority-faces
          '((?A :inverse-video t :inherit (bold org-priority))
            (?B :inverse-video t :inherit (bold org-priority))
            (?C :inverse-video t :inherit org-priority)
            (?D :inverse-video t :inherit org-priority)
            (?E :inverse-video t :inherit (shadow org-priority))
            (?F :inverse-video t :inherit (shadow org-priority))))

  ;; Tags
  (setopt org-modern-label-border 3
          org-modern-tag t)

  ;; Org blocks
  (setopt org-modern-block-fringe nil ; Doesn't work well with `olivetti-style' set to 'fancy
          org-modern-block-name '("⌜" . "⌞"))

  ;; Footnotes
  (setopt org-modern-footnote '(nil (raise 0.15) (height 0.9)))

  ;; Lists
  (setopt org-modern-list '((?+ . "◦")
                            (?- . "–")
                            (?* . "•")))

  ;; Timestamps
  (setopt org-modern-timestamp t)

  ;; Tables
  (setopt org-modern-table t
          org-modern-table-vertical 3
          org-modern-table-horizontal 0.1)

  ;; TODO 2025-05-23: Revisit this.
  ;; (krisb-modus-themes-setup-faces
  ;;  "org-modern"
  ;;  (setopt org-modern-tag-faces
  ;;          `(("project"
  ;;             :foreground ,(face-background 'default nil t)
  ;;             :background ,(face-foreground 'modus-themes-fg-magenta-cooler nil t)))))
  )

;;;;; Org-mem
;; FIXME 2025-10-17: Document also:
;; - `org-mem-db1-mode'
(setup org-mem
  (:package org-mem)
  (:if-package org)

  (setopt org-mem-do-sync-with-org-id t
          org-mem-watch-dirs (list krisb-folio-directory)
          org-mem-do-warn-title-collisions nil)
  (with-eval-after-load 'org-mem
    (cl-pushnew
     (file-name-as-directory (file-relative-name krisb-org-archive-directory krisb-folio-directory))
     org-mem-exclude))
  (org-mem-updater-mode 1)

  ;; FIXME 2025-05-27: Figure out which org-node update is breaking
  ;; org-roam database compatibility.
  (:with-local-quit
   (:quit)
   ;; Load things related to org-mem’s interaction with org-roam
   (with-eval-after-load 'org-roam
     ;; End dependence on `org-roam-db-sync'
     (setopt org-roam-db-update-on-save nil
             org-mem-roamy-do-overwrite-real-db t)
     (org-roam-db-autosync-mode -1)
     (org-mem-roamy-db-mode 1))))

;;;;; Org-node
(setup org-node
  (:package org-node)
  (:if-package org)

  (:bind-keys :map krisb-note-keymap
              ("l" . org-node-context-toggle)
              ([remap org-roam-buffer-toggle] . org-node-context-toggle)
              ("f" . org-node-find)
              ("i" . org-node-insert-link)
              ("t" . org-node-set-tags-here))

  (setopt org-node-file-directory-ask t
          org-node-file-timestamp-format "%Y%m%dT%H%M%S--"
          org-node-renames-allowed-dirs (list krisb-notes-directory))
  (setopt org-node-alter-candidates t
          org-node-blank-input-hint nil)
  (setopt org-node-do-filter-tags t
          org-node-warn-title-collisions nil)

  (org-node-cache-mode 1)

  (setopt org-node-context-persist-on-disk t)
  (org-node-context-follow-mode 1)

  ;; Bespoke filtering (exclusion) function.
  (defun krisb-org-node-filter-fn (node)
    "Predicate for whether to include NODE.
If non-nil, include.  If nil, exclude.  This predicate excludes these
nodes:
- With non-nil ROAM_EXCLUDE property value."
    (let ((exclude-val (cdr (assoc "ROAM_EXCLUDE" (org-node-get-properties node)))))
      (not (or (when exclude-val (string= "t" (string-trim exclude-val)))))))
  (setopt org-node-filter-fn #'krisb-org-node-filter-fn)

  ;; Bespoke `org-node-custom-link-format-fn' function
  (cl-defmethod krisb-org-node-custom-link-format-fn ((node org-mem-entry))
    "Bespoke function for `org-node-custom-link-format-fn'."
    (if (or (file-in-directory-p (org-node-get-file node) krisb-org-agenda-directory)
            (file-in-directory-p (org-node-get-file node) krisb-org-archive-directory))
        (org-node-get-title node)
      (let* ((place (krisb-org-node-get-place node))
             (type (krisb-org-node-get-type node))
             (title (org-node-get-title node))
             (file-title (org-node-get-file-title node)))
        (concat (when place (format "(%s) " place))
                (when type (format "{%s} " type))
                title
                (when (or (not (string= title file-title))
                          (not file-title))
                  (propertize (concat " (" file-title ")") 'face 'shadow))))))
  (setopt org-node-custom-link-format-fn #'krisb-org-node-custom-link-format-fn)

  ;; Bespoke org-node node accessors
  (cl-defmacro krisb-org-node--get-property (property node)
    "Get value of PROPERTY from NODE.
NODE is an org-mem-entry."
    `(cdr (assoc ,property (org-node-get-properties ,node) #'string-equal)))

  (cl-defmethod krisb-org-node-get-box ((node org-mem-entry))
    "Return the value of the ROAM_BOX property of NODE."
    (krisb-org-node--get-property "ROAM_BOX" node))

  (cl-defmethod krisb-org-node-box-or-dir ((node org-mem-entry))
    "Return a fontified value of the ROAM_BOX property of NODE.
If the ROAM_BOX property of NODE is nil, returns the directory name
containing NODE instead."
    (let ((box (krisb-org-node-get-box node))
          (dir (file-name-nondirectory
                (directory-file-name
                 (file-name-directory (org-node-get-file node))))))
      (propertize (or box (concat "/" dir)) 'face 'shadow)))

  (cl-defmethod krisb-org-node-get-place ((node org-mem-entry))
    "Return the value of the ROAM_PLACE property of NODE."
    (krisb-org-node--get-property "ROAM_PLACE" node))

  (cl-defmethod krisb-org-node-get-type ((node org-mem-entry))
    "Return the value of the ROAM_TYPE property of NODE."
    (krisb-org-node--get-property "ROAM_TYPE" node))

  (cl-defmethod krisb-org-node-get-person ((node org-mem-entry))
    "Return the value of the ROAM_PERSON property of NODE."
    (krisb-org-node--get-property "ROAM_PERSON" node))

  (cl-defmethod krisb-org-node-olp-full-propertized ((node org-mem-entry))
    "Return the full outline path of NODE fontified.
The full outline path of NODE (given by `org-node-get-olp-full')
surrounded by parentheses and whose parts are separated by \" > \".
Additionally, the entire string is fontified to the shadow face."
    (let ((olp (propertize (string-join (org-mem-olpath-with-file-title node) " > ") 'face 'shadow)))
      (unless (string-empty-p olp)
        (concat
         (propertize "(" 'face 'shadow)
         olp
         (propertize ")" 'face 'shadow)))))

  (cl-defmethod krisb-org-node-tags-propertized ((node org-mem-entry))
    "Return the full outline path of NODE fontified."
    (when-let ((tags (org-mem-tags node)))
      (propertize (concat "#" (string-join tags " #")) 'face 'org-tag)))

  ;; Bespoke `org-node-find' format
  (defun krisb-org-node-affixation-fn (node title)
    "Given NODE and TITLE, add a bespoke prefix and suffix.
For use as `org-node-affixation-fn'."
    (let ((box-or-dir (krisb-org-node-box-or-dir node))
          (place (krisb-org-node-get-place node))
          (type (krisb-org-node-get-type node))
          (person (krisb-org-node-get-person node))
          (olp-full (krisb-org-node-olp-full-propertized node))
          (tags (krisb-org-node-tags-propertized node)))
      (list title
            ;; Prefix
            (concat (when box-or-dir (concat box-or-dir " "))
                    (when place (propertize (concat place " ") 'face 'shadow))
                    (when type (propertize (concat "&" type " ") 'face 'font-lock-doc-face))
                    (when person (propertize (concat "@" person " ") 'face 'font-lock-keyword-face)))
            ;; Suffix
            (concat " "
                    (when olp-full (concat olp-full " "))
                    tags))))
  (setopt org-node-affixation-fn #'krisb-org-node-affixation-fn))

;; Rename buffer to the file's title if the file is an org-node node.
(with-eval-after-load 'org-node
  (defun krisb-org-node-rename-buffer-name-to-title ()
    "Rename org buffer to its #+TITLE property.
This only occurs when the file is an org-mem entry.  (See
`org-mem-watch-dirs' for files may contain entries.)

This function is written such that it calls org-mem and org-node as late
as possible, which is useful for ensuring those packages are lazy
loaded."
    ;; Our strategy for keeping org-mem and org-node lazy loaded is as
    ;; follows:
    ;; 1. Check if there is an ID at the top-level.
    ;; 2. Check if the ID has an associated org-mem entry (see
    ;;    `org-mem-watch-dirs'’).
    ;; 3. Check if entry is would be filter by `org-node-filter-fn’.
    (when-let* (((eq major-mode 'org-mode)) ; Guard
                ;; First check if there is an ID
                (id (save-excursion (widen) (org-id-get (point-min))))
                ((require 'org-mem))
                (entry (org-mem-entry-by-id id))
                ((require 'org-node))
                ((org-node-p entry))
                (title (org-mem-file-title-strict entry)))
      (rename-buffer (generate-new-buffer-name title (buffer-name)))))
  ;; The reason we add `krisb-org-node-rename-buffer-name-to-title’ to
  ;; `org-mode-hook’ here is because we do not want org-node being
  ;; loaded when we opn just any org-mode buffer.  Instead, we require
  ;; org-mem and org-node only when we need.  This helps keep org-mem
  ;; and org-node deferred as late as possible.
  (add-hook 'org-mode-hook #'krisb-org-node-rename-buffer-name-to-title))

;; Bespoke commands
(with-eval-after-load 'org-node
  (defun krisb-org-node-add-source-or-context ()
    "Set ROAM_CONTEXT or ROAM_SOURCE automatically.
Prompt the user for ROAM_CONTEXT or ROAM_SOURCE.  Set that property to
the value of the ROAM_REFS property of the nearest parent of the current
headline."
    (interactive)
    (org-set-property
     (completing-read "Type: " '("ROAM_CONTEXT" "ROAM_SOURCE"))
     (org-entry-get nil "ROAM_REFS" 'inherit))))

;;;;; Org-web-tools
(setup org-web-tools
  (:if-package org)
  (:package org-web-tools)

  (with-eval-after-load 'org
    (bind-keys :map org-mode-map
               ("C-c u" . org-web-tools-insert-link-for-url)))

  ;; Open then go into `view-mode'
  (with-eval-after-load 'org-web-tools
    (advice-add 'org-web-tools-read-url-as-org :after #'view-mode))

  ;; Add an org-attach entry for `org-web-tools-archive-attach’
  (with-eval-after-load 'org-attach
    (add-to-list 'org-attach-commands
                 '((?w) org-web-tools-archive-attach
                   "Download then attach an archive of a webpage using `org-web-tools'\n"))))

;;;;; Org-expiry
(setup org-expiry
  (:if-package org-contrib)
  (:load-after org)
  
  (with-eval-after-load 'org-expiry
    (setopt org-expiry-inactive-timestamps t)))

;;;;; Org-review
(setup org-review
  (:if-package org)
  (:package org-review)
  
  (with-eval-after-load 'org
    (bind-keys :map org-mode-map
               ("C-c r s" . org-review-insert-next-review)
               ("C-c r l" . org-review-insert-last-review)))
  (with-eval-after-load 'org-agenda
    (bind-keys :map org-agenda-mode-map
               ("C-c r s" . org-review-insert-next-review)
               ("C-c r l" . org-review-insert-last-review)))
  
  (with-eval-after-load 'org-review
    (setopt org-review-delay "+8d"
            org-review-last-timestamp-format 'inactive
            org-review-next-timestamp-format 'inactive
            org-review-sets-next-date t)))

(setup org-review
  ;; Agenda helpers
  (defun krisb-org-review-has-review-property-p ()
    "Skip the current todo if it has an org-review property.
Returns non-nil if the current todo has a property by the name of the
value of `org-review-next-property-name' or
`org-review-last-property-name'."
    (and (or (org-entry-get (point) org-review-next-property-name)
             (org-entry-get (point) org-review-last-property-name))
         (org-with-wide-buffer (or (outline-next-heading) (point-max)))))

  ;; Commands
  (defun krisb-org-review-unreview ()
    "Un-review the current heading.
Removes the properties denoted by `org-review-next-property-name' and
`org-review-last-property-name'."
    (interactive)
    (when (org-entry-get (point) org-review-next-property-name)
      (org-delete-property org-review-next-property-name))
    (when (org-entry-get (point) org-review-last-property-name)
      (org-delete-property org-review-last-property-name)))
  (with-eval-after-load 'org
    (bind-keys :map org-mode-map
               ("C-c r u" . krisb-org-review-unreview)))
  (with-eval-after-load 'org-agenda
    (bind-keys :map org-agenda-mode-map
               ("C-c r u" . krisb-org-review-unreview)))

  (defun krisb-org-review-scheduled-to-review ()
    "Turn the scheduled date of an agenda entry to a review date.
Sets the value of `org-review-next-property-name' to the scheduled
date.  Deletes the scheduled date afterward.

This command was initially used to help me transition from a
non-org-review workflow (a combination of an INBOX tag and scheduling)
to an org-review workflow."
    (interactive)
    (org-agenda-with-point-at-orig-entry nil
      (let ((date (org-entry-get (point) "SCHEDULED")))
        (if date
            (progn
              (org-set-property org-review-next-property-name
                                (concat "["
                                        (substring date 1 -1)
                                        "]"))
              (org-schedule '(4))
              (message "No scheduled date found for this item."))))))

  ;; "Scatter" org-review function; intended to be used as an org-agenda-bulk
  ;; action/function
  (defun krisb-org-review--select-day ()
    "Prompt for a number of days and return as an integer."
    (let ((days 0)
          (prompt "Scatter tasks across how many days? "))
      (while (<= days 0)
        (setq days (read-number prompt 7)
              prompt "Scatter tasks across how many days? Must be greater than 0: "))
      days))
  (defun krisb-org-review-randomize (days)
    "Randomly set the next review date for entry within the next DAYS days.
DAYS should be a positive integer.  Calls `org-review-insert-date' onto
a random date within the next DAYS days."
    (interactive (list (krisb-org-review--select-day)))
    (let* ((random-day (1+ (random days)))
           (ts (format-time-string (car org-time-stamp-formats)
                                   (time-add (current-time) (days-to-time random-day)))))
      ;; We don't also call `org-review-insert-last-review' because I sometimes
      ;; I do not want that.  In the cases when I'd like that function called as
      ;; well, I persist the org-agenda marks and call that function before or
      ;; after this one
      (org-review-insert-date org-review-next-property-name
                              org-review-next-timestamp-format
                              ts)))
  (with-eval-after-load 'org-agenda
    (add-to-list 'org-agenda-bulk-custom-functions
                 '(?R krisb-org-review-randomize
                      ;; Must return a list (of arguments)
                      (lambda () (list (krisb-org-review--select-day)))))))

;;;;; Org-hide-drawers
;; Make org drawers less visually obtrusive.
(setup org-hide-drawers
  ;; Elpaca:
  ;; :ensure ( :repo "https://github.com/krisbalintona/org-hide-drawers.git"
  ;;           :branch "devel")
  (:package (org-hide-drawers :url "https://github.com/krisbalintona/org-hide-drawers.git"
                              :branch "devel"))
  
  (:hide-mode)
  
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'org-hide-drawers-mode))
  
  (setopt org-hide-drawers-display-strings
          (list (list 'property-drawer-regexp
                      (lambda (property-drawer)
                        (let* ((property-info (org-hide-drawers--get-properties property-drawer))
                               (rating (alist-get "RATING" property-info nil nil #'string-equal)))
                          (propertize (when rating (concat "⎹ Rating — " rating " ")) 'face 'shadow)))
                      "^RATING$")
                (list 'property-drawer-regexp
                      (lambda (property-drawer)
                        (let* ((property-info (org-hide-drawers--get-properties property-drawer))
                               (box (alist-get "ROAM_BOX" property-info nil nil #'string-equal))
                               (place (alist-get "ROAM_PLACE" property-info nil nil #'string-equal)))
                          (propertize (if (or box place)
                                          (concat (when box (concat "⎹ Box — " box " "))
                                                  (when place (concat "⎹ Place — " place " ")))
                                        "󠁼⎹ NO INFO")
                                      'face 'shadow)))
                      "^ID$")
                (list 'property-drawer-regexp nil "^CUSTOM_ID$")
                (list 'property-drawer-regexp nil "^TOC$") ; For org-make-toc
                (list 'drawer-regexp nil "^CONTENTS$")     ; For org-make-toc
                (list 'drawer-regexp nil "^KEYTERM_INDEX$") ; For org-keyterm-index
                (list 'drawer-regexp (propertize "[Logbook]" 'face 'shadow) "^LOGBOOK$")
                (list 'drawer-regexp (propertize "[Hidden...]" 'face 'shadow) (rx (0+ anychar)))
                (list 'property-drawer-regexp (propertize " #" 'face 'shadow) (rx (0+ anychar))))))

(setup org-hide-drawers
  (with-eval-after-load 'org-hide-drawers
    (require 'transient)
    (transient-define-prefix org-hide-drawers-transient ()
      "Transient map for useful org-hide-drawers commands."
      [("h" "Hide drawers" org-hide-drawers-make-overlays)
       ("u" "Unhide drawers" org-hide-drawers-delete-overlays)
       ("t" "Toggle hiding" org-hide-drawers-toggle)])
    ;; TODO 2025-10-29: Autoload this keymap then define the transient
    ;; map when first needed?
    (bind-keys :map krisb-toggle-keymap
               ("h" . org-hide-drawers-transient))))

;;;;; Org-roam
(setup org-roam
  (:package org-roam)
  
  ;; Bind these keys only when I'm not using org-mem + org-node
  (unless (bound-and-true-p org-mem-updater-mode)
    (bind-keys :map krisb-note-keymap
               ("f" . org-roam-node-find)
               ("i" . org-roam-node-insert)
               ("c" . org-roam-capture)
               ("l" . org-roam-buffer-toggle)
               ("ta" . org-roam-tag-add)
               ("tr" . org-roam-tag-remove)
               ("g" . org-roam-graph)))

  (with-eval-after-load 'org-roam
    (setopt org-roam-directory krisb-notes-directory
            ;; org-roam-db-gc-threshold most-positive-fixnum
            org-roam-db-node-include-function
            (lambda () (not (member "ATTACH" (org-get-tags))))
            org-roam-mode-sections
            '((org-roam-backlinks-section :unique t)
              org-roam-reflinks-section))
    
    ;; Fold headings by default
    (add-to-list 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)

    ;; Having set user options, sync database when org-roam loads
    (org-roam-db-autosync-mode 1))
  
  ;; TODO 2025-05-27: Revisit this.
  ;; ;; Custom face for ID links to org-roam-nodes.  I prefer to change
  ;; ;; their foreground color to differentiate them from other types of
  ;; ;; links as well as to use a lighter face because a buffer
  ;; ;; packed-full of org links can become visually distracting and
  ;; ;; cluttered otherwise.
  ;; (org-link-set-parameters
  ;;  "id"
  ;;  :face (lambda (id)
  ;;          (if (org-roam-node-from-id id)
  ;;              '(:weight light :inherit font-lock-keyword-face)
  ;;            'org-link)))
  ;;
  ;; ;; Custom stored description
  ;; (org-link-set-parameters
  ;;  "id"
  ;;  :store (lambda (&optional interactive?)
  ;;           (let* ((id (org-id-get))
  ;;                  (node (org-roam-node-from-id id)))
  ;;             (if (and (equal major-mode 'org-mode)
  ;;                      ;; We want to check more than if there is a node at
  ;;                      ;; point; we want to make sure ID corresponds to an
  ;;                      ;; existing node
  ;;                      node)
  ;;                 (org-link-store-props :type "id"
  ;;                                       :link (concat "id:" id)
  ;;                                       :description (org-roam-node-formatted node))
  ;;               (funcall 'org-id-store-link-maybe interactive?)))))
  )

;;;;; Org-roam-folgezettel
(setup org-roam-folgezettel
  (:if-package org-roam)
  (:package (org-roam-folgezettel :url "https://github.com/krisbalintona/org-roam-folgezettel.git"
                                  :branch "vtable-unstable"))

  (add-hook 'org-roam-folgezettel-mode-hook #'hl-line-mode)
  (add-hook 'org-roam-folgezettel-mode-hook (lambda () (setq-local line-spacing 0.2)))

  (bind-keys :map krisb-note-keymap
             ("m" . org-roam-folgezettel-list)
             ;; TODO 2025-10-30: Bind only in org-mode buffers
             ("s" . org-roam-folgezettel-show-node-in-list))

  (setopt org-roam-folgezettel-default-filter-query '(box "main"))

  ;; FIXME 2025-06-30: Eventually upstream contents of
  ;; krisb-org-roam-ext once I figure out a generalizable zettelkasten
  ;; workflow for most/all users.
  (with-eval-after-load 'org-roam
    (require 'krisb-org-roam-ext))

  ;; Load embark integration
  (with-eval-after-load 'org-roam
    (with-eval-after-load 'embark
      (require 'org-roam-folgezettel-embark))))

(setup org-roam-folgezettel
  ;; We must add these after their default values are set by org
  (with-eval-after-load 'org
    (with-eval-after-load 'org-roam-folgezettel
      ;; Add ROAM_* properties to properties completing-read interface
      ;; completions
      (dolist (prop '("ROAM_EXCLUDE"
                      "ROAM_PLACE"
                      "ROAM_PERSON"
                      "ROAM_SOURCE"
                      "ROAM_CONTEXT"
                      "ROAM_REFS"
                      "ROAM_TYPE"
                      "ROAM_BOX"))
        (add-to-list 'org-default-properties prop))

      ;; Set inherited default values for some ROAM_* properties
      (add-to-list 'org-global-properties '("ROAM_TYPE" . "source collection pointer"))
      (add-to-list 'org-use-property-inheritance "ROAM_BOX"))))

;;;;; Org-ql
(setup org-ql
  (:package org-ql))

;; Copy Ihor's (https://github.com/yantar92/emacs-config) usage of
;; org-ql to define functions supplied to `org-agenda-skip-function'
;; to construct his org-agenda views
(setup org-ql
  (with-eval-after-load 'org-agenda
    (defvar prev-query nil)
    (defvar prev-buffer nil)
    (defvar prev-match-cdr nil)
    
    (define-advice org-agenda (:before (&rest _) reset-skip-cache)
      "Reset cache for `krisb-org-agenda-skip-org-ql'."
      (setq prev-query nil
            prev-buffer nil
            prev-match-cdr nil))

    (advice-add 'org-agenda-get-day-entries :before #'org-agenda@reset-skip-cache)
    (advice-add 'org-agenda-get-deadlines :before #'org-agenda@reset-skip-cache)
    (advice-add 'org-agenda-get-scheduled :before #'org-agenda@reset-skip-cache)
    (advice-add 'org-agenda-get-progress :before #'org-agenda@reset-skip-cache)
    (advice-add 'org-agenda-get-timestamps :before #'org-agenda@reset-skip-cache)
    (advice-add 'org-agenda-get-sexps :before #'org-agenda@reset-skip-cache)
    (advice-add 'org-agenda-get-blocks :before #'org-agenda@reset-skip-cache)
    (advice-add 'org-agenda-get-todos :before #'org-agenda@reset-skip-cache)

    (defun krisb-org-agenda-skip-org-ql (query &optional force)
      "Construct skip function using org-ql QUERY.
Do not use cache when FORCE is non-nil."
      (let ((match-list
             (if (and (cdr prev-match-cdr)
                      (equal query prev-query)
                      (equal prev-buffer (current-buffer))
                      (not force))
                 prev-match-cdr
               (sort (org-ql-select (list (current-buffer))
                       query
                       :narrow t
                       :action (lambda (&optional el)
                                 (if el
                                     (org-element-property :begin (org-element-lineage el '(headline inlinetask) t))
				   (org-element-property :begin (org-element-lineage (org-element-at-point) '(headline inlinetask) t)))))
                     #'<)))
            (cur-point (save-excursion
                         (org-back-to-heading t)
                         (point))))
        (if (not match-list)
            (point-max)
	  (catch :exit
            (unless (eq prev-match-cdr match-list)
              (setq prev-match-cdr match-list
                    prev-query query
                    prev-buffer (current-buffer)))
            (while prev-match-cdr
              (when (= cur-point (car prev-match-cdr))
                (throw :exit nil))
              (when (< cur-point (car prev-match-cdr))
                (throw :exit (car prev-match-cdr)))
              (setq prev-match-cdr (cdr prev-match-cdr)))
            (point-max)))))))

;;;; Bespoke extensions
;; Log changes to certain properties.
;;
;; The following describes how it works.  We add a function to
;; `org-property-changed-functions' (`krisb-org-log-property-change')
;; that calls `org-add-log-setup' to log a change to properties listed
;; in `krisb-org-log-properties' when they are changed.  (This logging
;; follows the value of of the `org-log-into-drawer' option.)  The
;; PURPOSE argument of `org-add-log-setup' is 'property; a
;; corresponding entry in `org-log-note-headings' is added.  This
;; option controls the format of logged entries based on the PURPOSE
;; argument passed to `org-add-log-setup'.
;;
;; `org-log-note-headings' offers several %-escape sequences.  My
;; desired behavior is to (i) log the name of the property and (ii)
;; the old value of the property, before it is changed.  We abuse the
;; "%s" escape sequence (initially for original todo states) to insert
;; the name of the property changed (by also passing the name of the
;; property as the STATE argument in `org-add-log-setup').  The
;; @-escape sequences offered cannot accommodate (ii) neatly.  To
;; accomplish (ii), we advise `org-entry-put' to store the value of
;; the property before it is changed in
;; `krisb-org-log-property-before'.  Then, we pass this as the
;; PREV-STATE in `krisb-org-log-property-change' so that it can be
;; inserted via the "%S" escape sequence in `org-log-note-headings'.
;; 
;; See also
;; https://www.reddit.com/r/emacs/comments/7gr9ps/add_logbook_entry_and_note_on_orgmode_property/.
(with-eval-after-load 'org
  (defvar krisb-org-log-properties nil
    "List of properties that are logged when their value is changed.
Users can set this variable globally, buffer locally, and directory
locally.")
  (put 'krisb-org-log-properties 'safe-local-variable #'listp)

  (defvar krisb-org-log-property-before nil
    "Temporary storage of property value before its change.")

  (defun krisb-org-log-property-set-before (epom property _value)
    "Set the value of `krisb-org-log-property-before'.
Meant as advice before `org-entry-put'.  Set the value of
`krisb-org-log-property-before' to the value of the property set by
`org-entry-put' before it is changed."
    (setq krisb-org-log-property-before (org-entry-get epom property)))
  (advice-add 'org-entry-put :before #'krisb-org-log-property-set-before)

  (defun krisb-org-log-property-change (prop _newval)
    "Log when certain properties change value.
PROP is the name of the property being changed and _NEWVAL is the value
this property has been set to."
    ;; NOTE: We allow inserting empty values of
    ;; `krisb-org-log-property-before' (i.e., when PROP is set for the
    ;; first time) because org's logging of todo state changes does
    ;; the same.
    (when (member prop krisb-org-log-properties)
      (org-add-log-setup 'property prop krisb-org-log-property-before 'time)))
  (add-hook 'org-property-changed-functions #'krisb-org-log-property-change)

  ;; We abuse the "%s" key in `org-log-note-headings' to store the
  ;; name of the property changed rather than the previous todo state,
  ;; which is the intended use of "%s"
  (add-to-list 'org-log-note-headings '(property . "Property %s changed from %S on %t")))

;; Bespoke settings for my media tracking system
(with-eval-after-load 'org
  (defun krisb-org-property-set-rating (&rest _args)
    "Read a number for the RATING property.
Prompts for a number until the user chooses a number between 0 and 10,
inclusive.

ARGS are the same as `completing-read'."
    (let* ((existing-rating (org-entry-get (point) "RATING"))
           (default (when existing-rating (string-to-number existing-rating)))
           (rating
            (read-number "Input a rating between 0 and 10, inclusive: " default)))
      (while (or (< rating 0) (> rating 10))
        (sleep-for 0.5)
        (setq rating (read-number "Rating not between 0 and 10, inclusive: " default)))
      (number-to-string rating)))
  (add-to-list 'org-property-set-functions-alist
               '("RATING" . krisb-org-property-set-rating))

  (defun krisb-org-property-allowed-platform (prop)
    "Return allowed list of values for the PLATFORM property.
PROP is the name of the property.  See
`org-property-allowed-value-functions'."
    (when (string= prop "PLATFORM")
      '("Netflix" "Hulu" "Disney+" "Tubi" ":ETC")))
  (add-to-list 'org-property-allowed-value-functions #'krisb-org-property-allowed-platform))

;;; Paren-face
;; Creates a face just for parentheses.  Useful for lispy languages
;; where readers want the parentheses as unnoticeable as possible.
(setup paren-face
  (:package paren-face)

  (:hide-mode)
  (setopt paren-face-mode-lighter " PF")

  (global-paren-face-mode 1))

;;; Consult
(setup consult
  (:package consult)

  (bind-keys ("C-x B" . consult-buffer)
             ;; Remaps of built-ins
             ([remap yank-pop] . consult-yank-pop)
             ([remap goto-line] . consult-goto-line)
             ([remap bookmark-jump] . consult-bookmark)
             ([remap Info-search] . consult-info)
             ([remap imenu] . consult-imenu)
             ([remap flymake-show-buffer-diagnostics] . consult-flymake)
             ([remap repeat-complex-command] . consult-complex-command)
             :map goto-map              ; The `M-g' prefix
             ("f" . consult-flymake)
             ("o" . consult-outline)
             ("e" . consult-compile-error)
             ("l" . consult-line)
             ("a" . consult-org-agenda)
             ("m" . consult-mark)
             :map search-map            ; The `M-s' prefix
             ("i" . consult-info)
             ("g" . consult-git-grep)
             ("G" . consult-grep)
             ("r" . consult-ripgrep)
             ("f" . consult-find)
             ("F" . consult-locate))
  (with-eval-after-load 'org
    (bind-key [remap consult-outline] #'consult-org-heading org-mode-map))

  (:bind-keys :map consult-narrow-map
              ("?" . consult-narrow-help)) ; Show available narrow keys

  (setopt consult-preview-key "C-M-;"
          consult-ripgrep-args
          (concat
           "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --search-zip"
           ;; Additional args
           " --line-number --hidden"))

  ;; `consult-bookmark-narrow'
  (with-eval-after-load 'activities
    (add-to-list 'consult-bookmark-narrow '(?a "Activities" activities-bookmark-handler)))
  (with-eval-after-load 'pdf-tools
    (add-to-list 'consult-bookmark-narrow '(?p "PDFs" pdf-view-bookmark-jump-handler)))

  ;; Add log-edit histories to `consult-mode-histories'
  (with-eval-after-load 'consult
    (add-to-list 'consult-mode-histories
                 '(log-edit-mode
                   log-edit-comment-ring
                   log-edit-comment-ring-index
                   log-edit-beginning-of-line)))

  ;; TODO 2025-10-20: Revisit this
  ;; ;; Use the faster plocate rather than locate
  ;; (when (executable-find "plocate")
  ;;   (setopt consult-locate-args "plocate --ignore-case --existing --regexp"))

  ;; Remove sources from `consult-buffer’ I dislike.  Alternatively, I
  ;; could make these hidden, allowing access to their filter despite
  ;; being unseen.
  (with-eval-after-load 'consult
    (dolist (source '(consult--source-recent-file
                      consult--source-file-register
                      consult--source-bookmark
                      consult--source-project-recent-file-hidden))
      (delq source consult-buffer-sources))))

;; Pulsar pulses
(setup pulsar
  (with-eval-after-load 'pulsar
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)))

;;; Time
(setup time

  (setopt display-time-24hr-format t
          display-time-format "%R"
          display-time-interval 60
          display-time-default-load-average nil)

  (display-time-mode 1)

  ;; Time zones for `world-clock'
  (setopt world-clock-list
          '(("America/Los_Angeles" "Seattle")
            ("America/New_York" "New York")
            ("Europe/London" "London")
            ("Europe/Paris" "Paris")
            ("Europe/Nicosia" "Nicosia (capital of Cyprus)")
            ("Asia/Calcutta" "Bangalore")
            ("Asia/Tokyo" "Tokyo")
            ("Asia/Shanghai" "Beijing"))))

;;; Diff-mode
;; TODO 2025-06-07: Document:
;; - `diff-font-lock-syntax’
;; - `diff-refine’
(setup diff-mode

  (setopt diff-font-lock-prettify t ; Make diff headers look like Magit’s
          ;; 2024-10-23 TODO: Revisit this. I think it causes a bug in
          ;; vc-jj
          ;; diff-default-read-only t
          )

  (:bind-keys :map diff-mode-map
              ("v" . vc-next-action))

  (add-hook 'diff-mode-hook #'diff-delete-empty-files)

  ;; TODO 2025-06-07: Revisit this.
  ;; (krisb-modus-themes-setup-faces
  ;;  "diff-mode"
  ;;  (set-face-attribute 'diff-header nil
  ;;                      :height 1.2
  ;;                      :overline t
  ;;                      :width 'expanded
  ;;                      :foreground (modus-themes-with-colors fg-alt)
  ;;                      :extend t)
  ;;  (set-face-attribute 'diff-hunk-header nil
  ;;                      :height 1.1
  ;;                      :slant 'italic
  ;;                      :foreground 'unspecified
  ;;                      :background (modus-themes-with-colors bg-dim)))
  )

(setup pulsar
  ;; Pulsar pulses while navigating
  (with-eval-after-load 'pulsar
    (add-to-list 'pulsar-pulse-functions 'diff-file-next)
    (add-to-list 'pulsar-pulse-functions 'diff-file-prev)
    (add-to-list 'pulsar-pulse-functions 'diff-hunk-next)
    (add-to-list 'pulsar-pulse-functions 'diff-hunk-prev)
    (add-to-list 'pulsar-pulse-functions 'diff-hunk-kill)))

;;; Find-func
;; Binds useful commands for jumping to variables, functions, and
;; libraries
(setup find-func

  ;; Emacs 31 converted `find-function-setup-keys' into
  ;; `find-function-setup-keys'
  (if (fboundp 'find-function-mode)
      (find-function-mode 1)
    (find-function-setup-keys)))

(setup tab-bar
  ;; Useful keybinds for `tab-bar-mode' usage
  (with-eval-after-load 'tab-bar
    (defun krisb-find-library-other-tab (library)
      "Find LIBRARY in other tab."
      (interactive (list (read-library-name)))
      (switch-to-buffer-other-tab
       (save-window-excursion (funcall-interactively #'find-library library))))

    (defun krisb-find-function-other-tab (function)
      "Find FUNCTION in other tab."
      (interactive (find-function-read))
      (find-function-do-it function nil 'switch-to-buffer-other-tab))

    (:bind-keys :map tab-prefix-map
                ("F" . krisb-find-function-other-tab)
                ("L" . krisb-find-library-other-tab))))

;;; Highlight-function-calls
(setup highlight-function-calls
  (:package highlight-function-calls)

  (dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
    (add-hook hook #'highlight-function-calls-mode))

  (setopt highlight-function-calls-not t
          highlight-function-calls-macro-calls t
          highlight-function-calls-special-forms t)

  (:face highlight-function-calls-face ((t (:underline nil :inherit font-lock-function-call-face)))))

;;; Puni
;; Puni: major-mode-agnostic structural editing.  We use some of its
;; commands, but do not enable the mode.
(setup puni
  (:package puni)

  (setopt puni-confirm-when-delete-unbalanced-active-region nil)

  (bind-keys
   ("C-S-o" . puni-split)
   ("M-+" . puni-splice)
   ("M-R" . puni-raise)
   ([remap transpose-sexps] . puni-transpose)
   ([remap kill-word] . puni-forward-kill-word)
   ([remap backward-kill-word] . puni-backward-kill-word)
   ([remap insert-parentheses] . puni-syntactic-backward-punct)
   ([remap move-past-close-and-reindent] . puni-syntactic-forward-punct)))

;;; Whitespace
;; Visualize whitespace so mistakes are more easily detectable.
(setup whitespace

  (setopt whitespace-style             ; See also `whitespace-cleanup'
          '( empty face tab-mark tabs page-delimiters
             indentation space-before-tab space-after-tab)
          whitespace-display-mappings '((tab-mark ?\t [?› ?\t])
                                        (newline-mark ?\n [?¬ ?\n])
                                        (space-mark ?\  [?·] [?.]))
          whitespace-line-column nil)

  (add-hook 'prog-mode-hook #'whitespace-mode)
  (:hide-mode))

;;; EAT
(setup eat
  ;; ;; 2024-12-29: See https://codeberg.org/akib/emacs-eat/pulls/133 for why we
  ;; ;; use this fork of eat.
  ;; :ensure ( :repo "https://codeberg.org/vifon/emacs-eat.git"
  ;;           :branch "fish-integration")
  (:package eat)

  (bind-keys :map krisb-open-keymap
             ("s" . eat))

  ;; 2025-04-05: This resolves the continuation lines issue in EAT
  ;; terminal (including eat-shell in
  ;; `eat-eshell-visual-command-mode').  The continuation line issue
  ;; results in, I think, the default font being too wide, causing the
  ;; width of the characters to exceed the width of the window,
  ;; resulting in ugly continuation lines that ruin the wrapping of
  ;; the output.
  (defun krisb-eat--setup ()
    "Set up an EAT terminal shell."
    (when (featurep 'fontaine)
      (set-face-attribute 'eat-term-font-0 nil
                          ;; This returns the default-family of the current
                          ;; preset, whether explicitly or implicitly set
                          :family (fontaine--get-preset-property fontaine-current-preset :term-family))))
  (with-eval-after-load 'fontaine
    (add-hook 'fontaine-set-preset-hook #'krisb-eat--setup)))

;; Integration with eshell
(setup eat
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

;; Integration with project.el
(setup eat
  (bind-keys :map project-prefix-map
             ("s" . eat-project))       ; Overshadow `project-shell’
  ;; Replace `project-shell’ with `eat-project’ in
  ;; `project-switch-commands’ anytime its value is changed
  (add-variable-watcher 'project-switch-commands
                        (lambda (_symbol newval operation where)
                          (when (equal operation 'set)
                            (setq project-switch-commands
                                  (mapcar (lambda (e)
                                            (if (equal e '(project-shell "Shell"))
                                                '(eat-project "EAT")
                                              e))
                                          project-switch-commands))))))

;; Integration with vc.el
(setup eat
  (with-eval-after-load 'vc
    ;; NOTE 2025-11-13: I've created an issue to get this into
    ;; upstream: https://codeberg.org/akib/emacs-eat/issues/241
    (add-to-list 'vc-deduce-backend-nonvc-modes 'eat-mode)))

;; Unbind M-<NUMBER> keybinds because I use them for switching betweeb
;; tab-bar tabs
(setup eat
  (with-eval-after-load 'tab-bar
    ;; 2025-10-23 TODO: Why not just use
    ;; `eat-semi-char-non-bound-keys' instead?
    (:bind-keys :map eat-semi-char-mode-map
                ("M-1" . nil)
                ("M-2" . nil)
                ("M-3" . nil)
                ("M-4" . nil)
                ("M-5" . nil)
                ("M-6" . nil)
                ("M-7" . nil)
                ("M-8" . nil)
                ("M-9" . nil)
                ("M-0" . nil))))

;;; Olivetti
(setup olivetti
  (:package olivetti)

  (:hide-mode)

  (dolist (hook '(org-mode-hook
                  Info-mode-hook
                  emacs-news-view-mode-hook
                  org-msg-edit-mode-hook
                  markdown-mode-hook))
    (add-hook hook #'olivetti-mode))

  (:bind-keys :map olivetti-mode-map
              ("C-c |" . nil))

  (with-eval-after-load 'olivetti
    (setopt olivetti-body-width 0.55
            olivetti-minimum-body-width 80
            olivetti-margin-width 8
            olivetti-style 'fancy)) ; Fancy makes the buffer look like a page

  ;; TODO 2025-05-22: Revisit this.
  ;; ;; FIXME 2024-01-11: This is a temporary solution. Olivetti's
  ;; ;; changing of margins and fringes messes with the calculation of
  ;; ;; `mode--line-format-right-align', which determines where the right
  ;; ;; side of the mode line is placed.
  ;; (mode-line-format-right-align
  ;;  '(:eval (if (and (bound-and-true-p olivetti-mode)
  ;;                   olivetti-style)     ; 'fringes or 'fancy
  ;;              (let ((mode-line-right-align-edge 'right-fringe))
  ;;                (mode--line-format-right-align))
  ;;            (mode--line-format-right-align))))

  ;; TODO 2025-05-22: Revisit this.
  ;; (krisb-modus-themes-setup-faces
  ;;  "olivetti"
  ;;  (set-face-attribute 'olivetti-fringe nil
  ;;                      :background bg-dim
  ;;                      :inherit 'unspecified))

  ;; TODO 2025-05-22: Revisit this.
  ;; ;; Set `bookmark-face' buffer-locally
  ;;   (defun krisb-olivetti-set-bookmark-face ()
  ;;     "Sets the buffer-local specification of `bookmark-face'.
  ;; We do this because the olivetti settings may change the background color
  ;; of the fringe, meaning bookmark fringe marks, which use the default
  ;; fringe background color, are out of place."
  ;;     (face-remap-add-relative 'bookmark-face :inherit '(olivetti-fringe success)))
  ;; (add-hook 'olivetti-mode-hook #'krisb-olivetti-set-bookmark-face)
  )

;;; Desktop
;; TODO 2025-05-20: Document in literate configuration prose.
;; See also `desktop-locals-to-save'
;; Save buffers across Emacs sessions
(setup desktop
  ;; ;; 2025-05-22: This is a workaround for elpaca.  Look at the bottom
  ;; ;; of desktop.el’s file: we must avoid enabling `desktop-save-mode’
  ;; ;; immediately when desktop is loaded because the desktop.el file
  ;; ;; automatically adds to `after-init-hook’ to call `desktop-read’ if
  ;; ;; `desktop-save-mode’ is enabled. This is problematic because, on
  ;; ;; account of elpaca’s asynchronicity, some packages will end up not
  ;; ;; being ready by the time desktop loads a buffer relevant to its
  ;; ;; behavior.  For example, outli is not enabled in buffers it should
  ;; ;; be because desktop opened them before outli could load.  Another
  ;; ;; example is org: if desktop loads an org buffer before the org
  ;; ;; declaration for elpaca, the built-in version is loaded before the
  ;; ;; use-package declarataion for org, meaning that elpaca will
  ;; ;; recognize the package as already available and therefore not
  ;; ;; install and use a more upgraded version.
  ;; ;;
  ;; ;; So we should avoid having desktop open buffers before elpaca is
  ;; ;; done initializing the packages.  One solution would be to just
  ;; ;; enable `desktop-save-hook’ after elpaca initializes, then call
  ;; ;; `desktop-read’ manually.  However, this would ignore the
  ;; ;; --no-desktop flag called with Emacs.  Instead, we opt to enabl
  ;; ;; `desktop-save-mode’ then add the very hook that desktop adds to
  ;; ;; `after-init-hook’ `elpaca-after-init-hook’.
  ;; (elpaca-after-init-hook
  ;;  . (lambda ()
  ;;      (desktop-save-mode 1)
  ;;      (let ((key "--no-desktop"))
  ;;        (when (member key command-line-args)
  ;;          (setq command-line-args (delete key command-line-args))
  ;;          (desktop-save-mode 0)))
  ;;      (when desktop-save-mode
  ;;        (desktop-read)
  ;;        (setq inhibit-startup-screen t))))
  (desktop-save-mode 1)

  (setopt desktop-load-locked-desktop 'check-pid
          desktop-save 'ask-if-new
          desktop-auto-save-timeout 3)
  ;; TODO 2025-05-19: Revisit this.
  ;; (desktop-files-not-to-save
  ;;  (rx (or (regexp "\\(\\`/[^/:]*:\\|(ftp)\\'\\)")
  ;;          ;; Don't save files from other Emacs repos because sometimes they
  ;;          ;; have local variables that mess with desktop's loading of files
  ;;          (literal (expand-file-name "emacs-repos/" "~"))
  ;;          ;; Don't want to open my large org-agenda files which I'll open
  ;;          ;; eventually anyway
  ;;          (literal krisb-org-agenda-directory))))
  (setopt desktop-restore-eager t
          desktop-restore-forces-onscreen nil
          desktop-restore-frames t
          desktop-restore-in-current-display nil)
  (add-to-list 'desktop-globals-to-save '(kill-ring . 5000)))

;; Also save and restore narrowing state.  Taken from
;; https://www.reddit.com/r/emacs/comments/162cjki/comment/jxzrthx/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(setup desktop
  (defun krisb-desktop--save-narrowing ()
    "Save narrowed information."
    (setq-local desktop-save-buffer
                (lambda (_desktop-dirname)
                  (when (buffer-narrowed-p) (list 'narrowed (point-min) (point-max))))))
  (dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
    (add-hook hook #'krisb-desktop--save-narrowing))

  (defun krisb-desktop--restore-narrowing (_f n misc &rest _)
    "Restore narrowing of buffer."
    (when (and misc (eq (car misc) 'narrowed))
      (apply #'narrow-to-region (cdr misc))))
  (advice-add 'desktop-restore-file-buffer :after #'krisb-desktop--restore-narrowing))

;;; Smart-mark
;; When pressing C-g while marking a region, move point to the
;; location the marking command was invoked from.
(setup smart-mark
  (:package smart-mark)

  (smart-mark-mode 1))

;;; Message
;; TODO 2025-05-23: Document:
;; - `message-confirm-send’
;; - `mml-attach-file-at-the-end’
;; - `message-ignored-cited-headers’
;; - `message-alternative-emails’
;; - `message-confirm-send’
;; - `message-send-rename-function’
;; - `message-generate-new-buffers’
;; - `message-mark-insert-begin’
;; - `message-mark-insert-end’
;; - `message-signature-insert-empty-line’
;; TODO 2025-05-23: Can we use `message-add-action’ effectively?  See
;; (info "(message) Message Actions")
;; TODO 2025-05-23: Document that users should probably read all the
;; nodes in (info "(message) Commands")
;; Universal email composition mode.  See (info "(message) Variables")
;; for more information.
(setup message

  (with-eval-after-load 'message
    (add-hook 'message-setup-hook #'message-sort-headers)
    (add-hook 'message-mode-hook #'flymake-mode)
    (add-hook 'message-mode-hook #'olivetti-mode)
    (add-hook 'message-mode-hook #'mixed-pitch-mode))

  (setopt message-directory krisb-email-directory
          message-mail-user-agent t)    ; Use `mail-user-agent'

  ;; Citations. See e.g. `message-cite-style-gmail' for the options
  ;; relevant to citations.  Importantly, I can set these options
  ;; buffer locally.
  (setopt message-cite-function 'message-cite-original-without-signature
          message-citation-line-function 'message-insert-formatted-citation-line
          message-citation-line-format "On %a, %b %d %Y, %N wrote:\n"
          message-cite-reply-position 'below)

  ;; Composition
  (setopt message-hidden-headers nil   ; Show all headers
          ;; Generates all headers in the variables
          ;; `message-required-headers’,
          ;; `message-required-news-headers', and
          ;; `message-required-mail-headers'.  Otherwise, unless
          ;; another package manually adds headers (e.g. mu4e), those
          ;; headers won't be inserted into a message draft buffer.  I
          ;; enable this to make sure that the date header is inserted
          ;; in a draft.  (No date header means the date is set to
          ;; time 0, which is annoying for querying emails via their
          ;; date using e.g. notmuch.)
          message-generate-headers-first t
          message-wide-reply-confirm-recipients t
          message-elide-ellipsis "> [... %l lines elided]\n"
          message-signature-insert-empty-line t
          message-signature "Kind regards,\nKristoffer\n"
          message-signature-separator "^-- *$")
  ;; REVIEW 2025-05-23: `message-auto-save-directory’ should be set
  ;; relative to `message-directory’, but based on the order of
  ;; evaluation, it never does so correctly when we set
  ;; `message-directory’ via :custom.  Submit a patch upstream?
  (setopt message-auto-save-directory krisb-email-drafts-directory ; Directory where drafts are saved
          message-subject-trailing-was-query 'ask
          message-kill-buffer-on-exit t
          mml-dnd-attach-options t)

  ;; Forwarding
  (setopt message-forward-as-mime t          ; NOTE 2024-09-27: Experimental
          ;; TODO 2025-05-23: Change value per-email depending on
          ;; `message-cite-reply-position'?
          message-forward-before-signature nil)

  ;; TODO 2025-05-23: Revisit this.
  ;; (krisb-modus-themes-setup-faces
  ;;  "message"
  ;;  (set-face-attribute 'message-mml nil :weight 'bold :background bg-sage))
  )

(setup message
  (with-eval-after-load 'mu4e
    (setopt mu4e-attachment-dir (expand-file-name ".attachments/" message-directory))))

(setup message
  (with-eval-after-load 'message
    ;; Taken from Doom. Detect empty subjects, and give users an
    ;; opportunity to fill something in
    (defun krisb-message-check-subject ()
      "Check that a subject is present, and prompt for a subject if not."
      (save-excursion
        (goto-char (point-min))
        (search-forward "--text follows this line--")
        (re-search-backward "^Subject:")
        (let ((subject (string-trim (substring (thing-at-point 'line) 8))))
          (when (string-empty-p subject)
            (end-of-line)
            (insert (read-string "Subject (optional): "))))))
    (add-hook 'message-send-hook #'krisb-message-check-subject)

    (defun krisb-message-check-from ()
      "Prompt user to confirm sending from this email.
If the `user-mail-address’ does not match the email in the FROM header,
ask to confirm.  This is useful if we have multiple email addresses and
ensure `user-mail-address’ matches the one we currently would like to
send from."
      (save-excursion
        (goto-char (point-min))
        (search-forward "--text follows this line--")
        (re-search-backward "^From:")
        (let ((from (string-trim (substring (thing-at-point 'line) 5))))
          (when (and (not (string-match-p (rx (literal user-mail-address)) from))
                     (not (yes-or-no-p (concat
                                        "Are you sure you want to send from "
                                        (propertize from 'face 'highlight)
                                        "?"))))
            (cl--set-buffer-substring (pos-bol) (pos-eol)
                                      (concat
                                       "From: "
                                       (read-string "Set FROM to: " user-mail-address)))))))
    (add-hook 'message-send-hook #'krisb-message-check-from)))

;;; Mixed-pitch
;; Locally remap default face to variable-pitch.
(setup mixed-pitch
  (:package mixed-pitch)
  
  (:hide-mode)

  ;; We don't want to set the height of variable-pitch faces because
  ;; non-variable-pitch faces will be "out of sync" with the height.
  ;; Therefore, to have larger font sizes in these buffers, we have to
  ;; remap those faces manually and locally.
  (setopt mixed-pitch-set-height nil
          mixed-pitch-variable-pitch-cursor nil))

;;; Footnote.el
;; TODO 2025-05-24: Document:
;; - `footnote-section-tag’
;; Footnotes for `message-mode'
(setup footnote
  
  (add-hook 'message-mode-hook #'footnote-mode)

  (:hide-mode)

  (with-eval-after-load 'footnote
    (setopt footnote-mode-line-string "FN"
            footnote-spaced-footnotes nil
            footnote-prompt-before-deletion nil)))

;;; Notmuch
;; TODO 2025-05-23: Document:
;; - `notmuch-identities’
(setup notmuch
  (:package notmuch)
  ;; For AUR:
  ;; :ensure-system-package (notmuch
  ;;                         (gmi . lieer-git))

  (bind-keys ([remap compose-mail] . notmuch-mua-new-mail)
             :map krisb-open-keymap
             ("n" . notmuch))
  (:bind-keys :map notmuch-search-mode-map
              ("a" . nil)           ; The default is too easy to hit accidentally
              ("/" . notmuch-search-filter)
              ("r" . notmuch-search-reply-to-thread)
              ("R" . notmuch-search-reply-to-thread-sender)
              :map notmuch-tree-mode-map
              ("S-SPC" . notmuch-tree-scroll-message-window-back)
              :map notmuch-show-mode-map
              ("S-SPC" . notmuch-show-rewind)
              ("a" . nil)
              ("r" . notmuch-show-reply)
              ("R" . notmuch-show-reply-sender)
              ;; TODO 2025-05-23: Revisit these.  Also move to krisb-notmuch-ext
              ;; configuration
              ;; ("T" . krisb-notmuch-show-trash-thread-then-next)
              ;; ([remap notmuch-show-advance-and-archive] . krisb-notmuch-show-advance-and-tag)
              )

  (setopt mail-user-agent 'notmuch-user-agent)

  ;; Hello UI
  (setopt notmuch-hello-sections (list #'notmuch-hello-insert-saved-searches
                                       #'notmuch-hello-insert-alltags
                                       #'notmuch-hello-insert-recent-searches)
          notmuch-hello-thousands-separator ","
          notmuch-show-all-tags-list t)

  ;; Notmuch-searches
  (setopt notmuch-saved-searches
          '(( :name "inbox"
              :query "(tag:inbox and not tag:list) or (tag:inbox and tag:watch)"
              :sort-order oldest-first
              :key "i")
            ( :name "Emacs mailing lists"
              :query "tag:list and tag:inbox and tag:emacs"
              :sort-order newest-first
              :key "e")
            ( :name "Guix mailing lists"
              :query "tag:list and tag:inbox and (tag:guix or tag:mumi)"
              :sort-order newest-first
              :key "g")
            ( :name "Other mailing lists"
              :query "tag:list and tag:inbox and (path:l2md/other/** or List:\"~abcdw/rde-discuss@lists.sr.ht\" or to:\"~abcdw/rde-discuss@lists.sr.ht\""
              :sort-order newest-first
              :key "o")
            ( :name "sent"
              :query "tag:sent"
              :sort-order newest-first
              :key "s")
            ( :name "drafts"
              :query "tag:draft or path:drafts/"
              :sort-order newest-first
              :key "d"
              :search-type unthreaded)
            ( :name "archived"
              :query "not tag:inbox and not tag:trash"
              :key "a")
            ( :name "all"
              :query "path:**"
              :key "A")
            ( :name "trash"
              :query "tag:trash"
              :key "t")))
  ;; See `man' for mbsync and notmuch to see valid search terms. See
  ;; https://www.emacswiki.org/emacs/NotMuch#h5o-2 on how to expunge local files
  ;; via cli
  (setopt notmuch-search-hide-excluded t
          notmuch-show-empty-saved-searches t
          notmuch-search-oldest-first nil
          notmuch-search-result-format '(("date" . "%14s ")
                                         ("count" . "%-7s ")
                                         ("authors" . "%-30s ")
                                         ("subject" . "%-75.75s ")
                                         ("tags" . "(%s)")))

  ;; Tags
  (setopt notmuch-archive-tags '("-inbox")
          notmuch-message-replied-tags '("+replied")
          notmuch-message-forwarded-tags '("+forwarded")
          notmuch-show-mark-read-tags '("-unread")
          notmuch-draft-tags '("+draft")
          notmuch-draft-folder     ; Relative to root of the notmuch database
          (file-relative-name krisb-email-drafts-directory krisb-email-directory)
          notmuch-draft-save-plaintext 'ask
          notmuch-tagging-keys
          `(("a" notmuch-archive-tags "Archive")
            ("r" notmuch-show-mark-read-tags "Mark read")
            ("f" ("+flagged") "Flag")
            ("s" ("+spam" "-inbox") "Mark as spam")
            ("t" ("+trash" "-inbox") "Trash"))
          notmuch-tag-formats
          '(("unread" (propertize tag 'face 'notmuch-tag-unread))
            ("flagged" (propertize tag 'face 'notmuch-tag-flagged))
            ("watch" (propertize tag 'face 'font-lock-warning-face)))
          notmuch-tag-deleted-formats
          '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
            (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

  ;; Notmuch-show-mode (i.e. reading emails)
  (with-eval-after-load 'notmuch-show
    (add-hook 'notmuch-show-hook #'olivetti-mode)
    (add-hook 'notmuch-show-hook #'visual-line-mode)
    (add-hook 'notmuch-show-hook #'visual-wrap-prefix-mode))
  (setopt notmuch-show-relative-dates t
          notmuch-show-all-multipart/alternative-parts nil
          notmuch-show-indent-multipart nil
          notmuch-show-indent-messages-width 3 ; We can toggle indentation anyway
          notmuch-show-part-button-default-action 'notmuch-show-interactively-view-part
          notmuch-show-text/html-blocked-images "." ; Block everything
          notmuch-wash-wrap-lines-length nil
          notmuch-unthreaded-show-out t
          notmuch-message-headers-visible nil
          ;; The order of headers in this list seems to be the order in which
          ;; they will appear in `notmuch-show’ buffers.  See also the user
          ;; option `notmuch-show-message-visible'.  Additionally, to add
          ;; headers to this list that are beyond the default, you must add to
          ;; the “extra_headers” setting in the “show” section of your notmuch
          ;; config.  Finally, to have these extra headers be query-able via
          ;; notmuch search queries, be sure to define a search term prefix
          ;; for it.  (See (info "(notmuch-config) DESCRIPTION") for how to
          ;; achieve such a set up.)
          notmuch-message-headers '("To" "Cc" "List-Id" ; Show mailing list ID
                                    "Date" "Subject")
          notmuch-multipart/alternative-discouraged
          '("text/html" "multipart/related"
            ;; FIXME 2025-05-23: This doesn’t work?
            "text/x-patch"))

  ;; Notmuch-tree-mode
  (setopt notmuch-tree-show-out nil
          notmuch-tree-result-format '(("date" . "%12s  ")
                                       ("authors" . "%-20s  ")
                                       ((("tree" . "%s")
                                         ("subject" . "%s"))
                                        . " %-85.85s  ")
                                       ("tags" . "(%s)"))
          notmuch-tree-outline-enabled nil)

  ;; Email composition
  (with-eval-after-load 'notmuch-mua
    (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)) ; See also `notmuch-mua-attachment-regexp'
  (setopt notmuch-mua-compose-in 'current-window
          notmuch-mua-hidden-headers nil
          notmuch-address-command 'internal
          notmuch-address-internal-completion '(sent nil)
          notmuch-always-prompt-for-sender t  ; See also the `notmuch-mua-prompt-for-sender' function
          notmuch-mua-cite-function 'message-cite-original-without-signature
          notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never
          notmuch-mua-user-agent-function nil
          notmuch-maildir-use-notmuch-insert t
          notmuch-wash-citation-lines-prefix 0
          notmuch-wash-citation-lines-suffix 0
          notmuch-crypto-process-mime t
          notmuch-crypto-get-keys-asynchronously t
          ;; See `notmuch-mua-send-hook'
          notmuch-mua-attachment-regexp (concat "\\b\\("
                                                "attache\?ment\\|attached\\|attach\\|"
                                                "pi[èe]ce\s+jointe?"
                                                "\\)\\b"))

  ;; Sending emails.
  ;; Use Lieer to send emails.  Also see
  ;; `krisb-notmuch-set-sendmail-args'.  Read
  ;; https://github.com/gauteh/lieer/wiki/Emacs-and-Lieer.
  (with-eval-after-load 'sendmail
    (setopt sendmail-program (if (executable-find "gmi") "gmi" "sendmail")
            send-mail-function 'sendmail-send-it))
  (setopt notmuch-fcc-dirs nil) ; Gmail already copies sent emails, so don't move them elsewhere locally

  ;; TODO 2025-05-23: Revisit this.
  ;; (krisb-modus-themes-setup-faces
  ;;  "notmuch"
  ;;  ;; More noticeable demarcation of emails in thread in notmuch-show-mode
  ;;  (set-face-attribute 'notmuch-message-summary-face nil
  ;;                      :foreground fg-alt
  ;;                      ;; NOTE 2024-09-26: We do it this way since changing
  ;;                      ;; faces will refresh the font to be 1.1 times the 1.1
  ;;                      ;; times height, and so on
  ;;                      :height (truncate (* (face-attribute 'default :height nil) 1.1))
  ;;                      :overline t
  ;;                      :extend nil
  ;;                      :inherit 'unspecified)
  ;;  (set-face-attribute 'notmuch-tag-added nil
  ;;                      :underline `(:color ,cyan-cooler :style double-line :position t))
  ;;  (add-to-list 'notmuch-tag-formats
  ;;               `("correspondence" (propertize tag 'face '(:foreground ,green-faint))))
  ;;  (add-to-list 'notmuch-tag-formats
  ;;               `("commitment" (propertize tag 'face '(:foreground ,yellow-faint)))))

  ;; Don't buttonize citations
  ;; FIXME 2024-10-07: For some reason putting this in :custom and setting it to
  ;; a high value doesn't work, so I put it here
  (setq notmuch-wash-citation-lines-prefix most-positive-fixnum
        notmuch-wash-citation-lines-suffix most-positive-fixnum)

  ;; Set sendmail args appropriate to using lieer as
  ;; `sendmail-program'
  (defun krisb-notmuch-set-sendmail-args ()
    "Set `message-sendmail-extra-arguments' arguments.
Set `message-sendmail-extra-arguments' accordingly (changing the
maildir) such that lieer can properly send the email. (This assumes
`sendmail-program' is set to the gmi executable.) Instruction from
https://github.com/gauteh/lieer/wiki/Emacs-and-Lieer."
    (when (and (stringp sendmail-program) (string-match-p "gmi" sendmail-program))
      (let* ((from (downcase (message-fetch-field "from")))
             (root-maildir krisb-email-directory)
             ;; These maildirs are according to the structure in my
             ;; local filesystem
             (personal-maildir (expand-file-name "personal" root-maildir))
             (uni-maildir (expand-file-name "uni" root-maildir)))
        (cond
         ((string-match-p (rx (literal "krisbalintona@gmail.com")) from)
          (setq-local message-sendmail-extra-arguments `("send" "--quiet" "-t" "-C" ,personal-maildir)))
         ((string-match-p (rx (literal "kristoffer_balintona@alumni.brown.edu")) from)
          (setq-local message-sendmail-extra-arguments `("send" "--quiet" "-t" "-C" ,uni-maildir)))))))
  (with-eval-after-load 'message
    (add-hook 'message-send-mail-hook #'krisb-notmuch-set-sendmail-args))

  ;; TODO 2025-05-23: Revisit this.
  ;; REVIEW 2024-09-26: Prot's lin package apparently makes disabling
  ;; this better?
  (with-eval-after-load 'lin
    (remove-hook 'notmuch-search-hook #'notmuch-hl-line-mode))

  ;; TODO 2025-05-23: Revisit this.
  ;; Prefer not to have emails recentered as I readjust them
  (advice-add 'notmuch-show-message-adjust :override #'ignore))

;; Set `display-buffer-alist'
(setup notmuch
  ;; Add to `display-buffer-alist'
  (add-to-list 'display-buffer-alist
               '("\\*notmuch-hello\\*"
                 (display-buffer-in-tab display-buffer-full-frame)
                 (tab-group . "media"))))

;; Mode line indicator for notmuch emails
(setup notmuch
  ;; Try using display-time's built-in email indicator --- less
  ;; informative but more visually subtle than `notmuch-indicator'.
  ;; Obviously the below applies only when `display-time-mode' is
  ;; non-nil.
  ;; 
  ;; We do this by: modifying `display-time-mail-string’ such that it
  ;; displays a neat unicode icon for mail.  Then modify
  ;; `display-time-mail-function’ such that it returns non-nil when
  ;; there is more than one notmuch email available that is worth
  ;; notifying my about. (Note: we use `display-time-mail-string’
  ;; instead of `display-time-use-mail-icon’ because the latter is for
  ;; xpm and pbm files; see the final form in
  ;; `display-time-string-forms’.)
  (require 'time)
  
  (defvar krisb-display-time-mail-string
    (cond
     ((require 'nerd-icons nil t)
      (propertize (nerd-icons-mdicon "nf-md-email")
                  'face `(:family ,(nerd-icons-mdicon-family) :height 1.1)
                  'display '(raise 0.05)))
     ((require 'all-the-icons nil t)
      (propertize (all-the-icons-material "mail_outline")
                  'face `(:family ,(all-the-icons-material-family) :height 1.1)
                  'display '(raise -0.1)))
     (t "🞷 "))
    "Icon I use for displaying mail in `display-time-string-forms'.")
  
  (defun krisb-display-time-mail-notmuch-function ()
    "Function for `display-time-mail-function'.
  Returns non-nil when there is mail."
    (when-let* ((executable-find "notmuch")
                (command
                 (format "notmuch search tag:inbox and tag:unread and not tag:list and not tag:sub | wc -l"))
                (mail-count (string-to-number (shell-command-to-string command))))
      (< 0 mail-count)))
  
  ;; TODO 2025-05-26: Setting `read-mail-command’ to `notmuch’ doesn’t
  ;; seem to succeed as the command run when clicking the mode line mail
  ;; string?
  (setopt display-time-mail-string krisb-display-time-mail-string
          display-time-mail-function 'krisb-display-time-mail-notmuch-function)
  (with-eval-after-load 'notmuch
    (setopt display-time-mail-face 'notmuch-search-flagged-face))
  
  ;; FIXME 2025-05-26: This assumes that we always leave notmuch via the
  ;; notmuch-hello buffer.  This is a workaround because I know of no
  ;; other reliable indiaction of when I’m done checking mail.  Is there
  ;; something better?
  ;; Advise `notmuch-bury-or-kill-this-buffer’ such that it updates
  ;; after leaving the notmuch-hello buffer.  This prevents the mail
  ;; string from being visible right after we’ve just checked mail in
  ;; notmuch
  (with-eval-after-load 'notmuch
    (advice-add 'notmuch-bury-or-kill-this-buffer :around
                (lambda (&rest args)
                  (when (equal major-mode 'notmuch-hello-mode)
                    (display-time-update))
                  (apply args)))))

;;; Notmuch-addr
;; Better email address completion for notmuch.  Replaces the built-in
;; `notmuch-address' completion system.  See
;; https://nmbug.notmuchmail.org/nmweb/show/20201108231150.5419-1-jonas%40bernoul.li
;; for more information
(setup notmuch-addr
  (:package notmuch-addr)
  
  (with-eval-after-load 'notmuch-address
    (notmuch-addr-setup)))

;;; Ol-notmuch
;; Org-links for search queries (i.e. notmuch-search-mode,
;; notmuch-tree-mode) and messages (i.e. notmuch-show-mode).
(setup ol-notmuch
  (:package ol-notmuch)
  
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 `("e" "Email" entry
                   (file ,(expand-file-name "todo.org" krisb-org-agenda-directory))
                   "* TODO %? [[%L][\"%:subject\"]] :email:\n\nFrom %:from\nTo: %:to\n"
                   :empty-lines 1)
                 'append)
    (add-to-list 'org-capture-templates
                 `("n" "Review newsletter/subscription email" entry
                   (file ,krisb-org-agenda-main-file)
                   "* TODO [#E] Review subscription/newsletter email: [[%L][\"%:subject\"]] %? :email:inbox:%^g\n\nFrom %:from\nTo: %:to\n"
                   :immediate-finish t
                   :empty-lines 1)
                 'append)

    ;; Using `dolist' or `cl-loop' will not work as expected... you'll need to
    ;; (copy-sequence ...) the shared objects, making those forms not elegant
    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "notmuch-tree-mode"))))
    (add-to-list 'org-capture-templates-contexts '("n" ((in-mode . "notmuch-tree-mode"))))
    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "notmuch-search-mode"))))
    (add-to-list 'org-capture-templates-contexts '("n" ((in-mode . "notmuch-search-mode"))))
    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "notmuch-show-mode"))))
    (add-to-list 'org-capture-templates-contexts '("n" ((in-mode . "notmuch-show-mode")))))

  ;; Modify to my liking how `org-notmuch-store-link' behaves
  (el-patch-defun org-notmuch-store-link ()
    (el-patch-swap
      "Store a link to one or more notmuch messages."
      "Store a link to one or more notmuch messages.
My version allows for linking to the first message in an email thread
from a `notmuch-search-mode' buffer.")
    ;; 2025-04-09: Not sure what the most elegant el-patch directives would
    ;; be, so I just remove then add.
    (el-patch-remove
      (when (memq major-mode '(notmuch-show-mode notmuch-tree-mode))
        ;; The value is passed around using variable `org-store-link-plist'.
        (org-link-store-props
         :type       "notmuch"
         :message-id (notmuch-show-get-message-id t)
         :subject    (notmuch-show-get-subject)
         :from       (notmuch-show-get-from)
         :to         (notmuch-show-get-to)
         :date       (org-trim (notmuch-show-get-date)))
        (org-link-add-props :link (org-link-email-description "notmuch:id:%m"))
        (org-link-add-props :description (org-link-email-description))
        org-store-link-plist))
    (el-patch-add
      (cond
       ((memq major-mode '(notmuch-show-mode notmuch-tree-mode))
        ;; The value is passed around using variable `org-store-link-plist'.
        (org-link-store-props
         :type       "notmuch"
         :message-id (notmuch-show-get-message-id t)
         :subject    (notmuch-show-get-subject)
         :from       (notmuch-show-get-from)
         :to         (notmuch-show-get-to)
         :date       (org-trim (notmuch-show-get-date)))
        (org-link-add-props :link (org-link-email-description "notmuch:id:%m"))
        (org-link-add-props :description (org-link-email-description))
        org-store-link-plist)
       ((equal major-mode 'notmuch-search-mode)
        (save-window-excursion
          (let ((buf (notmuch-show (notmuch-search-find-thread-id))))
            (with-current-buffer buf
              (org-link-store-props
               :type       "notmuch"
               :message-id (notmuch-show-get-message-id t)
               :subject    (notmuch-show-get-subject)
               :from       (notmuch-show-get-from)
               :to         (notmuch-show-get-to)
               :date       (org-trim (notmuch-show-get-date)))
              (org-link-add-props :link (org-link-email-description "notmuch:id:%m"))
              (org-link-add-props :description (org-link-email-description)))
            (kill-buffer buf)
            org-store-link-plist)))))))

;;; Abbrev
;; Automatically correct typed strings (e.g. words).  Most useful for
;; correcting spelling mistakes as they are made.
(setup abbrev
  
  (:hide-mode)
  
  (setopt save-abbrevs 'silently
          abbrev-suggest t
          abbrev-suggest-hint-threshold 2)

  ;; Enable the mode globally
  (setopt abbrev-mode t))

;;; Tempel
;; Like tempo.el but updated to modern standards.
(setup tempel
  (:package tempel)
  
  (bind-keys ("M-*" . tempel-insert))
  
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'krisb-tempel-setup-capf))
  
  ;; Applies to `tempel-expand' and `tempel-complete'.  We prefer
  ;; non-pair characters to avoid inserting an extra pair from
  ;; `electric-pair-mode'.  If set, it should be an unused (or at
  ;; least very rarely used) comment delimiter to avoid indenting the
  ;; line when pressing the TAB key and with `tab-always-indent' set
  ;; to \\='complete.  If this is set to nil, then template names
  ;; should not be ambiguous, otherwise trying to complete other
  ;; symbol names will get hijacked by completing for tempel templates
  ;; (assuming the tempel `completion-at-point’ functions are set).
  (setopt tempel-trigger-prefix nil)
  
  ;; Element that expands other templates by name.  E.g., (i header)
  ;; expands the template named "header."
  (with-eval-after-load 'tempel
    (defun krisb-tempel-include (elt)
      (when (eq (car-safe elt) 'include)
        (if-let (template (alist-get (cadr elt) (tempel--templates)))
            (cons 'l template)
          (message "Template %s not found" (cadr elt))
          nil)))
    (add-to-list 'tempel-user-elements #'krisb-tempel-include))

  ;; Set up with `completion-at-point-functions'
  (defun krisb-tempel-setup-capf ()
    "Add `tempel-expand' to the beginning of local `completion-at-point-functions'.
We also add `tempel-expand' to the beginning of the global value for
`completion-at-point-functions'.  The difference here is that we want
`tempel-expand' to be the first `completion-at-point' function for the
buffers in which this function is run."
    (add-hook 'completion-at-point-functions 'tempel-expand -90 t))
  ;; Place `tempel-complete' at the beginning of the fallback (global
  ;; value) `completion-at-point-functions'
  (add-hook 'completion-at-point-functions #'tempel-complete -90))

;;; Jinx
;; JIT spell checker that uses Enchant.  The executable is enchant-2.
;; See the Enchant manual for more information about the
;; spell-checker's features and using it:
;; https://abiword.github.io/enchant/src/enchant.html.
(setup jinx
  ;; NOTE 2025-11-10: Disable for now. Trying out ispell + flyspell
  (:quit)
  
  ;; Installed via Guix because it needs to compile a C module
  ;; For AUR:
  ;; :ensure-system-package ((enchant-2 . enchant)
  ;;                         (pkgconf)
  ;;                         ;; Don't forget to install spell checker libraries!
  ;;                         (hunspell)
  ;;                         ("/usr/share/hunspell/en_US-large.dic" . hunspell-en_us)
  ;;                         (hspell)      ; Hebrew
  ;;                         (nuspell) ; Newest spell checker to be used by Firefox, Thunderbird, etc.
  ;;                         (voikkospell . libvoikko)) ; Finnish
  
  (add-hook 'on-first-buffer-hook #'global-jinx-mode)
  
  (:hide-mode)
  
  (:bind-keys :map jinx-mode-map
              ([remap ispell-word] . jinx-correct)
              ("C-," . jinx-correct)
              ("C-M-$" . jinx-languages))
  
  (setopt jinx-delay 0.7)

  (with-eval-after-load 'jinx
    ;; Mimic `flyspell-abbrev-p'.  Taken from
    ;; https://github.com/minad/jinx/wiki#save-misspelling-and-correction-as-abbreviation
    (defun krisb-jinx--add-to-abbrev (overlay word)
      "Add abbreviation to `local-abbrev-table'.
The misspelled word is taken from OVERLAY.  WORD is the corrected word."
      (let ((abbrev (buffer-substring-no-properties
                     (overlay-start overlay)
                     (overlay-end overlay))))
        (message "Abbrev: %s -> %s" abbrev word)
        ;; Change this to `global-abbrev-table' if preferred
        (define-abbrev local-abbrev-table abbrev word)))
    (advice-add 'jinx--correct-replace :before #'krisb-jinx--add-to-abbrev)

    ;; Read Ispell's "LocalWords."  Taken from
    ;; https://github.com/minad/jinx/wiki#make-jinx-read-from-localwords
    (defun krisb-jinx-ispell--get-localwords ()
      "Return a string of ispell's local words.
Those are the words following `ispell-words-keyword' (usually
\"LocalWords\") in the current buffer."
      (require 'ispell)
      (save-excursion
        (goto-char (point-min))
        (cl-loop while (search-forward ispell-words-keyword nil t)
                 collect (string-trim (buffer-substring-no-properties (point) (line-end-position))) into result
                 finally return (mapconcat #'identity result " "))))
    (defun krisb-jinx-ispell-add-localwords ()
      "Add ispell's local words to `jinx-local-words'."
      (let ((ispell-localwords (krisb-jinx-ispell--get-localwords)))
        (setq jinx-local-words (concat jinx-local-words ispell-localwords))
        (setq jinx--session-words (append jinx--session-words (split-string ispell-localwords)))))
    (add-hook 'jinx-mode-hook #'krisb-jinx-ispell-add-localwords)

    ;; Write to buffer's LocalWords instead of populating
    ;; `jinx-local-words', a local variable.  Taken from
    ;; https://github.com/minad/jinx/wiki#make-jinx-write-localwords
    (defun krisb-jinx-save-as-ispell-localword (save key word)
      "Save WORD using ispell's `ispell-words-keyword'.
If SAVE is non-nil save, otherwise format candidate given action KEY."
      (if save
          (progn
            (require 'ispell)
            (ispell-add-per-file-word-list word)
            (add-to-list 'jinx--session-words word)
            (setq jinx-local-words
                  (string-join
                   (sort (delete-dups
                          (cons word (split-string jinx-local-words)))
                         #'string<)
                   " "))))
      (list key word "File (LocalWords)"))
    ;; NOTE 2023-07-16: Can also directly add to `jinx--save-keys'
    ;; directly
    (setf (alist-get ?* jinx--save-keys) #'krisb-jinx-save-as-ispell-localword)))

;;; Orgmdb
;; Use the OMdb API to populate org headings with IMDB information on
;; movies, shows, and episodes.
(setup orgmdb
  (:package orgmdb)
  
  ;; FIXME 2025-10-06: Should I obfuscate my API key?  There's a limit
  ;; of 1000 requests per day.
  (with-eval-after-load 'orgmdb
    (setopt orgmdb-omdb-apikey "8ab4d64e"
            orgmdb-show-tag "show"
            orgmdb-type-prop "CATEGORY"
            orgmdb-fill-property-list
            '( imdb-link
               genre director writer actors box-office
               tomatometer metascore metacritic imdb-rating))))

;;; Lin
;; Lin is a stylistic enhancement for Emacs' built-in `hl-line-mode'.
;; It remaps the `hl-line' face (or equivalent) buffer-locally to a
;; style that is optimal for major modes where line selection is the
;; primary mode of interaction.
(setup lin
  (:package lin)
  
  (lin-global-mode 1)

  (with-eval-after-load 'lin
    (setopt lin-face 'lin-cyan)
    (add-to-list 'lin-mode-hooks 'LaTeX-mode-hook)))

;;; Org-mime
;; TODO 2025-10-28: Document:
;; - `org-mime-debug'
(setup org-mime
  (:package (org-mime :url "https://github.com/krisbalintona/org-mime.git"
                      :branch "merge"))

  (with-eval-after-load 'message
    (autoload 'org-mime-edit-mail-in-org-mode "org-mime")
    (bind-keys :map message-mode-map
               ("C-c M-o" . org-mime-htmlize)
               ("C-c '" . org-mime-edit-mail-in-org-mode))
    
    (autoload 'org-mime-confirm-when-no-multipart "org-mime")
    (add-hook 'message-send-hook #'org-mime-confirm-when-no-multipart))

  (with-eval-after-load 'org-mime
    (setopt ;; org-mime-export-ascii 'utf-8
            org-mime-preserve-breaks nil
            org-mime-export-options '( :with-latex imagemagick
                                       :section-numbers nil
                                       :with-author nil
                                       :with-toc nil
                                       :with-title nil)
            ;; Keep GPG signatures outside of multipart.  Modified
            ;; version of
            ;; https://github.com/org-mime/org-mime?tab=readme-ov-file#keep-gpg-signatures-outside-of-multipart
            org-mime-find-html-start
            (lambda (start)
              (save-excursion
                (goto-char start)
                (if (search-forward "<#secure method=pgpmime mode=sign>" nil t)
                    (1+ (point))
                  start))))

    (defun krisb-org-mime-setup ()
      "Nicely offset block quotes in email bodies.
Taken from
https://github.com/org-mime/org-mime?tab=readme-ov-file#css-style-customization."
      (org-mime-change-element-style
       "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))
    (add-hook 'org-mime-html-hook #'krisb-org-mime-setup)))

;;; Astute
;; Display punctuation typographically (e.g., em-dashes as "—" and
;; en-dashes as "–")
(setup astute
  (:package astute)
  
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'astute-mode))
  
  (with-eval-after-load 'astute
    (setopt astute-lighter " As"
            astute-prefix-single-quote-exceptions
            '("bout"
              "em"
              "n'"
              "cause"
              "round"
              "twas"
              "tis"))))

;;; Dictionary
;; See definitions of words from an online dictionary.
;; TODO 2025-05-23: Document these options:
;; - `dictionary-create-buttons’
;; - `dictionary-read-word-function’
;; - `dictionary-search-interface’
;; - `dictionary-server'
(setup dictionary
  :ensure nil
  ;; Don't forget to install the following packages from the AUR:
  ;; paru -S dict-wn dict-gcide dict-moby-thesaurus dict-foldoc
  ;; :ensure-system-package (dict . dictd) ; Localhost (offline). Don't forget to enable the systemd service

  (with-eval-after-load 'dictionary
    (setopt dictionary-use-single-buffer t
            dictionary-read-dictionary-function 'dictionary-completing-read-dictionary)

    (when (package-installed-p 'hide-mode-line)
      (add-hook 'dictionary-mode-hook #'hide-mode-line-mode)))
  
  (defun krisb-dictionary-dwim (promptp)
    "Show dictionary definition for word at point.
If region is active, use the region's contents instead.

If PROMPTP is non-nil, prompt for a word to find the definition of
instead."
    (interactive "P")
    (if-let ((word (cond
                    (promptp (read-string "Define: "))
                    ((use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))
                    (t (thing-at-point 'word :no-properties)))))
        (dictionary-search word)
      (message "No word or region selected.")))
  (bind-keys ("C-h =" . krisb-dictionary-dwim)))

(setup embark
  (:bind-keys :map embark-region-map
              ("=" . krisb-dictionary-dwim)
              :map embark-identifier-map
              ("=" . krisb-dictionary-dwim)))

;;; Powerthesaurus
;; Search for synonyms using an online thesaurus.
(setup powerthesaurus
  (:package powerthesaurus)
  
  (with-eval-after-load 'embark
    (bind-keys :map embark-region-map
               ("t" . powerthesaurus-lookup-synonyms-dwim)
               ("T" . powerthesaurus-lookup-dwim)
               :map embark-identifier-map
               ("t" . powerthesaurus-lookup-synonyms-dwim)
               ("T" . powerthesaurus-lookup-dwim))))

;;; Wombag
(setup wombag
  ;; Elpaca:
  ;; :ensure ( :repo "https://github.com/krisbalintona/wombag.git"
  ;;           :branch "merge")
  (:package (wombag :url "https://github.com/krisbalintona/wombag.git"
                    :branch "merge"))
  
  (bind-keys :map krisb-open-keymap
             ("w" . wombag))
  
  (setopt wombag-dir (no-littering-expand-var-file-name "wombag")
          wombag-db-file (no-littering-expand-var-file-name "wombag/wombag.sqlite")
          wombag-username "krisbalintona"
          wombag-host "https://app.wallabag.it"
          wombag-password (auth-source-pick-first-password :host "app.wallabag.it")
          wombag-client-id "23882_1jzdzdd09ikgw4k8o0cog4wggk48cgc0gwk8oos0gsc44gcsco"
          wombag-client-secret (auth-source-pick-first-password :host "emacs-wombag.el")
          wombag-search-filter "")
  
  (with-eval-after-load 'wombag
    (defun krisb-wombag-entry-setup ()
      "Set up the visual for wombag-entry buffers."
      (setq-local line-spacing 0.08)
      (face-remap-add-relative 'default :height 1.1)
      (when (require 'olivetti nil t)
        (olivetti-mode 1)
        (olivetti-set-width 120))
      (when (require 'mixed-pitch nil t)
        (mixed-pitch-mode 1))
      (visual-line-mode 1))
    (add-hook 'wombag-show-mode-hook #'krisb-wombag-entry-setup)))

(setup wombag
  (with-eval-after-load 'wombag
    (when (package-installed-p 'org-remark)
      (add-hook 'wombag-show-mode-hook #'org-remark-mode))))

;;; Display-line-numbers
;; Show line numbers on the left fringe
(setup display-line-numbers
  
  (bind-keys :map krisb-toggle-keymap
             ("l" . display-line-numbers-mode))
  
  (setopt display-line-numbers-type t
          display-line-numbers-width-start t)) ; Use same width throughout

;;; Ispell
(setup ispell

  (with-eval-after-load 'ispell
    ;; TODO 2025-11-09: Ensure that the enchant system package is
    ;; installed, too
    (setopt ispell-program-name (or (executable-find "enchant-2")
                                    (executable-find "aspell"))
            ispell-help-in-bufferp 'electric
            ispell-dictionary "en_US"
            ispell-complete-word-dict
            (when (string-match-p "enchant" ispell-program-name)
              (require 'xdg)
              (expand-file-name (format "enchant/%s.dic" ispell-dictionary) (xdg-config-home))))))

;;; Flyspell
(setup flyspell
  
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'conf-mode-hook #'flyspell-prog-mode)

  (:hide-mode)
  
  (with-eval-after-load 'flyspell
    (setopt flyspell-issue-welcome-flag nil
            flyspell-issue-message-flag nil ; Greatly increase `flyspell-buffer' speed
            flyspell-abbrev-p t
            flyspell-check-changes t
            flyspell-delay-use-timer t)) ; Emacs 31.1
            
  (:bind-keys :map flyspell-mode-map
              ("C-;" . nil)
              ("C-." . nil)
              ("C-M-i" . nil)))

;;; Xref
;; TODO 2025-05-22: Document:
;; - `xref-file-name-display’
(setup xref
  
  (bind-keys ("C-M-?". xref-find-references-and-replace)) ; Emacs 29.1
  
  (with-eval-after-load 'xref
    (setopt xref-search-program (if (executable-find "rg") 'ripgrep 'grep)
            xref-history-storage 'xref-window-local-history)) ; Per-window history of `xref-go-*'
  
  (with-eval-after-load 'xref
    ;; We remove the default sole fallback backend, `etags--xref-backend',
    ;; which prompts the user for an etags table -- this is undesirable
    ;; for me, since I don't use etags.
    (remove-hook 'xref-backend-functions #'etags--xref-backend)
    ;; Then add `elisp--xref-backend' as the global value of
    ;; `xref-backend-functions', which means it is run when the local
    ;; value ends with `t'.  (See (info "(elisp) Running Hooks") for an
    ;; explanation of why.)
    ;; 
    ;; Effectively, when combined with the above, this becomes the (only)
    ;; backend function that is checked after all the buffer-local backend
    ;; functions are checked.
    (add-hook 'xref-backend-functions #'elisp--xref-backend))

  (with-eval-after-load 'xref
    ;; TODO 2025-05-22: Revisit this.
    ;; (setopt xref-show-definitions-function 'xref-show-definitions-completing-read
    ;;         xref-show-xrefs-function 'xref-show-definitions-buffer)
    (add-to-list 'display-buffer-alist
                 `((or (major-mode . xref--xref-buffer-mode)
                       (,(rx (literal xref-buffer-name))))
                   (display-buffer-below-selected display-buffer-at-bottom)
                   (window-height . 0.25)))
    (add-to-list 'display-buffer-alist
                 '(((category . xref)
                    (display-buffer-reuse-window display-buffer-use-some-window)
                    (some-window . mru))))))

(setup xref
  ;; TODO 2025-05-22: Revisit this.
  ;; ;; Revealing headings
  ;;   (with-eval-after-load 'krisb-reveal
  ;;     (defun krisb-reveal-xref-find-information ()
  ;;       "Return information required by `krisb-reveal-fold-commands'.
  ;; See the docstring of `krisb-reveal-fold-commands'."
  ;;       (save-window-excursion
  ;;         (save-excursion
  ;;           (xref-goto-xref)
  ;;           (cons (point) (current-buffer)))))
  ;;     ;; I could also advise the following commands to call
  ;;     ;; `xref-show-location-at-point' afterwards.  Though such a solution is
  ;;     ;; applicable only to xref.  I wanted similar functionality for non-xref
  ;;     ;; buffers, so I wrote krisb-reveal, and to remain idiomatic with my usage
  ;;     ;; of it, I also do it here.
  ;;     (dolist (command '(xref-prev-line
  ;;                        xref-next-line
  ;;                        xref-quit-and-goto-xref))
  ;;       (add-to-list 'krisb-reveal-fold-commands
  ;;                    (list :command command
  ;;                          :location #'krisb-reveal-xref-find-information)))
  ;;     (add-hook 'xref-after-jump-hook #'krisb-reveal-fold))
  )

;;; Cape
(setup cape
  (:package cape)

  (bind-keys ("C-c p" . cape-prefix-map))
  
  (with-eval-after-load 'ispell
    (setopt cape-dict-file (list ispell-complete-word-dict))))

;; Add cape functions to `completion-at-point-functions'
(setup cape
  ;; For Emacs >30, disable the ispell capf automatically added to
  ;; text-mode buffers.  Recommended in
  ;; https://github.com/minad/corfu?tab=readme-ov-file#configuration.
  ;; We use `cape-dict' as an alternative, adding it where needed
  ;; (below).
  (setopt text-mode-ispell-word-completion nil)
  
  (defun krisb-cape-super-capf--dict-dabbrev ()
    "Super-capf of `cape-dict' and `cape-dabbrev'."
    (cape-wrap-super 'cape-dict :with 'cape-dabbrev))

  ;; Capfs added to the end of the global value of
  ;; `completion-at-point-functions'.  Consequently, they act as
  ;; fallback backends.
  (dolist (capf (reverse '(krisb-cape-super-capf--dict-dabbrev)))
    (add-hook 'completion-at-point-functions capf 100))
  
  ;; Macro to help adding capfs via major mode hooks
  (defmacro krisb-cape-setup-capfs (label hooks capfs)
    "Set up `completion-at-point-functions' for HOOKS.
CAPFS are a list of `completion-at-point-functions'. Adds CAPFS when a
hook in HOOKS is run. These effects are added by a defined function with
LABEL appended to `krisb-cape-setup-capfs-'.

The order of elements in CAPFS are the order they will appear in
`completion-at-point-functions' for that buffer. That is, the first
element in CAPFS will be the first element in
`completion-at-point-functions'.

This macro does not affect capfs already in
`completion-at-point-functions' nor how later capfs are added to
`completion-at-point-functions'."
    (declare (indent 0))
    `(dolist (hook ,hooks)
       (add-hook hook
                 (defun ,(intern (concat "krisb-cape-setup-capfs-" label)) ()
                   (dolist (capf (reverse ,capfs))
                     (add-hook 'completion-at-point-functions capf -50 t))))))

  ;; Add capfs by major-mode
  (krisb-cape-setup-capfs
    "elisp"
    '(emacs-lisp-mode-hook lisp-interaction-mode-hook)
    (list #'cape-file #'cape-elisp-symbol))
  
  (krisb-cape-setup-capfs
    "commit"
    '(git-commit-setup-hook log-edit-mode-hook)
    (list #'cape-elisp-symbol))

  (krisb-cape-setup-capfs
    "shells"
    '(eshell-mode-hook comint-mode-hook)
    (list #'cape-file #'cape-history)))

;; Make eglot's capf non-exclusive
(setup cape
  (with-eval-after-load 'cape
    (with-eval-after-load 'eglot
      (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive))))

;; Resolve the undesirable behavior of `cape-elisp-symbol' and friends
;; with occupying the *Help* buffer, as described here:
;; https://github.com/minad/corfu/discussions/504#discussioncomment-12592545.
;; This solution was taken from the suggestion of
;; https://github.com/minad/corfu/discussions/504#discussioncomment-12593463
(setup cape
  (with-eval-after-load 'corfu
    (defun krisb-cape-corfu-popupinfo--doc-buffer (str)
      "Wrapper around `elisp--company-doc-buffer'.
This function is a replacement for `elisp--company-doc-buffer', which
normally returns the main Help buffer (returned by `help-buffer').
Instead, this function returns a separate buffer to use as the Help
buffer.

Accepts the same argument as `elisp--company-doc-buffer' (STR).

Meant to be used with `cape-capf-properties' on the `cape-elisp-symbol'
completion at point function."
      (let* ((help-xref-following t)
             (new-help-buf-name
              "*corfu-popupinfo documentation*")
             (new-help-buf (get-buffer-create new-help-buf-name)))
        (with-current-buffer new-help-buf
          (help-mode)
          (elisp--company-doc-buffer str))))

    (defun krisb-cape-elisp--around-advice (orig-fun &rest _args)
      "Advice to use a different doc buffer for documentation."
      (cape-wrap-properties orig-fun :company-doc-buffer #'krisb-cape-corfu-popupinfo--doc-buffer))

    (dolist (capf '(cape-elisp-symbol elisp-completion-at-point))
      (advice-add capf :around #'krisb-cape-elisp--around-advice))))

;;; Project
;; TODO 2025-05-22: Document:
;; - `project-vc-extra-root-markers’
(setup project

  (with-eval-after-load 'project
    (setopt project-file-history-behavior 'relativize
            ;; The commands in `project-switch-commands' must be found
            ;; in `project-prefix-map'
            project-switch-commands
            `((project-find-file "Find file")
              (project-switch-to-buffer "Switch to buffer")
              (project-find-regexp "Find regexp")
              (project-find-dir "Find directory")
              (project-vc-dir "VC-Dir")
              (project-eshell "Eshell")
              (project-shell "Shell")
              (project-any-command "Other"))
            project-compilation-buffer-name-function 'project-prefixed-buffer-name
            project-vc-merge-submodules nil ; Respect subprojects as their own projects
            project-mode-line t
            project-mode-line-face 'italic))

  (with-eval-after-load 'project
    (bind-keys :map project-prefix-map
               ("e" . project-eshell)
               ("C" . project-recompile))
    (add-to-list 'project-switch-commands '(project-compile "Compile"))
    (add-to-list 'project-switch-commands '(project-recompile "Recompile"))))

;;; Info
;; TODO 2025-06-16: Document:
;; - `Info-hide-note-references’
(setup info
  
  (with-eval-after-load 'info
    (setup mixed-pitch-mode
      (:if-package mixed-pitch-mode)
      ;; TODO 2025-11-15: Revisit whether I want `mixed-pitch-mode'
      ;; enabled
      (:quit)
      
      (add-hook 'Info-selection-hook #'mixed-pitch-mode)))
  
  (with-eval-after-load 'info
    (setopt Info-isearch-search nil))
  
  ;; Adjust font sizes
  (with-eval-after-load 'info
    (defun krisb-info-font-resize ()
      "Increase the font size of text in Info buffers."
      (when (bound-and-true-p mixed-pitch-mode)
        (face-remap-set-base 'default '(:height 1.2))))
    (add-hook 'Info-selection-hook #'krisb-info-font-resize)

    (set-face-attribute 'info-title-1 nil :height 1.4)
    (set-face-attribute 'info-title-2 nil :height 1.3)
    (set-face-attribute 'info-title-3 nil :height 1.2)
    (set-face-attribute 'info-title-4 nil :height 1.1)))

;;; Fish-mode
(setup fish-mode
  (:package fish-mode))

;;; Backup
;; TODO 2025-05-22: Document:
;; - `dired-kept-versions’
;; - `dired-backup-overwrite'
;; Backup files: "Emacs makes a backup for a file only the first time
;; the file is saved from the buffer that visits it."
(setup files
  
  (setopt make-backup-files t   ; See docstring for useful information
          backup-by-copying t) ; Always copy; see (info "(emacs) Backup Copying")
  (setup vc
    (setopt vc-make-backup-files t))
  
  ;; Numbering backups
  (setopt version-control t
          ;; Be generous: file space is cheap
          kept-new-versions 10
          kept-old-versions 3
          delete-old-versions t)
  
  ;; REVIEW 2025-11-15: Consider not hashing file names, instead
  ;; having a directory-tree structure (with a non-hashed file name)
  ;; instead the backup directory (corresponding with the current
  ;; file).  This is what Ihor does in his config, setting
  ;; `make-backup-file-name-function' instead of advising
  ;; `make-backup-file-name-1':
  ;; https://github.com/yantar92/emacs-config.  Doing so in a way that
  ;; respects `backup-directory-alist' and other built-in
  ;; functionality is complicated though I think... see see
  ;; `make-backup-file-name-1' and the usage of
  ;; ``make-backup-file-name-1' in `find-backup-file-name'.
  ;; 
  ;; REVIEW 2025-11-15: Does this work with `file-name-sans-versions',
  ;; as the docstring of `make-backup-file-name-function' instructs it
  ;; should?
  ;; 
  ;; Modified from Doom Emacs.  Backup files have names that are
  ;; hashed.
  (defun krisb-backup-file-name-hash (fn file)
    "Hash the backup file name.
Takes any FILE and return a hashed version.

This is necessary when the user has very long file names since some
systems, including Linux, have a maximum for the number of bytes a file
name occupies.  With this method, we ensure backup file names are an
acceptable length while still being unique.  The only potential downside
is that outside of Emacs, the backup file name alone does not indicate
which file on the system it backs up."
    (let ((alist backup-directory-alist)
          backup-directory)
      (while alist
        (let ((elt (car alist)))
          (if (string-match (car elt) file)
              (setq backup-directory (cdr elt)
                    alist nil)
            (setq alist (cdr alist)))))
      (let ((file (funcall fn file)))
        (if (or (null backup-directory)
                (not (file-name-absolute-p backup-directory)))
            file
          (expand-file-name (sha1 (file-name-nondirectory file))
                            (file-name-directory file))))))
  (advice-add 'make-backup-file-name-1 :around #'krisb-backup-file-name-hash))

;;; Startup time
;; Message for total init time after startup
(defun krisb-startup-time ()
  "Report Emacs startup time."
  (message "Total startup time: %s" (emacs-init-time)))
(add-hook 'after-init-hook #'krisb-startup-time)
