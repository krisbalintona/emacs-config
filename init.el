;; -*- lexical-binding: t; -*-



(setopt user-full-name "Kristoffer Balintona"
        user-mail-address "krisbalintona@gmail.com")

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

(defvar krisb-org-archive-directory (expand-file-name "archive" krisb-folio-directory)
  "The archive directory for my org files.")

(defvar krisb-bibliography-files (list (expand-file-name "master-lib.bib" krisb-folio-directory))
  "A list of my bibliography (.bib) files.")

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

;;;; Package.el
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

;;;; Setup.el
(unless (package-installed-p 'setup)
  (package-install 'setup))

(setup setup
  (:package setup)

  ;; Demote setup errors to warnings.  Taken from the note left in
  ;; https://www.emacswiki.org/emacs/SetupEl#h5o-27.
  (add-to-list 'setup-modifier-list 'setup-wrap-to-demote-errors))

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
    :documentation "Try to require the current feature, or stop evaluating body."))

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
  
  ;; Insert spaces instead of tab characters.  The below disables
  ;; `indent-tabs-mode' globally
  (setopt indent-tabs-mode nil)
  ;; Use DWIM case commands
  (bind-keys
   ([remap upcase-word] . upcase-dwim)
   ([remap downcase-word] . downcase-dwim)
   ([remap capitalize-word] . capitalize-dwim)))

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

  (setopt completion-styles '(initials substring flex)
          ;; TODO 2025-10-17: Revisit this
          ;; completion-pcm-leading-wildcard t ; Emacs 31
          )

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

  ;; We don’t want to ignore case for completions, but buffer and file names are
  ;; exceptions
  (setopt completion-ignore-case nil
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

  (add-hook 'vertico-mode-hook #'vertico-multiform-mode)

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
                5000)
             ;; Don’t auto-save `pdf-view-mode’ buffers
             (derived-mode-p 'pdf-view-mode))))

  (add-hook 'on-first-file-hook #'auto-save-visited-mode))

;;; Recentf
;; Track recently opened files
(setup recentf

  (:bind-keys :map ctl-x-map
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

;;; Messages buffer


;;; Elisp-demos
;;;; Elisp-demos
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
  (setopt corfu-quit-at-boundary nil
          corfu-separator ?\s ; Use space
          corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted

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
    (unless (bound-and-true-p vertico-mode)
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

  ;; FIXME 2025-10-17: For some reason, I need to manually require
  ;; autorevert so that `vc-auto-revert-mode' does not error on not
  ;; being able to find the `auto-revert-mode' variable...
  (require 'autorevert)
  (vc-auto-revert-mode 1)

  (add-to-list 'display-buffer-alist
               '((or . ((major-mode . vc-dir-mode)
                        (major-mode . vc-git-log-view-mode)
                        (major-mode . vc-git-region-history-mode)))
                 (display-buffer-same-window)))

  ;; Dispatcher between `vc-diff’ and `diff-buffer-with-file’
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
;; Incremental search
(setup isearch

  (setopt isearch-repeat-on-direction-change t
        isearch-allow-scroll 'unlimited
        isearch-allow-motion t
        isearch-lazy-count t
        isearch-wrap-pause 'no-ding)

  ;; Treatment of whitespace.  Also be aware of the
  ;; `isearch-toggle-lax-whitespace' command during isearch searches
  (setopt isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil
        ;; TODO 2025-10-19: Revisit this
        ;; search-whitespace-regexp ".*?"
        ))

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
  (:package org)

  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local line-spacing 0.2
                                         fill-column 100)))

  (setopt org-directory krisb-folio-directory)

  ;; TODO 2025-05-22: Document:
  ;; - `org-hide-macro-markers'
  ;; - `org-pretty-entities-include-sub-superscripts' - see also `org-export-with-sub-superscripts'
  ;; - `org-hidden-keywords'
  ;; Fancy markup
  (setopt org-hide-emphasis-markers t
          org-pretty-entities t ; Show as UTF-8 characters (useful for math)
          org-use-sub-superscripts '{}) ; Requires brackets to recognize superscripts and subscripts

  ;; TODO 2025-05-22: Document:
  ;; - `org-special-ctrl-k'
  ;; Movement
  (setopt org-special-ctrl-a/e t
          org-ctrl-k-protect-subtree 'error)

  ;; Logging
  (setopt org-log-done 'time
          org-log-into-drawer t
          org-log-refile 'time
          org-log-reschedule 'time
          org-log-redeadline 'time)

  ;; TODO 2025-05-22: Document the "Org Plain List" customize group as
  ;; well as these options:
  ;; - `org-list-use-circular-motion'
  ;; Plain lists
  (setopt org-list-allow-alphabetical t
          org-list-demote-modify-bullet
          '(("+" . "-")
            ("-" . "*")
            ("*" . "+"))))

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

  (add-hook 'org-agenda-mode #'hl-line-mode)

  (bind-keys :map krisb-open-keymap
             ("a" . org-agenda))

  (setopt org-agenda-inhibit-startup t)
  (setopt org-agenda-files (list krisb-org-agenda-directory))

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
            ("CANCELED" . error)))
  (setopt org-enforce-todo-dependencies t)
  (setopt org-agenda-dim-blocked-tasks t)

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

  ;; Effort
  (setopt org-agenda-sort-noeffort-is-high nil)

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

;;;;; Org-id
(setup org-id

  (setopt org-id-track-globally t
        org-id-method 'ts
        org-id-link-to-org-use-id 'use-existing
        org-id-link-consider-parent-id t))

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

  (setopt org-src-fontify-natively t
        org-src-window-setup 'current-window))

;;;;; Org-cite
;; Built-in citations in org-mode
(setup oc

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
  ;; Load things related to org-mem’s interaction with org-roam
  (with-eval-after-load 'org-roam
    ;; End dependence on `org-roam-db-sync'
    (setopt org-roam-db-update-on-save nil
            org-mem-roamy-do-overwrite-real-db t)
    (org-roam-db-autosync-mode -1)
    (org-mem-roamy-db-mode 1)))

;;;;; Org-node
(setup org-node
  (:package org-node)

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
  (add-to-list 'consult-mode-histories
               '(log-edit-mode
                 log-edit-comment-ring
                 log-edit-comment-ring-index
                 log-edit-beginning-of-line))

  ;; TODO 2025-10-20: Revisit this
  ;; ;; Use the faster plocate rather than locate
  ;; (when (executable-find "plocate")
  ;;   (setopt consult-locate-args "plocate --ignore-case --existing --regexp"))

  ;; Remove sources from `consult-buffer’ I dislike.  Alternatively, I
  ;; could make these hidden, allowing access to their filter despite
  ;; being unseen.
  (dolist (source '(consult--source-recent-file
                  consult--source-file-register
                  consult--source-bookmark
                  consult--source-project-recent-file-hidden))
    (delq source consult-buffer-sources)))

;; Pulsar pulses
(setup pulsar
  (with-eval-after-load 'pulsar
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)))

;;; Time
(setup time

  (display-time-mode 1)

  (setopt display-time-24hr-format t
        display-time-format "%R"
        display-time-interval 60
        display-time-default-load-average nil)

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

;;; Startup time
;; Message for total init time after startup
(defun krisb-startup-time ()
  "Report Emacs startup time."
  (message "Total startup time: %s" (emacs-init-time)))
(add-hook 'after-init-hook #'krisb-startup-time)
