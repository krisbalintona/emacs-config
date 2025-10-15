;; -*- lexical-binding: t; -*-



(setopt user-full-name "Kristoffer Balintona"
        user-mail-address "krisbalintona@gmail.com")

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

;;; Meta-configuration

;;;; Bespoke helpers
(defun krisb-wayland-p ()
    "Return non-nil if Emacs is under Wayland."
    (getenv "WAYLAND_DISPLAY"))

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
    (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
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
 
 ;; Show context menu from right-click
 (when (display-graphic-p) (context-menu-mode 1))
 
 ;; TODO 2025-10-14: Document:
 ;; `history-delete-duplicates'
 (setopt history-length 1000)
 
 ;; Don’t wait until yanking to put clipboard text into `kill-ring’
 (setopt save-interprogram-paste-before-kill t)
 
 ;; Wayland compatibility
 (when (krisb-wayland-p)
   (setopt interprogram-cut-function
           (lambda (text)
             (start-process "wl-copy" nil "wl-copy"
                            "--trim-newline" "--type" "text/plain;charset=utf-8" text)))))

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

;;; Vertico and extensions
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

;;; Savehist
;; Make history of certain things (e.g. minibuffer) persistent across sessions
(setup savehist
  (:require)

  (setopt savehist-save-minibuffer-history t
          savehist-autosave-interval 30)

  (savehist-mode 1)

  (dolist (var '((Info-history-list . 250)))
    (add-to-list 'savehist-additional-variables var)))

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

  (add-to-list 'display-buffer-alist
               '((major-mode . help-mode)
                 (display-buffer-reuse-window display-buffer-pop-up-window display-buffer-below-selected)
                 (window-height . shrink-window-if-larger-than-buffer))))

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

;;; Startup time
;; Message for total init time after startup
(defun krisb-startup-time ()
  "Report Emacs startup time."
  (message "Total startup time: %s" (emacs-init-time)))
(add-hook 'after-init-hook #'krisb-startup-time)
