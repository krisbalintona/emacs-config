;; -*- lexical-binding: t; -*-

(defconst krisb-extra-verbose-p (getenv "EXTRA_VERBOSE")
  "Whether Emacs should be \"extra verbose\" with certain things.
A variable I use in several places when I want Emacs to be particularly
verbose for the sake of, e.g., debugging and testing.")

(when krisb-extra-verbose-p
  (setq force-load-messages t))

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

;; I have several patches awaiting review or application.
(krisb-wip-monkeypatch-symlink "~/emacs-repos/packages/notmuch/emacs/notmuch.el")
(krisb-wip-monkeypatch-symlink "~/emacs-repos/packages/notmuch/emacs/notmuch-mua.el")
(krisb-wip-monkeypatch-symlink "~/emacs-repos/packages/notmuch/emacs/notmuch-tree.el")

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

(defcustom krisb-completion-paradigm 'completion-list
  "Choose our completion paradigm."
  :type '(choice :tag "Completions user interface"
                 (const :tag "Default Completions list UI" built-in)
                 (const :tag "Use `vertico'" vertico)))

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
(setopt package-review-policy t) ; Review packages upon upgrade

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
      (add-hook hook #'krisb-set-cursor-prose))
  
    (defun krisb-set-cursor-completions-list ()
      "Set cursor settings in *Completions* buffer."
      (when (eq krisb-completion-paradigm 'completion-list)
        (setq-local cursor-in-non-selected-windows 'hbar)))
    (add-hook 'completion-list-mode-hook #'krisb-set-cursor-completions-list))
  
  ;; Word wrapping.  Continue wrapped lines at whitespace rather than
  ;; breaking in the middle of a word.
  (setopt word-wrap t)
  
  ;; Disable the ring-bell (it's annoying)
  (setopt ring-bell-function #'ignore)
  
  ;; Hide `buffer-face-mode' minor-mode lighter
  (setup face-remap
    
    (:hide-mode buffer-face-mode))
  
  ;; Don't let the window manager stop Emacs from resizing its frames
  ;; when fullscreen
  (setopt alter-fullscreen-frames t)      ; Emacs 31
  
  ;; Follow symlinks when opening files
  (setopt find-file-visit-truename t)
  
  ;; Move files into trash directory.  See also
  ;; `remote-file-name-inhibit-delete-by-moving-to-trash'
  (setopt trash-directory (no-littering-expand-var-file-name "trash")
          delete-by-moving-to-trash t) ; See also `remote-file-name-inhibit-delete-by-moving-to-trash'
  
  (defun krisb-empty-trash ()
    "Empty the trash directory."
    (interactive)
    (if delete-by-moving-to-trash
        (let ((size (string-trim (shell-command-to-string (concat"du -sh " trash-directory " | cut -f1")))))
          (when (yes-or-no-p (format "Empty trash directory of %s size? " size))
            (save-window-excursion (async-shell-command (concat "rm -rf " trash-directory "/*")))))
      (message "delete-by-moving-to-trash is nil; not emptying trash")))
  
  ;; Don't create lock files.  See also `remote-file-name-inhibit-locks'
  ;; for remote files
  (setopt create-lockfiles nil)
  
  ;; Behavior for `cycle-spacing-actions'
  (setopt cycle-spacing-actions '(just-one-space (delete-all-space -) restore))
  
  ;; TODO 2025-05-22: Document the `duplicate-line-final-position'and
  ;; `duplicate-region-final-position' user options
  (bind-key "C-x ;" #'duplicate-dwim)
  
  ;; `indent-for-tab-command' functionality: what happens when you press
  ;; TAB in a regular buffer?
  (setopt tab-always-indent 'complete
          tab-first-completion 'word)
  
  ;; Show context menu from right-click
  (when (display-graphic-p) (context-menu-mode 1))
  
  ;; See (emacs) Minibuffer Edit for an explanation
  (setopt resize-mini-windows t)
  
  ;; Exclude from M-x commands definitely irrelevant to the current
  ;; major mode
  (setopt read-extended-command-predicate #'command-completion-default-include-p)
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
    "Set up the value for `sentence-end-double-space'.
  However, do not override buffer-local and directory-local values for
  `sentence-end-double-space'."
    ;; First check if `sentence-end-double-space' is set buffer-locally
    ;; (`file-local-variables-alist') or directory-locally
    ;; (`dir-local-variables-alist')
    (unless (local-variable-p 'sentence-end-double-space)
      (setq-local sentence-end-double-space
                  (cond ((derived-mode-p '(prog-mode conf-mode log-edit-mode)) t)
                        ((derived-mode-p '(text-mode wombag-show-mode)) nil)))))
  (add-hook 'after-change-major-mode-hook #'krisb-sentence-end-double-space-setup)
  
  (setopt shell-command-prompt-show-cwd t))

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
  ;; When KRISB-EXTRA-VERBOSE-P is non-nil, print garbage collection
  ;; messages.  Otherwise, keep the default.
  (when krisb-extra-verbose-p
    (setopt garbage-collection-messages t))
  ;; NOTE 2024-02-11: Please reference
  ;; https://emacsconf.org/2023/talks/gc/ for a statistically-informed
  ;; recommendation for GC variables
  (setopt gc-cons-threshold (* 16 1024 1024) ; 16 mb
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

  (with-eval-after-load 'gcmh
    (setopt gcmh-high-cons-threshold (* 32 1024 1024) ; 32 Mb
            ;; If the idle delay is too long, we run the risk of runaway
            ;; memory usage in busy sessions.  And if it's too low, then
            ;; we may as well not be using gcmh at all.  See
            ;; https://emacsconf.org/2023/talks/gc/ for a
            ;; statistically-informed analysis of GC in Emacs.
            gcmh-idle-delay 5
            gcmh-verbose garbage-collection-messages)))

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

  (with-eval-after-load 'tab-bar
    (setopt tab-bar-close-button-show nil
            tab-bar-close-last-tab-choice 'delete-frame
            tab-bar-new-tab-choice 'window
            tab-bar-select-tab-modifiers '(meta)
            tab-bar-tab-hints t
            tab-bar-show t
            tab-bar-separator " "
            tab-bar-show-inactive-group-tabs t
            tab-bar-format
            '(tab-bar-format-tabs-groups
              tab-bar-separator
              tab-bar-format-align-right
              tab-bar-format-global)))

  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;;; Completion and minibuffer
;; TODO 2025-05-27: Document this advice by vertico to show
;; `completing-read-multiple' separator on Emacs versions below 31:
;; https://github.com/minad/vertico#completing-read-multiple.
(setup minibuffer
  
  ;; Completion styles.  `completion-styles' is set initially here but
  ;; may be modified elsewhere later (e.g., when loading the orderless
  ;; package, I add to `completion-styles')
  (setopt completion-styles '(substring initials flex)
          ;; Emacs 31.  Treat words like the substring completion
          ;; style does.  Read its docstring for details
          completion-pcm-leading-wildcard t) ; Emacs 31

  ;; A non-exhaustive list of known completion categories:
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;;
  ;; During completion, you can find out the category of the
  ;; completion canddiate at point by evaluating the form below in the
  ;; minibuffer when the point is on a completion candidate (note:
  ;; users can only do so when `enable-recursive-minibuffers' is
  ;; non-nil):
  ;; 
  ;;  (completion-metadata-get (completion-metadata (minibuffer-contents) minibuffer-completion-table minibuffer-completion-predicate) 'category)
  ;;
  (setq completion-category-defaults nil) ; This is a defvar, although setopt would still set it
  (setopt completion-category-overrides
          ;; NOTE: The eager-display and eager-update properties only
          ;; have an effect on the completion list system, not
          ;; minibuffer completion systems (like vertico)
          '((calendar-month (display-sort-function . identity))
            ;; We include `partial-completion' to enable wildcards and
            ;; partial paths (when `completion-pcm-leading-wildcard'
            ;; is non-nil)
            (file (styles . (partial-completion)))
            (kill-ring (styles . (orderless-literal-and-prefixes)))
            (consult-outline (styles . (orderless-literal-and-prefixes)))
            (consult-location (eager-display . t))
            (consult-grep (eager-display . nil))
            (consult-info (eager-display . nil))
            (recentf (eager-display . t))
            (bookmark (eager-display . t))
            ;; For tempel `citar-at-point-function's and commands
            (tempel (eager-display . t))
            (info-menu (eager-display t))
            (org-node (eager-display . t))))
  
  ;; How do we want to treat case in completion?
  (setopt completion-ignore-case t
          read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t))

(setup minibuffer
  
  ;; Minibuffer movement.  See also `minibuffer-visible-completions'
  ;; and the commands bound to `minibuffer-visible-completions-map'
  (setopt minibuffer-beginning-of-buffer-movement t)
  
  ;; Let a minibuffer be created from within a minibuffer
  (setopt enable-recursive-minibuffers t)
  ;; And indicate the current recursion level in the minibuffer prompt
  (minibuffer-depth-indicate-mode 1)

  ;; TODO 2025-10-14: Document:
  ;; `history-delete-duplicates'
  (setopt history-length 1000)

  ;; Minibuffer prompts
  (setopt minibuffer-default-prompt-format " [%s]")
  (minibuffer-electric-default-mode 1)  ; Show default value
  (setopt minibuffer-prompt-properties
          ;; Don't let point enter the minibuffer prompt
          '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Don't show mode line
  (add-hook 'completion-list-mode-hook #'mode-line-invisible-mode)) ; Emacs 31.1

;;;; Completion-list (*Completions* buffer)
;; TODO 2025-05-20: Document the following options below in the
;; literate configuration:
;; - `completion-cycle-threshold' - Cycling = repeated completion
;;   attempts insert the next completion candidate into the minibuffer
;; - `completion-flex-nospace’
(setup minibuffer

  ;; Completions format
  (setopt completion-show-help nil
          completions-max-height 10 ; Otherwise the completions buffer can grow to fill the entire frame
          completions-format 'vertical
          completions-group t           ; Emacs 28
          completions-detailed t) ; Show annotations for candidates (like marginalia.el)

  ;; Custom sorting function.  Copied from Ihor's dotemacs.  Based on
  ;; Emacs 31's `minibuffer-sort-by-history' (the 'historical value
  ;; for `completions-sort')
  (defun krisb-minibuffer-sort-by-history-then-distance (completions)
    "Sort COMPLETIONS by Levenshtein distance to input, then put recent on top.
This function is like `minibuffer-sort-by-history', but sorts by string
distance + `string-lessp' first rather than using only `string-lessp'."
    (let* ((minibuffer-input (when (minibufferp) (minibuffer-contents)))
           (to-complete-string
            (when minibuffer-input
              (substring minibuffer-input
                         (car (completion-boundaries
                               minibuffer-input
                               minibuffer-completion-table nil "")))))
           (pre-sorted
            (sort completions
                  :lessp
                  (if to-complete-string
                      (lambda (a b)
                        (let ((distance-a (string-distance to-complete-string a))
                              (distance-b (string-distance to-complete-string b)))
                          (or (< distance-a distance-b)
                              (and (= distance-a distance-b)
                                   (string-lessp a b)))))
                    #'string-lessp))))
      ;; The following code is copied from
      ;; `minibuffer-sort-by-history'.
      ;;
      ;; Only use history when it's specific to these completions.
      (if (eq minibuffer-history-variable
              (default-value minibuffer-history-variable))
          pre-sorted
        (minibuffer--sort-by-position
         (minibuffer--sort-preprocess-history minibuffer-completion-base)
         pre-sorted))))
  (setopt completions-sort #'krisb-minibuffer-sort-by-history-then-distance)
  
  ;; Automatic ("eager") display and updating of the *Completions*
  ;; buffer: control with `completion-category-overrides'
  (setopt completion-eager-display 'auto
          completion-eager-update t)

  ;; Selection of the Completions buffer from the minibuffer
  (setopt completion-auto-help 'visible
          completion-auto-select 'second-tab ; When, if ever, to select *Completions* on TAB press
          completion-auto-deselect nil)

  ;; *Completion* buffer keybindings
  (bind-keys :map completion-list-mode-map
             ;; TODO 2025-11-30: Suggest these bindings to upstream
             ("b" . previous-column-completion)
             ("f" . next-column-completion))
  ;; Emacs 30.1: Navigate in the *Completions* buffer from the
  ;; minibuffer.  (Fans of vertico might like this)
  (setopt minibuffer-visible-completions t)
  (define-key minibuffer-visible-completions-map
              (kbd "C-v")
              (minibuffer-visible-completions--bind #'minibuffer-hide-completions))
  (unbind-key "C-g" minibuffer-visible-completions-map)
  (define-key minibuffer-visible-completions-map
              (kbd "C-n")
              (minibuffer-visible-completions--bind #'minibuffer-next-line-completion))
  (define-key minibuffer-visible-completions-map
              (kbd "C-p")
              (minibuffer-visible-completions--bind #'minibuffer-previous-line-completion)))

;; Bespoke extensions
(setup minibuffer
  ;; Change the behavior of `C-g' in the Completions buffer.  (I
  ;; noticed that a similar command is `mct-keyboard-quit-dwim' from
  ;; mct.)
  (defun krisb-keyboard-quit ()
    "Change the behavior of `C-g' in the Completions buffer.
Make `C-g' in in the Completions buffer behave like
`minibuffer-keyboard-quit'.

In the Completions buffer, if the region is active, deactivate the
region.  If the region is not active, then call `abort-recursive-edit',
ending the minibuffer session. (This is equivalent to the behavior of
`minibuffer-keyboard-quit' in the minibuffer.)"
    (interactive nil completion-list-mode)
    (if (and delete-selection-mode (region-active-p))
        (deactivate-mark)
      (abort-recursive-edit)))
  (bind-key [remap keyboard-quit] 'krisb-keyboard-quit 'completion-list-mode-map))

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
    (add-hook 'on-first-input-hook
              (lambda () (cl-nsubstitute 'hotfuzz 'flex completion-styles))))

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
  (:require)

  (setopt completion-styles '(orderless flex))

  ;; TODO 2025-05-20: Revisit this.
  ;; ;; Eglot forces `flex' by default.
  ;; (add-to-list 'completion-category-overrides '(eglot (styles . (orderless flex))))

  (with-eval-after-load 'orderless
    (setopt orderless-component-separator 'orderless-escapable-split-on-space
            orderless-expand-substring 'substring ; Prefer this `orderless-try-completion' behavior
            orderless-matching-styles
            '(orderless-regexp
              orderless-prefixes
              orderless-initialism
              orderless-literal
              ;; orderless-literal-prefix
              ;; orderless-flex
              )))
  ;; Our orderless components are split by spaces (see
  ;; `orderless-component-separator') so, for convenience, make SPC
  ;; insert space
  (with-eval-after-load 'orderless
    (unbind-key "SPC" minibuffer-local-completion-map))

  ;; Define my own completion styles that bundle sets of orderless
  ;; matching styles
  (orderless-define-completion-style orderless-literal-and-prefixes
    "Combine the literal and prefixes orderless matching styles.
This is useful for when I need something like the built-in substring or
partial-completion styles but want orderless's order-less component
matching."
    (orderless-matching-styles '(orderless-literal
                                 orderless-prefixes))))

;;;; Vertico and extensions
(setup vertico
  (:package vertico)
  ;; NOTE 2025-11-25: Trying out the built-in Completions buffer for a
  ;; while
  (:quit)
  
  (vertico-mode 1)
  
  (:bind-keys :filter vertico-mode ("C-c v r" . vertico-repeat)
              :map vertico-map
              :filter vertico-mode ("C-c v s" . vertico-suspend))

  (with-eval-after-load 'vertico
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)))

;; More convenient path modification commands
(setup vertico-directory
  (:load-after vertico)

  (with-eval-after-load 'vertico-directory
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

;; On-demand change the type of UI
(setup vertico-multiform
  (:load-after vertico)
  
  (with-eval-after-load 'vertico-multiform
    (vertico-multiform-mode 1)

    ;; See related, `completion-category-overrides'
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
               flat)))))

;; A minimal, Ido-like UI
(setup vertico-flat
  (:load-after vertico)

  (with-eval-after-load 'vertico-flat
    (setopt vertico-flat-annotate t
            vertico-flat-format
            `( :multiple #("\n{%s}" 0 2 (face minibuffer-prompt) 4 5 (face minibuffer-prompt))
               :single #("\n[%s]" 0 2 (face minibuffer-prompt) 2 4 (face success) 4 5 (face minibuffer-prompt))
               :prompt #("(%s)" 0 1 (face minibuffer-prompt) 3 4 (face minibuffer-prompt))
               :separator #("  |  " 0 5 (face minibuffer-prompt))
               :ellipsis ,(propertize "…" 'face 'minibuffer-prompt)
               :no-match ,(propertize "\n[No match]" 'face 'shadow)
               :spacer #(" " 0 1 (cursor t))))))

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
;; TODO 2025-05-23: Note that auto-save is distinct from
;; `auto-save-visited-mode’
;; 
;; See also `remote-file-name-inhibit-auto-save' and
;; `remote-file-name-inhibit-auto-save-visited'
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

  (setopt recentf-autosave-interval 300 ; Emacs 31
          recentf-auto-cleanup 600
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
                   (display-buffer-reuse-window display-buffer-pop-up-window display-buffer-below-selected)))))

;;; Apropos
(setup apropos)

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

;;; Elisp-mode
(setup elisp-mode
  
  ;; Emacs 31: semantic highlighting
  (with-eval-after-load 'elisp-mode
    (setopt elisp-fontify-semantically t)
    ;; Trust content from my Emacs directory and the Guix store (where
    ;; Guix installs Emacs source files).  (Directory paths must end
    ;; in "/".)
    (add-to-list 'trusted-content "/gnu/store/")
    ;; TODO 2025-12-03: Looks like the file names in this option must
    ;; be abbreviated (look at `trusted-content-p').  Send a bug
    ;; report
    (add-to-list 'trusted-content (abbreviate-file-name user-emacs-directory))
    (add-to-list 'trusted-content "~/emacs-repos/")))

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
  (with-eval-after-load 'diff
    (unbind-key "o" diff-mode-shared-map))
  (with-eval-after-load 'eat
    (unbind-key "M-o" eat-semi-char-mode-map))
  (with-eval-after-load 'xscheme
    (unbind-key "M-o" scheme-mode-map))

  (setopt switch-to-buffer-obey-display-actions t
          window-resize-pixelwise t
          quit-restore-window-no-switch t ; Emacs 31
          kill-buffer-quit-windows t))    ; Emacs 31

;;; Corfu
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;; - `corfu-auto'
;; - `corfu-cycle'
;; Faster, minimal, and more lightweight autocomplete that is more
;; faithful to the Emacs infrastructure
(setup corfu
  (:package corfu)
  
  (global-corfu-mode 1)
  
  (:bind-keys
   ;; TODO 2025-05-20: Revisit this.
   ;; ("M-i" . completion-at-point) ; For harmony with "M-i" in `completion-preview-active-mode-map'
   :map corfu-map
   ("M-d" . corfu-info-documentation))

  ;; Always use a fixed-pitched font for corfu; variable pitch fonts
  ;; (which will be adopted in a variable pitch buffer) have
  ;; inconsistent spacing
  (:face corfu-default ((t (:inherit 'default))))
  
  (with-eval-after-load 'corfu
    (setopt corfu-count 14
            corfu-scroll-margin 3
            ;; Always have the same width
            corfu-min-width 75
            corfu-max-width corfu-min-width))
  
  ;; Allow spaces and don't quit on boundary to leverage orderless's
  ;; space-separated components
  (with-eval-after-load 'orderless
    (setopt corfu-quit-at-boundary nil
            corfu-separator ?\s         ; Use space
            corfu-quit-no-match 'separator)) ; Don't quit if there is `corfu-separator' inserted

  ;; Corfu in the minibuffer only conditionally.  Based on
  ;; https://github.com/minad/corfu?tab=readme-ov-file#completing-in-the-minibuffer
  (setopt global-corfu-minibuffer
          (lambda ()
            ;; Enable in minibuffer if it is for a command that reads
            ;; elisp expressions (e.g. `eval-expression') and if it
            ;; satisfies the other predicates below
            (and (eq (current-local-map) read--expression-map)
                 (not (or
                       ;; Showing completions in the minibuffer from
                       ;; either MCT or vertico
                       (bound-and-true-p mct--active)
                       (bound-and-true-p vertico--input)
                       ;; Inputting password
                       (eq (current-local-map) read-passwd-map)))))))

;; Extras
(setup corfu
  (with-eval-after-load 'corfu
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
      (:bind-keys :map corfu-map ("M-j" . krisb-corfu-move-to-minibuffer))
      (add-to-list 'corfu-continue-commands #'krisb-corfu-move-to-minibuffer))))

;; Extension that comes with corfu.  Popup documentation window for
;; corfu candidates
(setup corfu-popupinfo
  (:load-after corfu)

  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)

  (bind-keys :map corfu-map
             ([remap corfu-info-documentation] . corfu-popupinfo-toggle)
             ("M-l" . corfu-popupinfo-location))

  (with-eval-after-load 'corfu-popupinfo
    (setopt corfu-popupinfo-delay '(nil . 0.4) ; Don't display initially
            corfu-popupinfo-direction '(right left vertical)
            corfu-popupinfo-hide t
            corfu-popupinfo-resize t
            corfu-popupinfo-max-height 70
            corfu-popupinfo-max-width 80
            corfu-popupinfo-min-height 1
            corfu-popupinfo-min-width 25)))

;; History of selections of corfu candidates for better completion
;; sorting
(setup corfu-history
  (:load-after corfu)
  
  ;; See also `corfu-history-duplicate' and `corfu-history-decay'
  (corfu-history-mode 1))

;;; Completion-preview
(setup completion-preview
  
  (:bind-keys :map completion-preview-active-mode-map
              ("M-p" . completion-preview-prev-candidate)
              ("M-n" . completion-preview-next-candidate)
              ("M-i" . completion-preview-insert-word)
              ("C-M-i" . completion-preview-complete))
  
  (with-eval-after-load 'completion-preview
    (setopt completion-preview-sort-function
            #'krisb-minibuffer-sort-by-history-then-distance)))

;;; Electric
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;; - `electric-quote-comment'
;; - `electric-quote-string'
;; - `electric-quote-inhibit-functions'
;; - `electric-pair-delete-adjacent-pairs'
;; - `electric-pair-pairs'
;; - `electric-pair-text-pairs'
;; NOTE: 2025-05-22: For some reason lisp-mode sets these buffer
;; locally.  See `lisp-mode-variables'.
;; - `electric-pair-skip-whitespace'
;; - `electric-pair-open-newline-between-pairs'
(setup electric
  
  (electric-pair-mode 1)
  (electric-indent-mode 1)

  (with-eval-after-load 'electric
    (setopt electric-quote-context-sensitive t
            electric-quote-replace-double t)))

(with-eval-after-load 'log-edit
  (defun krisb-electric-setup-log-edit ()
    "Add to `electric-pair-text-pairs'."
    (make-local-variable 'electric-pair-pairs)
    (when-let* ((project (project-current)))
      (pcase (expand-file-name (project-root project))
        ((pred (string-match-p "emacs-repos/packages/emacs/"))
         (cl-pushnew '(?' . ?') electric-pair-pairs :test #'equal))
        (_
         (cl-pushnew '(?` . ?`) electric-pair-pairs :test #'equal)))))
  (add-hook 'log-edit-mode-hook 'krisb-electric-setup-log-edit))

;;; Files
(setup files
  
  (with-eval-after-load 'files
    (setopt remote-file-name-inhibit-locks t ; Don't create lock files in remotes
            ;; Disables aggressive caching that slows Dired/Magit;
            ;; test and toggle if needed.
            remote-file-name-inhibit-cache nil

            remote-file-name-inhibit-auto-save-visited nil
            remote-file-name-inhibit-auto-save nil
            remote-file-name-inhibit-delete-by-moving-to-trash t
            
            ;; Set a timeout to prevent the failure to access files to
            ;; indefinitely bloc Emacs
            remote-file-name-access-timeout 10

            enable-remote-dir-locals t)))

;;; Autorevert
;; Automatically update buffers as files are externally modified
;; TODO 2025-05-22: Document:
;; - `auto-revert-verbose’
;; - `auto-revert-avoid-polling'
;; - `auto-revert-check-vc-info'
(setup autorevert

  (:hide-mode auto-revert-mode)

  (with-eval-after-load 'autorevert
    (setopt auto-revert-interval 3
            global-auto-revert-non-file-buffers t))

  ;; FIXME 2025-12-29: Causing freezes when SSHing into my remote
  ;; machine.  Look into if this is a Tramp or remote machine issue.
  ;; ;; Revert remote files too?  See also
  ;; ;; `remote-file-name-inhibit-auto-save-visited'
  ;; (setopt auto-revert-remote-files t)
  
  (if (fboundp 'vc-auto-revert-mode)
      (vc-auto-revert-mode 1)             ; Emacs 31
    (global-auto-revert-mode 1)))

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

  (with-eval-after-load 'vc
    (setopt vc-follow-symlinks t
            vc-allow-rewriting-published-history 'ask ; Emacs 31
            vc-async-checkin t
            vc-allow-async-diff t       ; Emacs 31
            vc-revert-show-diff t
            vc-find-revision-no-save t            ; Emacs 31
            vc-dir-hide-up-to-date-on-revert t    ; Emacs 31
            vc-dir-save-some-buffers-on-revert t  ; Emacs 31
            vc-use-incoming-outgoing-prefixes t)) ; Emacs 31

  ;; I dislike this default
  (with-eval-after-load 'vc
    (remove-hook 'vc-log-finish-functions 'vc-shrink-buffer-window))
  
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

  (:face log-edit-summary ((t (:family ,(face-attribute 'variable-pitch :family)))))
  
  ;; Evaluate after log-edit defines `log-edit-hook' first, since the
  ;; hook already has several functions.  (Otherwise, we are adding to
  ;; an empty hook.)
  (with-eval-after-load 'log-edit
    (add-hook 'log-edit-hook #'auto-fill-mode)
    (add-hook 'log-edit-hook #'log-edit-maybe-show-diff)))

;;; Outline.el
(setup outline

  (:hide-mode outline-minor-mode)

  (setopt outline-minor-mode-cycle t
          outline-minor-mode-cycle-filter nil
          outline-minor-mode-highlight 'append
          outline-blank-line t))

;;; Hideshow
(setup hideshow

  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (add-hook 'conf-mode-hook #'hs-minor-mode)
  (:hide-mode hs-minor-mode)
  
  ;; See also `hs-hide-block-behavior' and `hs-display-lines-hidden'
  (with-eval-after-load 'hideshow
    (setopt hs-hide-comments-when-hiding-all nil
            hs-isearch-open t)))

;;; Outli.el
;; TODO 2025-05-20: Document that I prefer this over the heavier,
;; less-compatible outshine.el as well as outline-indent.
;; Coding language-agnostic file outlines.  Lightweight and close to
;; the built-in outline.el.
(setup outli
  ;; Elpaca: :ensure (:repo "https://github.com/jdtsmith/outli")
  (:package outli)

  (add-hook 'text-mode-hook #'outli-mode)
  (add-hook 'prog-mode-hook #'outli-mode)
  (add-hook 'conf-mode-hook #'outli-mode)
  
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
          outli-blend nil))

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

;;; Occur
(setup occur

  (add-to-list 'display-buffer-alist
               '((major-mode . occur-mode)
                 nil
                 (post-command-select-window . t))))

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

  (with-eval-after-load 'pulsar
    (setopt pulsar-pulse t
            pulsar-delay 0.05
            pulsar-iterations 5
            pulsar-pulse-on-window-change t)

    (cl-delete 'backward-kill-word pulsar-pulse-region-functions)
    (cl-delete 'backward-kill-sexp pulsar-pulse-region-functions)
    (cl-delete 'backward-kill-sentence pulsar-pulse-region-functions)
    (cl-delete 'kill-word pulsar-pulse-region-functions)
    (cl-delete 'kill-sexp pulsar-pulse-region-functions)
    (cl-delete 'kill-line pulsar-pulse-region-functions)
    (cl-delete 'kill-visual-line pulsar-pulse-region-functions)
    
    ;; Faces
    (setopt pulsar-face 'pulsar-red
            pulsar-region-change-face 'pulsar-cyan
            pulsar-window-change-face 'pulsar-green))

  (pulsar-global-mode 1))

;;; Org
;;;; Org built-ins
;;;;; Org-mode
(setup org
  (:pin org-mode "gnu-elpa-devel")
  (:package org)

  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'variable-pitch-mode)
    (add-hook 'org-mode-hook #'visual-line-mode)
    (add-hook 'org-mode-hook #'visual-wrap-prefix-mode)
    (add-hook 'org-mode-hook (lambda ()
                               (setq-local line-spacing 0.2
                                           fill-column 100))))
  ;; Show zero width spaces (org syntax's escape character) as red
  ;; middle dots
  (font-lock-add-keywords
   'org-mode
   `(("\u200B"
      0 (progn
          ;; Show a red middle dot at the ZWSP
          (put-text-property (match-beginning 0) (match-end 0)
                             'display (propertize "·" 'face '(:inherit error)))
          nil)))
   'append)

  (setopt org-directory krisb-folio-directory)

  (with-eval-after-load 'org
    ;; TODO 2025-10-15: Document these:
    ;; - `org-use-fast-todo-selection'
    ;; Todos
    (setopt org-todo-keywords
            '((sequence "TODO(t)" "DOING(o)" "NEXT(n)" "HOLD(h@/!)" "MAYBE(m)" "|"
                        "DONE(d!/@)" "CANCELED(c@/!)"))
            org-todo-keyword-faces
            '(("DOING" . (bold success))
              ("NEXT" . (bold success))
              ("TODO" . org-todo)
              ("HOLD" . (shadow error))
              ("MAYBE" . (shadow org-todo))
              ("DONE" . (bold org-done))
              ("CANCELED" . error))
            org-enforce-todo-dependencies t
            org-agenda-dim-blocked-tasks t)
    ;; See also `org-trigger-hook'
    (setopt org-todo-state-tags-triggers
            '((todo)
              ("DOING" ("WAITING" . nil) ("PUSHED" . nil))
              ("NEXT" ("WAITING" . nil))
              ("TODO" ("WAITING" . nil))
              ("HOLD" ("PUSHED" . nil))
              ("MAYBE" ("WAITING" . nil))
              (done ("WAITING" . nil) ("PUSHED" . nil))
              ("DONE")
              ("CANCELED")))
    
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
            org-use-fast-tag-selection 'auto
            org-fast-tag-selection-single-key 'expert
            org-tags-exclude-from-inheritance
            '("PROJECT" "email"
              "__journal" "__log" "__top_of_mind")
            org-tag-faces '(("INBOX" . font-lock-variable-use-face)
                            ("PROJECT" . font-lock-doc-markup-face)
                            ("WAITING" . font-lock-keyword-face)
                            ("PING" . font-lock-keyword-face)
                            ("REVIEW" . font-lock-keyword-face)
                            ("PUSHED" . font-lock-negation-char-face)))
    ;; See also `org-todo-state-tags-triggers'
    ;; TODO 2025-12-02: Send a patch upstream sometime
    (el-patch-defun org-set-tags (tags)
      "Set the tags of the current entry to TAGS, replacing current tags.
    
    TAGS may be a tags string like \":aa:bb:cc:\", or a list of tags.
    If TAGS is nil or the empty string, all tags are removed.
    
    This function assumes point is on a headline."
      (org-with-wide-buffer
       (org-fold-core-ignore-modifications
         (let ((tags (pcase tags
                       ((pred listp) tags)
                       ((pred stringp) (split-string (org-trim tags) ":" t))
                       (_ (error "Invalid tag specification: %S" tags))))
               (old-tags (org-get-tags nil t))
               (tags-change? nil))
           (when org-tags-sort-function
             (setq tags (sort tags #'org-tags-sort)))
           (setq tags-change? (not (equal tags old-tags)))
           (when tags-change?
             ;; Delete previous tags and any trailing white space.
             (goto-char (if (org-match-line org-tag-line-re) (match-beginning 1)
                          (line-end-position)))
             (skip-chars-backward " \t")
             (delete-region (point) (line-end-position))
             ;; Deleting white spaces may break an otherwise empty headline.
             ;; Re-introduce one space in this case.
             (unless (org-at-heading-p) (insert " "))
             (when tags
               (save-excursion (insert-and-inherit " " (org-make-tag-string tags)))
               ;; When text is being inserted on an invisible region
               ;; boundary, it can be inadvertently sucked into
               ;; invisibility.
               (unless (org-invisible-p (line-beginning-position))
                 (org-fold-region (point) (line-end-position) nil 'outline))))
           ;; Align tags, if any.
           (when (and tags org-auto-align-tags) (org-align-tags))
           (when tags-change?
             (el-patch-swap
               (run-hooks 'org-after-tags-change-hook)
               (run-hook-with-args 'org-after-tags-change-hook old-tags tags)))))))
    
    (defun kribs-org-inbox-tag (before-tags after-tags)
      "Prompt to remove the NEXT_VISIBLE property when adding or removing the INBOX tag.
    BEFORE-TAGS are the tags before the headline\\='s tags were updated and
    AFTER-TAGS are the tags after the headline was updated.
    
    Meant to be added to `org-after-tags-change-hook'."
      (let ((prop-name "NEXT_VISIBLE")
            (inbox-tag-p (member "INBOX" (seq-union after-tags before-tags))))
        (when (and inbox-tag-p
                   (org-entry-get (point) prop-name)
                   (y-or-n-p (format "Remove %s property?" prop-name)))
          (org-entry-delete (point) prop-name)
          (message "Removed the \"%s\" property from todo" prop-name))))
    (add-hook 'org-after-tags-change-hook #'kribs-org-inbox-tag)

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
  (defun krisb-org-agenda-skip-timely ()
    "Filter tasks for timely agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (or (scheduled :to today)
               (deadline auto)))))
  
  (defun krisb-org-agenda-skip-focus ()
    "Filter tasks for focus agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (not (tags-local "PROJECT"))
           (tosee)
           (not (tags "PUSHED"))
           (not  (scheduled :to today))
           (not (deadline auto))
           (not (tags-local "REVIEW"))
           (or (todo "DOING")
               (priority "A")))))
  
  (defun krisb-org-agenda-skip-pushed ()
    "Filter tasks for push agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (tosee)
           (tags "PUSHED"))))
  
  (defun krisb-org-agenda-skip-routine ()
    "Filter tasks for routine agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (not (tags-local "PROJECT" "INBOX"))
           (tosee)
           (or (and (or (habit)
                        ;; FIXME 2025-12-03: Avoid hard-coding this path
                        (path "recurring\\.org"))
                    (or (scheduled :to today)
                        (deadline :to auto)))))))
  
  (defun krisb-org-agenda-skip-radar ()
    "Filter tasks for radar agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (not (tags-local "PROJECT" "INBOX"))
           (tosee)
           (not (tags-local "REVIEW"))
           (not (tags "PUSHED"))
           (or (and (todo "NEXT" "TODO")
                    (not (or (scheduled)
                             (deadline))))
               (todo "HOLD" "MAYBE")))))
  
  (defun krisb-org-agenda-skip-review ()
    "Filter tasks for review agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (or (toreview)
               (tags-local "REVIEW")))))
  
  (defun krisb-org-agenda-skip-inbox ()
    "Filter tasks for inbox agenda."
    (krisb-org-agenda-skip-org-ql
     '(and (not (done))
           (tosee)
           (or (tags-local "INBOX")))))
  
  (setopt org-agenda-custom-commands
          '(("n" "Agenda and all TODOs"
             ((agenda "")
              (alltodo "")))
            ("f" "Focus"
             ((agenda ""
                      ((org-agenda-overriding-header "Scheduled and deadlines")
                       (org-agenda-skip-function 'krisb-org-agenda-skip-timely)
                       (org-agenda-span 2)
                       (org-agenda-show-future-repeats nil)))
              (alltodo ""
                       ((org-agenda-overriding-header "Focus")
                        (org-agenda-skip-function 'krisb-org-agenda-skip-focus)))
              (alltodo ""
                       ((org-agenda-overriding-header "Waiting and Pings")
                        (org-agenda-skip-function (lambda ()
                                                    (krisb-org-agenda-skip-org-ql
                                                     '(and (not (done))
                                                           (tosee)
                                                           (or (tags-local "PING")
                                                               (tags-local "WAITING"))))))))
              (alltodo ""
                       ((org-agenda-overriding-header "Pushed to the front")
                        (org-agenda-skip-function 'krisb-org-agenda-skip-pushed)))))
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

  ;; Helpers for `org-capture-templates'
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
                                                     (org-mem-tags node)))
                                          tags))
                              t nil 'org-node-hist)
             org-node--candidate<>entry))

  ;; See also `org-capture-templates-contexts'
  (setopt org-capture-templates
          `(("t" "Todo" entry
             (file krisb-org-agenda-main-file)
             "* TODO %? :INBOX:%^g\n"
             :empty-lines 1)
            ("T" "Todo (without processing)" entry
             (file krisb-org-agenda-main-file)
             "* TODO %? %^g\n"
             :empty-lines 1
             :prepare-finalize (lambda ()
                                 (message "Insert NEXT_VISIBLE")
                                 (krisb-org-review-insert-next-visible)
                                 (message "Insert NEXT_REVIEW")
                                 (org-review-insert-next-review)))
            ("c" "Coding projects and contributions" entry
             (file (lambda () (expand-file-name "coding_projects.org" krisb-org-agenda-directory)))
             "* TODO %? %^g\n"
             :empty-lines 1
             :refile-targets
             ((,(expand-file-name "coding_projects.org" krisb-org-agenda-directory)
               . (:level . 1)))
             :prepare-finalize (lambda ()
                                 (message "Insert NEXT_VISIBLE")
                                 (krisb-org-review-insert-next-visible)
                                 (message "Insert NEXT_REVIEW")
                                 (org-review-insert-next-review)))
            ("j" "Journal" entry
             (file+olp+datetree
              (lambda ()
                (let* ((node (krisb-org-capture--org-node-by-tags `(,(rx bol (or "__journal" "__top_of_mind") eol)))))
                  (org-capture-put :krisb-node node)
                  (org-mem-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-mem-subtree-p node)
                    (org-mem-olpath-with-self node)))))
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
                  (org-mem-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-mem-subtree-p node)
                    (org-mem-olpath-with-self node)))))
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
                  (org-mem-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-mem-subtree-p node)
                    (org-mem-olpath-with-self node)))))
             "%U %?"
             :tree-type (quarter week)
             :clock-in t
             :clock-resume t)
            ("m" "Work meeting notes" entry
             (file+olp+datetree
              (lambda ()
                (let* ((node (org-mem-entry-by-id "20241114T091749.707997")))
                  (org-capture-put :krisb-node node)
                  (org-mem-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-mem-subtree-p node)
                    (org-mem-olpath-with-self node)))))
             "* (%<%c>)%?\n\n"
             :tree-type (year quarter month)
             :jump-to-captured t
             :immediate-finish t)
            ("r" "New reference" entry
             (file+olp+datetree
              (lambda ()
                (let* ((node (krisb-org-capture--org-node-by-tags '("^__references$"))))
                  (org-capture-put :krisb-node node)
                  (org-mem-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-mem-subtree-p node)
                    (org-mem-olpath-with-self node)))))
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
            ("g" "LoL match review" entry
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
                  (org-mem-file node)))
              (lambda ()
                (let ((node (org-capture-get :krisb-node)))
                  ;; Should return nil if node is a file
                  (when (org-mem-subtree-p node)
                    (org-mem-olpath-with-self node)))))
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

;;;;; Org-refile
;; TODO 2025-06-25: Document:
;; - `org-refile-use-cache'
(setup org-refile
  (:if-package org)
  
  (setopt org-refile-targets
          '((org-agenda-files . (:maxlevel . 3))
            (org-agenda-files . (:tag . "PROJECT"))
            (nil . (:maxlevel . 4)))
          org-outline-path-complete-in-steps nil
          org-refile-use-outline-path 'title
          ;; TODO 2024-10-07: Think about whether I actually want this.  What
          ;; if I want to refile to a non-todo heading in the current file?
          ;; (org-refile-target-verify-function ; Only let not done todos be refile targets
          ;;  (lambda () (if (org-entry-is-todo-p) (not (org-entry-is-done-p)))))
          org-refile-target-verify-function nil
          org-refile-allow-creating-parent-nodes 'confirm))

(setup org-refile
  (when (eq krisb-completion-paradigm 'vertico)
    (with-eval-after-load 'vertico
      ;; Workaround for vertico issue with `org-refile'.  See
      ;; https://github.com/minad/vertico#org-refile
      (setopt org-refile-use-outline-path 'file
              org-outline-path-complete-in-steps t)
      (defun krisb-vertico-enforce-basic-completion (&rest args)
        (minibuffer-with-setup-hook
            (:append
             (lambda ()
               (let ((map (make-sparse-keymap)))
                 (define-key map [tab] #'minibuffer-complete)
                 (use-local-map (make-composed-keymap (list map) (current-local-map))))
               (setq-local completion-styles (cons 'basic completion-styles)
                           vertico-preselect 'prompt)))
          (apply args)))
      (advice-add #'org-olpath-completing-read :around #'krisb-vertico-enforce-basic-completion))))

;;;; Other org packages
;;;;; Org-contrib
;; Collection of org packages
(setup org-contrib
  (:package org-contrib))

;;;;; Org-modern
;; TODO 2025-12-03: Document
;; - `org-modern-agenda'
(setup org-modern
  (:package org-modern)

  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'org-modern-mode))
  
  (with-eval-after-load 'org-modern
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
            '(("DOING" :inherit (bold success org-modern-todo))
              ("NEXT" :inherit (bold success org-modern-todo))
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
            org-modern-table-horizontal 0.1))

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
    (let ((exclude-val (cdr (assoc "ROAM_EXCLUDE" (org-mem-properties node)))))
      (not (or (when exclude-val (string= "t" (string-trim exclude-val)))))))
  (setopt org-node-filter-fn #'krisb-org-node-filter-fn)

  ;; Bespoke `org-node-custom-link-format-fn' function
  (cl-defmethod krisb-org-node-custom-link-format-fn ((node org-mem-entry))
    "Bespoke function for `org-node-custom-link-format-fn'."
    (if (or (file-in-directory-p (org-mem-file node) krisb-org-agenda-directory)
            (file-in-directory-p (org-mem-file node) krisb-org-archive-directory))
        (org-mem-entry-title node)
      (let* ((place (krisb-org-node-get-place node))
             (type (krisb-org-node-get-type node))
             (title (org-mem-entry-title node))
             (file-title (org-mem-file-title-strict node)))
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
    `(cdr (assoc ,property (org-mem-properties ,node) #'string-equal)))

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
                 (file-name-directory (org-mem-file node))))))
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

  (with-eval-after-load 'org-web-tools
    (setopt org-web-tools-pandoc-replacements
            (add-to-list 'org-web-tools-pandoc-replacements
                         `(,(rx "·") . "|"))))

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
  
  (with-eval-after-load 'org-expiry
    (setopt org-expiry-inactive-timestamps t))
  
  (autoload 'org-expiry-insert-created "org-expiry")
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-before-finalize-hook 'org-expiry-insert-created)))

;;;;; Org-review
(setup org-review
  (:if-package org)
  (:package org-review)
  
  (with-eval-after-load 'org-review
    (setopt org-review-delay "+1m"
            org-review-last-timestamp-format 'inactive
            org-review-next-timestamp-format 'inactive
            org-review-sets-next-date t))
  
  (with-eval-after-load 'org
    (bind-keys :map org-mode-map
               ("C-c r s" . org-review-insert-next-review)
               ("C-c r l" . org-review-insert-last-review)))
  (with-eval-after-load 'org-agenda
    (bind-keys :map org-agenda-mode-map
               ("C-c r s" . org-review-insert-next-review)
               ("C-c r l" . org-review-insert-last-review))))

;; Custom predicate for org-review
(setup org-review
  (with-eval-after-load 'org-ql
    (:require)
    
    (org-ql-defpred toreview ()
      "Match headings where `org-review-toreview-p' is non-nil."
      :body (org-review-toreview-p))

    (org-ql-defpred hasreview ()
      "Match headings where `org-review-next-review-prop' is non-nil."
      :body (org-review-next-review-prop))))

;; Personal extensions to org-review
(setup org-review
  
  (with-eval-after-load 'org-review
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
    (defun krisb-org-review-randomize (prop-name days)
      "Randomly set the next PROP-NAME date for entry within the next DAYS days.
DAYS should be a positive integer.  Calls `org-review-insert-date' onto
a random date within the next DAYS days."
      (interactive
       (list (completing-read "Property: "
                              (list org-review-next-property-name
                                    "NEXT_VISIBLE"))
             (krisb-org-review--select-day)))
      (let* ((random-day (1+ (random days)))
             (ts (format-time-string (car org-time-stamp-formats)
                                     (time-add (current-time) (days-to-time random-day)))))
        ;; We don't also call `org-review-insert-last-review' because
        ;; I sometimes I do not want that.  In the cases when I'd like
        ;; that function called as well, I persist the org-agenda
        ;; marks and call that function before or after this one
        (org-review-insert-date prop-name
                                org-review-next-timestamp-format
                                ts))))
  
  ;; Keybinds
  (with-eval-after-load 'org-agenda
    (add-to-list 'org-agenda-bulk-custom-functions
                 '(?R krisb-org-review-randomize
                      ;; Must return a list (of arguments)
                      (lambda ()
                        (list (completing-read "Property: "
                                               (list org-review-next-property-name
                                                     "NEXT_VISIBLE"))
                              (krisb-org-review--select-day)))))))

;; Adjacent to org-review: NEXT_VISIBLE
(defun krisb-org-review-insert-next-visible ()
  "Prompt the user for the date of the next review, and insert
it as a property of the headline."
  (interactive nil org-mode org-agenda-mode)
  (require 'org-review)
  (let ((ts (format-time-string (car org-time-stamp-formats) (org-read-date nil t))))
    (org-review-insert-date "NEXT_VISIBLE"
                            'inactive ts)))

(defun krisb-org-review-remove ()
  "Un-review the current heading.
Removes the properties denoted by `org-review-next-property-name' and
`org-review-last-property-name'."
  (interactive)
  (if-let* ((props (org-entry-properties))
            (present-props
             (remq nil
                   (mapcar
                    (lambda (p)
                      (member (car p) '("NEXT_REVIEW" "NEXT_VISIBLE")))
                    props)))
            (prop-name
             (completing-read "Remove property:" present-props)))
      (org-delete-property prop-name)
    (message "No review properties present in entry")))

(with-eval-after-load 'org
  (bind-keys :map org-mode-map
             ("C-c r v" . krisb-org-review-insert-next-visible)
             ("C-c r u" . krisb-org-review-remove)))

(with-eval-after-load 'org-agenda
  (bind-keys :map org-agenda-mode-map
             ("C-c r v" . krisb-org-review-insert-next-visible)
             ("C-c r u" . krisb-org-review-remove)))

;;;;; Org-repeat-by-cron
(setup org-repeat-by-cron
  (:package org-repeat-by-cron)
  
  (with-eval-after-load 'org
    (global-org-repeat-by-cron-mode)))

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

;; Custom predicates.  Inspired by org-review
(with-eval-after-load 'org-ql
  (org-ql-defpred tosee ()
    "Match headings I should see now."
    :body
    (let* ((prop-val (org-entry-get (point) "NEXT_VISIBLE"))
           (next (and prop-val (org-read-date nil t prop-val))))
      (or (not next)
          (time-less-p next (current-time))))))

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

;;; Tramp
;; TODO 2025-11-28: Document this:
;; - `tramp-verbose'
;; - `tramp-syntax' - See (info "(tramp) Change file name syntax")
(setup tramp

  ;; Tip: for a list of general ways to speed up TRAMP, see (info
  ;; "(tramp) Frequently Asked Questions").  Many/most of the
  ;; selections below were taken from there.
  (with-eval-after-load 'tramp
    (setopt tramp-verbose 1
            ;; 2025-12-06: Increase the cutoff to use external
            ;; methods.  Recommendation taken from testing of
            ;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
            tramp-copy-size-limit (* 1024 1024)) ; 1MB

    ;; FIXME 2025-11-30: Not sure why this "davs"is causing problems
    ;; for me... disable it for now.
    (setopt tramp-gvfs-methods (cl-remove "davs" tramp-gvfs-methods :test #'string=))
    
    ;; SSH-relevant settings.  See (info "(tramp) Ssh setup") for more
    ;; information
    (setopt tramp-use-connection-share t
            ;; Most significant speedup for TRAMP, since it opens many
            ;; SSH connections
            tramp-ssh-controlmaster-options ; Only when `tramp-use-connection-share' is t
            (concat "-o ControlMaster=auto "
                    "-o ControlPersist=30m "
                    ;; See the ssh_config(5) man page for a list of
                    ;; the possible tokens.  NOTE: %% is encoded as %
                    "-o ControlPath=/tmp/tramp-cm-%%r@%%h:%%p")
            ;; Only effects copying between two remote hosts: when
            ;; non-nil, to copy a file from remote1 to remote 2,
            ;; instead of copying to the local machine then copying
            ;; again to remote 2, we copy directly from remote1 and
            ;; remote2.
            ;; 
            ;; Assumes we have password-less authentication into
            ;; host. For more details see section "4.19.4 Configure
            ;; direct copying between two remote servers" in (info
            ;; "(tramp) Ssh setup")
            tramp-use-scp-direct-remote-copying t
            remote-file-name-inhibit-cache nil))

  ;; Guix-specific
  ;; Add the user's load path to `tramp-remote-path' so that TRAMP can
  ;; search the directories Guix adds to PATH
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

;; User- and host-specific configurations
(with-eval-after-load 'tramp
  
  ;; Don't show symlinks, since that requires the expensive
  ;; `file-truename'.  Taken from (info "(tramp) Frequently Asked
  ;; Questions")
  (connection-local-set-profile-variables
   'krisb-dired-no-symlink
   '((dired-check-symlinks . nil)))
  (connection-local-set-profiles
   '(:application tramp :machine "sublation")
   'krisb-dired-no-symlink)

  ;; Use "direct async processes": create sub-processes directly on
  ;; the host.  See 5.6.10 Improving performance of asynchronous
  ;; remote processes in (info "(tramp) Remote processes").  I
  ;; learned of this from
  ;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./#use-direct-async
  ;;
  ;; Be aware of the limitations described in the aforementioned
  ;; manual page, including:
  ;; - Only works for some methods,
  ;; - Does not support interactive user authentication
  ;; - Cannot be applied for ssh-based methods that use the
  ;;   RemoteCommand option.
  ;; - May fail when when the command is too long (e.g. long
  ;;   directory names or long PATH that is set)
  (connection-local-set-profile-variables
   'krisb-remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :user "krisbalintona" :machine "sublation")
   'krisb-remote-direct-async-process)

  ;; NOTE 2026-01-04: Set `shell-file-name' specially.  This is a
  ;; workaround for several problems:
  ;;
  ;; 1. Because tramp shells are non-login, they do not source
  ;;    ~/.profile, which is populated by the setup-environment script
  ;;    in e.g. ~/.guix-home, which
  ;;    `home-environment-variables-service-type' adds to.  As a
  ;;    workaround, we manually source ~/.profile for created
  ;;    interactive shells (e.g., `shell', `eat').
  ;;
  ;; 2. I use fish but currently have the SHELL for my sublation machine
  ;;    bash.  The reason is issues using tramp on this machine with a
  ;;    non-POSIX shell.  As a workaround, I start the fish shell
  ;;    manually (only for created interactive shells like `shell' and
  ;;    `eat').
  ;;
  ;; 3. Set the LC_ALL env var in order to render unicode properly.
  ;;    (Setting LC_ALL might be overkill, but I wasn't able to
  ;;    determine which specific LC_* variable suffices for this end.)
  (connection-local-set-profile-variables
   'krisb-remote-source-profile
   `((shell-file-name . "/bin/sh -c 'source ~/.profile && exec $(which fish)'")
     (tramp-remote-process-environment
      ;; FIXME 2026-01-04: Avoid hardcoding locale somehow?
      . ,(cons "LC_ALL=en_US.utf8" tramp-remote-process-environment))))
  (connection-local-set-profiles
   '(:application tramp :user "krisbalintona" :machine "sublation")
   'krisb-remote-source-profile))

;;; Tramp-hlo
(setup tramp-hlo
  (:pin tramp-hlo "gnu-elpa-devel")
  (:package tramp-hlo)
  
  (autoload 'tramp-hlo-setup "tramp-hlo")
  (with-eval-after-load 'tramp
    (tramp-hlo-setup)))

;;; Tramp-theme
;; NOTE: I learned about this package from (info "(tramp) Frequently
;; Asked Questions").
(setup tramp-theme
  (:package tramp-theme)

  (with-eval-after-load 'tramp
    (load-theme 'tramp)))

;;; Tramp-rpc
(setup tramp-rpc
  (:package (tramp-rpc :url "https://github.com/ArthurHeymans/emacs-tramp-rpc.git"))

  (with-eval-after-load 'tramp-rpc
    (setopt tramp-rpc-deploy-backend 'python)))

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
             ([remap repeat-complex-command] . consult-complex-command)
             :map goto-map              ; The `M-g' prefix
             ("o" . consult-outline)
             ("e" . consult-compile-error)
             ("l" . consult-line)
             ("a" . consult-org-agenda)
             ("m" . consult-mark)
             ("F" . consult-flymake)
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
              ("/" . consult-narrow-help)) ; Show available narrow keys

  (setopt consult-preview-key "C-M-;"
          consult-ripgrep-args
          (concat
           "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --search-zip"
           ;; Additional args
           " --line-number --hidden"))

  ;; `consult-bookmark-narrow'
  (with-eval-after-load 'consult
    (with-eval-after-load 'activities
      (add-to-list 'consult-bookmark-narrow '(?a "Activities" activities-bookmark-handler)))
    (with-eval-after-load 'pdf-tools
      (add-to-list 'consult-bookmark-narrow '(?p "PDFs" pdf-view-bookmark-jump-handler))))

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

(add-to-list 'display-buffer-alist
             `((major-mode . diff-mode)
               (display-buffer-reuse-mode-window
                display-buffer-in-previous-window
                display-buffer-use-least-recent-window
                display-buffer-pop-up-window)))

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
    (add-hook hook (lambda ()
                     (unless (and (boundp 'elisp-fontify-semantically)
                                  (buffer-local-value 'elisp-fontify-semantically (current-buffer)))
                       (highlight-function-calls-mode 1)))))

  (with-eval-after-load 'highlight-function-calls
    (setopt highlight-function-calls-not t
            highlight-function-calls-macro-calls t
            highlight-function-calls-special-forms t))

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

  (with-eval-after-load 'eat
    ;; Set TERM to xterm-256color in order for certain functionality
    ;; to be match normal terminals, like avoiding "WARNING: terminal
    ;; is not fully functional" messages.  (Haven't noticed any
    ;; unexpected behavior from this yet.)
    (setopt eat-term-name "xterm-256color"))

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
  (with-eval-after-load 'eat
    (with-eval-after-load 'fontaine
      (add-hook 'fontaine-set-preset-hook #'krisb-eat--setup))))

;; Integration with eshell
(setup eat
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

;; Integration with vc.el
(setup eat
  (with-eval-after-load 'vc
    ;; NOTE 2025-11-13: I've created an issue to get this into
    ;; upstream: https://codeberg.org/akib/emacs-eat/issues/241
    (add-to-list 'vc-deduce-backend-nonvc-modes 'eat-mode)))

;; REVIEW 2025-12-11: Consider contributing upstream.  However,
;; currently, the project author has not been active.  Many issues and
;; PRs have gone unreviewed/unnoticed for around a year now.
;; Fix buffer names for remote projects
(with-eval-after-load 'project
  (el-patch-defun project-prefixed-buffer-name (mode)
    (concat "*"
            (when-let* ((remote (file-remote-p default-directory 'host)))
              ;; Similar format as `mode-line-buffer-identification'
              ;; does for remote buffers
              (concat remote ": "))
            (if-let* ((proj (project-current nil)))
                (project-name proj)
              (file-name-nondirectory
               (directory-file-name default-directory)))
            "-"
            (downcase mode)
            "*")))

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
            olivetti-margin-width 8))
            
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

;;; Bookmark
;; TODO 2025-11-28: Document:
;; - `bookmark-use-annotations'
;; - `bookmark-version-control'
(setup bookmark
  
  (with-eval-after-load 'bookmark
    (setopt bookmark-save-flag 1
            bookmark-sort-flag 'last-modified)))

(add-to-list 'display-buffer-alist
             ;; NOTE 2025-12-03: We also match the default buffer name
             ;; since only matching against the major mode only works
             ;; after bookmark.el has been loaded, for some reason.
             ;; (Maybe related to how `derived-mode-p' works?)
             '((or "\\*Bookmark List\\*"
                   (major-mode . bookmark-bmenu-mode))
               (display-buffer-in-new-tab)))

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
  (with-eval-after-load 'message
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
            message-forward-before-signature nil))

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

  (setopt footnote-prefix [(control ?c) ?f])
  (with-eval-after-load 'footnote
    (setopt footnote-spaced-footnotes nil
            footnote-mode-line-string "FN"
            footnote-prompt-before-deletion nil)))

;;; Notmuch
;; TODO 2025-05-23: Document:
;; - `notmuch-identities’
(setup notmuch
  (:package notmuch)
  ;; For AUR:
  ;; :ensure-system-package (notmuch
  ;;                         (gmi . lieer-git))

  (bind-keys :map krisb-open-keymap
             ("n" . notmuch))
  (:bind-keys :map notmuch-search-mode-map
              ("a" . nil)           ; The default is too easy to hit accidentally
              ("/" . notmuch-search-filter)
              ("r" . notmuch-search-reply-to-thread)
              ("R" . notmuch-search-reply-to-thread-sender)
              :map notmuch-tree-mode-map
              ("S-SPC" . notmuch-tree-scroll-message-window-back)
              ("r" . notmuch-tree-reply)
              ("R" . notmuch-tree-reply-sender)
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
  (let ((emacs-devel "List:emacs-devel")
        (emacs-bugs "List:bug-gnu-emacs")
        (guix-devel "List:guix-devel")
        (guix-help "List:help-guix")
        (guix-patches "List:guix-patches")
        (guix-bugs "List:bug-guix")
        (mumi-bugs "List:bug-mumi")
        (rde "List:\"~abcdw/rde-discuss@lists.sr.ht\"")
        (notmuch "List:notmuch.notmuchmail.org"))
    (setopt notmuch-saved-searches
            `(( :name "inbox"
                :query "tag:inbox and (not tag:list or tag:watch)"
                :sort-order oldest-first
                :key "i")
              ( :name "Emacs mailing lists"
                :query ,(format "(%s or %s) and tag:inbox"
                                emacs-devel emacs-bugs)
                :sort-order newest-first
                :key "e")
              ( :name "Guix mailing lists"
                :query ,(format "(%s or %s or %s or %s) and tag:inbox"
                                guix-devel guix-help guix-patches guix-bugs)
                :sort-order newest-first
                :key "g")
              ( :name "Other mailing lists"
                :query ,(format "(%s or %s or %s) and tag:inbox"
                                mumi-bugs rde notmuch)
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
                :key "t"))))
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
          notmuch-address-use-company nil
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
    (setopt sendmail-program (or (executable-find "gmi") "sendmail")
            send-mail-function 'sendmail-send-it))
  
  ;; GMail already copies sent emails, so don't move them elsewhere
  ;; locally after sending; see (emacs) Mail Headers for an explanation
  ;; of the FCC header.  (Using gmi to send emails also automatically
  ;; does this for us locally.)
  (with-eval-after-load 'notmuch
    (setopt notmuch-fcc-dirs nil))
  
  ;; Set sendmail args appropriate to using lieer as `sendmail-program'
  (defun krisb-notmuch-set-sendmail-args ()
    "Modify `message-sendmail-extra-arguments' to send emails via gmi.
  Set `message-sendmail-extra-arguments' such that, when
  `sendmail-program' is the path to a gmi executable, the arguments passed
  to `sendmail-program' are modified buffer-locally.  They are modified
  such that gmi sends the email and stores the sent email in the
  appropriate mail directory."
    (when (and (stringp sendmail-program) (string-match-p "gmi" sendmail-program))
      (let* ((from (downcase (message-fetch-field "from")))
             (root-maildir krisb-email-directory)
             ;; These maildirs are according to the structure in my
             ;; local filesystem
             (personal-maildir (expand-file-name "personal" root-maildir))
             (uni-maildir (expand-file-name "uni" root-maildir)))
        ;; See `message-send-mail-with-sendmail' for the relation
        ;; between `sendmail-program' and
        ;; `message-sendmail-extra-arguments'
        (setq-local message-sendmail-extra-arguments
                    (list "send" "--quiet" "-t" "-C"
                          (cond
                           ((string-match-p "krisbalintona@gmail\\.com" from)
                            personal-maildir)
                           ((string-match-p "kristoffer_balintona@alumni\\.brown\\.edu" from)
                            uni-maildir)))))))
  (with-eval-after-load 'message
    (add-hook 'message-send-mail-hook #'krisb-notmuch-set-sendmail-args))
  
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
  
  ;; Asynchronously update count for relevant mail.  We do so
  ;; asynchronously since "notmuch count" can sometimes cause a
  ;; noticeable delay
  (defvar krisb-display-time-mail-count 0
    "Count for relevant unread mail.")
  
  (defun krisb-display-time--update-mail-count ()
    "Update the *notmuch count* buffer.
  This buffer\\='s content is the the number of relevant emails currently
  unread.
  
  Return the \"notmuch count\" process spawned."
    (with-temp-buffer
      (let ((buf (get-buffer-create "*notmuch count*"))
            result)                       ; Store result for sentinel
        (make-process
         :name "notmuch-count"
         :buffer buf
         :noquery t
         :command (list "notmuch"
                        "count" "tag:inbox" "and"
                        "tag:unread" "and"
                        "not" "tag:list" "and"
                        "not" "tag:sub")
         :filter (lambda (_proc chunk)
                   ;; Store result for (lexically-bound) retrieval by
                   ;; sentinel
                   (setq result (concat result chunk)))
         :sentinel (lambda (proc event)
                     (when (string= event "finished\n")
                       ;; Set cached mail count
                       ;; (`krisb-display-time-mail-count') to value
                       ;; stored in RESULT
                       (setq krisb-display-time-mail-count (string-to-number result))))))))
  
  ;; Update mail count every 120 seconds.  Although, only
  ;; `krisb-display-time-mail-count' is updated; the mode line is not.
  ;; To update the mode line as well, call `display-time-update'
  ;; afterwards
  (run-with-timer 0 120 #'krisb-display-time--update-mail-count)
  
  (defun krisb-display-time-mail-count ()
    "Returns non-nil when there is relevant mail.
  Meant to be the value of `display-time-mail-function'."
    (when-let ((buf (get-buffer "*notmuch count*")))
      (with-current-buffer buf
        (< 0 krisb-display-time-mail-count))))
  
  ;; TODO 2025-05-26: Setting `read-mail-command’ to `notmuch’ doesn’t
  ;; seem to succeed as the command run when clicking the mode line mail
  ;; string?
  (setopt display-time-mail-string krisb-display-time-mail-string
          display-time-mail-function #'krisb-display-time-mail-count
          display-time-mail-face 'font-lock-keyword-face)
  
  ;; FIXME 2025-05-26: This assumes that we always leave notmuch via the
  ;; notmuch-hello buffer.  This is a workaround because I know of no
  ;; other reliable indiaction of when I’m done checking mail.  Is there
  ;; something better?
  ;;
  ;; Advise `notmuch-bury-or-kill-this-buffer’ such that it updates
  ;; after leaving the notmuch-hello buffer.  This prevents the mail
  ;; string from being visible right after we’ve just checked mail in
  ;; notmuch
  (defun krisb-notmuch-hello-update-mail-count-advice (&rest args)
    "Update mode line mail count immediately.
  The mail count is the number stored in the variable
  `krisb-display-time-mail-count'.  It is updated by calling
  `krisb-display-time--update-mail-count'.  We then call
  `display-time-update' to update the mode line to reflect the new value.
  
  Our approach is to call `display-time-update' at the end of the existing
  sentinel of `krisb-display-time--update-mail-count'.
  
  Meant to be added as :around advice for
  `notmuch-bury-or-kill-this-buffer'.  ARGS are the arguments passed to
  `notmuch-bury-or-kill-this-buffer'."
    (when (equal major-mode 'notmuch-hello-mode)
      (let* ((process (krisb-display-time--update-mail-count))
             (initial-sentinel (process-sentinel process)))
        (set-process-sentinel process
                              (lambda (proc event)
                                (when initial-sentinel
                                  (funcall initial-sentinel proc event))
                                (when (string= event "finished\n")
                                  (display-time-update))))))
    (apply args))
  (with-eval-after-load 'notmuch
    (advice-add 'notmuch-bury-or-kill-this-buffer :around #'krisb-notmuch-hello-update-mail-count-advice)))

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
                   "* TODO [#E] Review subscription/newsletter email: [[%L][\"%:subject\"]] %? :email:INBOX:%^g\n\nFrom %:from\nTo: %:to\n"
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

;;; Notmuch-bookmarks
(setup notmuch-bookmarks
  (:package notmuch-bookmarks)

  (with-eval-after-load 'bookmark
    (notmuch-bookmarks-mode)))

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
  
  ;; Element that expands other templates by name, e.g., (i header)
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
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'krisb-tempel-setup-capf)))

;;; Jinx


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
    (setopt org-mime-export-ascii 'ascii
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
                  start)))
            org-mime-instructions-hint "# org-mime hint: Press C-c C-c to commit change.\n"
            org-mime-obey-display-buffer-p t)

    (defun krisb-org-mime-setup ()
      "Nicely offset block quotes in email bodies.
Taken from
https://github.com/org-mime/org-mime?tab=readme-ov-file#css-style-customization."
      (org-mime-change-element-style
       "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))
    (add-hook 'org-mime-html-hook #'krisb-org-mime-setup))

  (add-to-list 'display-buffer-alist
               '((or "OrgMimeMailBody"
                     ;; The major mode for org-mime org buffers is
                     ;; `org-mode' with the `org-mime-src-mode' minor
                     ;; mode enabled
                     (and (major-mode . org-mode)
                          (lambda (buffer-or-name &rest _args)
                            (when (fboundp 'org-mime-src-mode)
                              (buffer-local-value 'org-mime-src-mode
                                                  (get-buffer buffer-or-name))))))
                 (display-buffer-same-window))))

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
      (add-hook 'dictionary-mode-hook #'hide-mode-line-mode))))

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
;; TODO 2025-11-16: Document:
;; - `flyspell-check-changes'
(setup flyspell
  
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'conf-mode-hook #'flyspell-prog-mode)

  (:hide-mode)
  
  (with-eval-after-load 'flyspell
    (setopt flyspell-issue-welcome-flag nil
            flyspell-issue-message-flag nil ; Greatly increase `flyspell-buffer' speed
            flyspell-abbrev-p t
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
                   (display-buffer-reuse-window
                    display-buffer-reuse-mode-window
                    display-buffer-below-selected
                    display-buffer-at-bottom)
                   (window-height . 0.25)))
    (add-to-list 'display-buffer-alist
                 '(((category . xref)
                    (display-buffer-reuse-window
                     display-buffer-reuse-mode-window
                     display-buffer-use-some-window)
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
  ;; We use `cape-dict' as an alternative, adding it where needed.
  (setopt text-mode-ispell-word-completion nil)

  ;; Capfs added to the end of the global value of
  ;; `completion-at-point-functions'.  Consequently, they act as
  ;; fallback backends.
  (dolist (capf (reverse '(cape-file cape-elisp-symbol cape-dict)))
    (add-hook 'completion-at-point-functions capf))
  
  ;; Macro to help adding capfs via major mode hooks
  (defmacro krisb-cape-setup-capfs (label hooks capfs &rest body)
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
`completion-at-point-functions'.

Finally, when BODY is non-nil, evaluate these forms.  This argument is
intended to evaluate forms relevant to `completion-at-point' and related
functionality."
    (declare (indent 0))
    (let ((func (intern (concat "krisb-cape-setup-capfs-" label))))
      `(progn
         (defun ,func ()
           "Modifies `completion-at-point-functions' buffer-locally."
           (dolist (capf (reverse ,capfs))
             (add-hook 'completion-at-point-functions capf -50 t))
           ,@body)
         (dolist (hook ,hooks)
           (add-hook hook #',func)))))

  ;; Add capfs by major-mode
  (krisb-cape-setup-capfs
    "elisp"
    '(emacs-lisp-mode-hook lisp-interaction-mode-hook)
    (list #'cape-file #'cape-elisp-symbol))

  (krisb-cape-setup-capfs
    "commit"
    '(git-commit-setup-hook log-edit-mode-hook)
    (list #'cape-elisp-symbol)
    ;; Also change the delimiters inserted by `cape-elisp-symbol'
    ;; depending on the project I'm in
    ;; TODO 2025-11-30: Figure out which directories I want to do what
    ;; and program that behavior accordingly
    (require 'cape)
    (make-local-variable 'cape-elisp-symbol-wrapper)
    (when-let* ((project (project-current)))
      (pcase (expand-file-name (project-root project))
        ((pred (string-match-p "emacs-repos/packages/emacs/"))
         (setq cape-elisp-symbol-wrapper
               (cons '(log-edit-mode ?' ?')
                     (assq-delete-all 'log-edit-mode cape-elisp-symbol-wrapper))))
        (_
         (setq cape-elisp-symbol-wrapper
               (cons '(log-edit-mode ?` ?`)
                     (assq-delete-all 'log-edit-mode cape-elisp-symbol-wrapper)))))))
  
  (krisb-cape-setup-capfs
    "shells"
    '(eshell-mode-hook comint-mode-hook)
    (list #'cape-file #'cape-history)))

;; Make eglot's capf non-exclusive
(setup cape
  (with-eval-after-load 'cape
    (with-eval-after-load 'eglot
      (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive))))

;;; Project
;; TODO 2025-05-22: Document:
;; - `project-vc-extra-root-markers’
;; - `project-kill-buffer-conditions'
(setup project
  (:pin project "gnu-elpa-devel")
  (:package project)

  (with-eval-after-load 'project
    (setopt project-file-history-behavior 'relativize
            ;; FIXME 2025-11-23: I have to use `file-relative-name'
            ;; since `project-root' returns abbreviated file paths.
            ;; Create a bug report/patch to have file paths be
            ;; expanded?
            project-list-exclude (list (file-relative-name package-user-dir user-emacs-directory))
            ;; project-list-exclude nil
            project-compilation-buffer-name-function 'project-prefixed-buffer-name
            project-vc-merge-submodules nil)) ; Respect subprojects as their own projects
  
  ;; Killing projects
  (with-eval-after-load 'project
    (setopt project-kill-buffers-display-buffer-list t))

  ;; Mode line
  (with-eval-after-load 'project
    (setopt project-mode-line t
            project-mode-line-face 'italic)
  
  ;; Keybindings
  (with-eval-after-load 'project
    ;; The commands in `project-switch-commands' must be found in
    ;; `project-prefix-map'
    (setopt  project-switch-commands
             `((project-find-file "Find file")
               (project-switch-to-buffer "Switch to buffer")
               (project-find-regexp "Find regexp")
               (project-find-dir "Find directory")
               (project-vc-dir "VC-Dir")
               (project-eshell "Eshell")
               (project-shell "Shell")
               (project-any-command "Other")))
    
    (bind-keys :map project-prefix-map
               ("e" . project-eshell)
               ("C" . project-recompile))
    (add-to-list 'project-switch-commands '(project-compile "Compile"))
    (add-to-list 'project-switch-commands '(project-recompile "Recompile"))

    ;; Overshadow `project-shell’ in keybindings with `eat-project'
    (bind-keys :map project-prefix-map
               ("s" . eat-project))
    (cl-nsubstitute '(eat-project "EAT") 'project-shell project-switch-commands
                    :key #'car))))

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

  ;; Tramp has its own backup-directory-alist
  ;; (`tramp-backup-directory-alist'); use `backup-directory-alist'
  ;; instead
  (with-eval-after-load 'tramp
    ;; TODO 2025-12-12: Revisit this.
    ;; (setopt tramp-backup-directory-alist backup-directory-alist)
    )

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

;;; Word-wrap-mode
;; Instead of wrapping lines on whitespace, as is the default, also
;; wrap lines on the characters denoted by
;; `word-wrap-whitespace-characters’, e.g., em-dashes and en-dashes.
(setup word-wrap-mode
  
  (global-word-wrap-whitespace-mode 1))

;;; Flymake
;; TODO 2025-05-24: Document:
;; - `elisp-flymake-byte-compile-load-path’
;; - `flymake-suppress-zero-counters’
(setup flymake
  
  ;; TODO 2025-05-24: Revisit this.
  ;; (prog-mode-hook . (lambda ()
  ;;                     (setq-local flymake-indicator-type nil
  ;;                                 flymake-show-diagnostics-at-end-of-line 'fancy) ; Emacs 31 value
  ;;                     (flymake-mode 1)))
  ;; TODO 2025-11-17: Revisit this.
  ;; (text-mode-hook . (lambda ()
  ;;                     (setq-local flymake-indicator-type nil)
  ;;                     (flymake-mode 1)))

  (add-hook 'prog-mode-hook #'flymake-mode)
  (add-hook 'text-mode-hook #'flymake-mode)
  
  (setopt flymake-wrap-around nil)
  
  ;; TODO 2025-11-17: Revisit this.
  ;; ;; Indicators
  ;; (setopt flymake-indicator-type nil
  ;;         flymake-fringe-indicator-position nil ; Position for fringe position type
  ;;         flymake-margin-indicator-position 'right-margin ; Position for margin position type
  ;;         flymake-show-diagnostics-at-end-of-line nil)

  ;; Mode line
  (setopt flymake-mode-line-format
          '(" " flymake-mode-line-title flymake-mode-line-exception flymake-mode-line-counters)
          flymake-mode-line-counter-format
          '("["
            flymake-mode-line-error-counter
            flymake-mode-line-warning-counter
            flymake-mode-line-note-counter
            "]"))
  (setq flymake-mode-line-counters
        '(:eval (if (mode-line-window-selected-p)
                    (flymake--mode-line-counters)
                  (propertize (format-mode-line (flymake--mode-line-counters))
                              'face '(:inherit (bold mode-line-inactive)))))))

;;; Flymake-vale
(setup flymake-vale
  (:package (flymake-vale :url "https://github.com/tpeacock19/flymake-vale.git"))
  
  (add-hook 'text-mode-hook #'flymake-vale-load))

;;; Package-guard-upgrade


;;; Sendmail
;; Use the `sendmail' program to send emails?  If yes, set the value
;; of `send-mail-function' to `sendmail-send-it'
(setup sendmail

  ;; Make sure that emails are sent from the email address specified
  ;; in the "From" header field.  Is sufficient when
  ;; `message-sendmail-envelope-from' is 'obey-mail-envelope-from
  ;; (default).  Alternatively, we can set
  ;; `message-sendmail-envelope-from' to 'header.  (I prefer the
  ;; latter approach, since specifies behavior specific to sending
  ;; emails.)
  (setopt mail-specify-envelope-from t
          mail-envelope-from 'header))

;;; Smtpmail
;; Use `msmtp' program to send emails?  If yes, set the value of
;; `send-mail-function' to `smtpmail-send-it'
(setup smtpmail
  ;; For AUR:
  ;; :ensure-system-package msmtp
  
  ;; Queuing emails
  (setopt smtpmail-queue-mail nil)
  ;; Make sure email details that are used are not the current
  ;; variables (the variables set when sending queued emails), but the
  ;; variables used when writing the email
  (setopt smtpmail-store-queue-variables t
          smtpmail-queue-dir (expand-file-name "smtp-queue/" krisb-email-drafts-directory))
  
  ;; Below are the settings associated with GMail.  See
  ;; https://support.google.com/mail/answer/7126229?hl=en#zippy=%2Cstep-change-smtp-other-settings-in-your-email-client
  (setopt smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587
          smtpmail-stream-type 'starttls
          ;; 2024-08-25: Fixes Gmail's 530 error on sending
          smtpmail-servers-requiring-authorization "gmail"))

;;; Work-timer
(setup work-timer
  (:package (work-timer :url "git@github.com:krisbalintona/work-timer.git"))
  
  ;; FIXME 2025-11-20: I don't know how to `bind-key' before a keymap
  ;; is defined.  For now I :require, but this isn't ideal.
  (:require)
  (bind-key "C-c w" work-timer-prefix-map)

  (with-eval-after-load 'work-timer
    (setopt work-timer-work-duration-function 'work-timer-work-duration-fractional
            work-timer-break-duration-function 'work-timer-break-duration-fractional
            work-timer-fractional-break-duration-fraction 0.35))

  (with-eval-after-load 'work-timer
    (work-timer-with-org-clock-mode 1)))

;;; Foldout
(setup foldout

  ;; FIXME 2025-11-18: This is a defvar when it should be defcustom.
  ;; Create a patch upstream or submit a bug report?
  (setq foldout-inhibit-key-bindings t)
  (:bind-keys :map outline-mode-map
              ("C-c C-z" . foldout-zoom-subtree)
              ("C-c C-x" . foldout-exit-fold)
              ("C-c C-w" . foldout-widen-to-current-fold))
  
  (with-eval-after-load 'outline
    (:require)))

;;; Citar
;; TODO 2025-11-18: Document:
;; - `citar-open-entry-function'
(setup citar
  (:package citar)
  (:load-after oc)
  
  (bind-keys ("C-c b o" . citar-open)
             ("C-c b f" . citar-open-files)
             ("C-c b n" . citar-open-notes))
  
  (with-eval-after-load 'citar
    (setopt citar-bibliography krisb-bibliography-files
            citar-notes-paths (list krisb-notes-directory)
            citar-default-action #'citar-open-files))

  ;; TODO 2025-11-18: Revisit this.
  ;; ;; Fancy UI.
  ;; (setopt citar-templates
  ;;         ;; See also citar-format.el for more information on what
  ;;         ;; happens with the templates.
  ;;         '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}") ; Candidate
  ;;           (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}") ; Candidate annotation
  ;;           (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n") ; Formatted reference
  ;;           (note . "${title} by ${author}"))) ; New note title
  ;; (with-eval-after-load 'all-the-icons
  ;;   ;; Taken from https://github.com/emacs-citar/citar/wiki/Indicators
  ;;   (defvar citar-indicator-files-icons
  ;;     (citar-indicator-create
  ;;      :symbol (all-the-icons-faicon
  ;;               "file-o"
  ;;               :face 'all-the-icons-green
  ;;               :v-adjust -0.1)
  ;;      :function #'citar-has-files
  ;;      :padding "  " ; Need this because the default padding is too low for these icons
  ;;      :tag "has:files"))
  ;;   (defvar citar-indicator-links-icons
  ;;     (citar-indicator-create
  ;;      :symbol (all-the-icons-octicon
  ;;               "link"
  ;;               :face 'all-the-icons-orange
  ;;               :v-adjust 0.01)
  ;;      :function #'citar-has-links
  ;;      :padding "  "
  ;;      :tag "has:links"))
  ;;   (defvar citar-indicator-notes-icons
  ;;     (citar-indicator-create
  ;;      :symbol (all-the-icons-material
  ;;               "speaker_notes"
  ;;               :face 'all-the-icons-blue
  ;;               :v-adjust -0.3)
  ;;      :function #'citar-has-notes
  ;;      :padding "  "
  ;;      :tag "has:notes"))
  ;;   (defvar citar-indicator-cited-icons
  ;;     (citar-indicator-create
  ;;      :symbol (all-the-icons-faicon
  ;;               "circle-o"
  ;;               :face 'all-the-icon-green)
  ;;      :function #'citar-is-cited
  ;;      :padding "  "
  ;;      :tag "is:cited"))
  ;;   (setq citar-indicators
  ;;         (list citar-indicator-files-icons
  ;;               citar-indicator-links-icons
  ;;               citar-indicator-notes-icons
  ;;               citar-indicator-cited-icons)))
  )

;; Integrate citar with org and org-cite
(setup citar-org
  (:if-feature citar)
  (:load-after citar)

  (setopt org-cite-insert-processor 'citar
          org-cite-follow-processor 'citar
          org-cite-activate-processor 'citar
          citar-org-styles-format 'long)

  (with-eval-after-load 'org
    (:bind-keys :map org-mode-map
                ([remap org-cite-insert] . citar-insert-citation))))

;;; Citar-org-node
(setup citar-org-node
  (:package (citar-org-node :url "https://github.com/krisbalintona/citar-org-node.git"))
  (:if-package citar)
  (:if-package org-node)
  
  (bind-keys :map krisb-note-keymap
             ("b a" . citar-org-node-add-refs)
             ("b o" . citar-org-node-open-resource))
  
  (with-eval-after-load 'citar
    (citar-org-node-mode 1))
  (:hide-mode citar-org-node-mode))

;;; Bibtex
(setup bibtex
  
  (with-eval-after-load 'bibtex
    ;; Biblatex entries and fields.  See the biblatex manual for all
    ;; the information possible about about biblatex types, fields,
    ;; and so on:
    ;; https://mirror.clarkson.edu/ctan/macros/latex/contrib/biblatex/doc/biblatex.pdf
    (setopt bibtex-dialect 'biblatex
            bibtex-biblatex-aux-entry-alist
            ;; NOTE: The order of fields in an entry matters: they are
            ;; the order that fields are sorted in when calling
            ;; `bibtex-reformat' with `bibtex-entry-format' including
            ;; 'sort-fields
            '(("Review" "Review of some other work" "Article")
              ("Movie" "A motion picture"
               (("title")
                ("date" nil nil 1)
                ("year" nil nil -1))
               nil
               (("titleaddon" "An annex to the title, to be printed in a different font")
                ("subtitle")
                ;; Credits
                ("author" nil nil 2)
                ("director" nil nil -2)
                ("producer")
                ("writer" "Writer of the screenplay")
                ("scriptwriter")
                ("editor")
                ("editortype")
                ("editora")
                ("editoratype")
                ("editorb")
                ("editorbtype")
                ("editorc")
                ("editorctype")
                ;; Event
                ("eventtitle")
                ("eventdate")
                ;; Local attachments
                ("file")
                ;; Indexing
                ("keywords")
                ("abstract")))
              ("Video" "An audiovisual recording"
               (("title")
                ("date" nil nil 1)
                ("year" nil nil -1))
               nil
               (("author" nil nil 2)
                ("organization" nil nil -2)
                ("publisher" nil nil -2)
                ("institution" nil nil -2)
                ("editor")
                ("editortype")
                ("editora")
                ("editoratype")
                ("editorb")
                ("editorbtype")
                ("editorc")
                ("editorctype")
                ;; Platform/medium
                ("howpublished" nil nil 3)
                ("type" nil nil -3)
                ;; Attachments
                ("file")
                ;; Indexing
                ("keywords")
                ("abstract"))))
            bibtex-biblatex-aux-opt-alist
            '(("entrysubtype" "Subtype of an entry type")))
    
    ;; Standardize entries' formatting and order
    (setopt bibtex-sort-entry-class
            ;; I use the biblatex dialect (see `bibtex-dialect'), so
            ;; see `bibtex-biblatex-entry-alist' and
            ;; `bibtex-biblatex-aux-entry-alist'
            '(("String")
              ("Unpublished" "PhdThesis")
              ("Article" "Proceedings" "InProceedings")
              ("Booklet" "Book")
              ("InCollection" "InBook")
              ("Online" "Video" "Movie")
              (catch-all))
            bibtex-maintain-sorted-entries 'entry-class
            bibtex-align-at-equal-sign t
            bibtex-unify-case-function 'downcase
            bibtex-entry-format
            '( opts-or-alts
               required-fields
               numerical-fields
               ;; page-dashes
               whitespace
               inherit-booktitle
               realign
               last-comma
               delimiters
               unify-case
               braces
               strings
               sort-fields))
    ;; NOTE 2025-11-22: I make the `fill-column' as large as possible
    ;; to avoid field values from being filled.  This effects
    ;; `bibtex-fill-field-bounds', and therefore `bibtex-fill-field'
    ;; and `bibtex-reformat' (when 'realign is in
    ;; `bibtex-entry-format').
    (add-hook 'bibtex-mode-hook (lambda () (setq-local fill-column most-positive-fixnum)))
    
    ;; Create Better BibTeX-like cite keys with
    ;; `bibtex-generate-autokey'
    (setopt bibtex-autokey-edit-before-use nil ; Trust `bibtex-generate-autokey'
            bibtex-autokey-names 1
            bibtex-autokey-names-stretch 1
            bibtex-autokey-name-case-convert-function 'identity
            bibtex-autokey-year-length 4
            bibtex-autokey-titleword-case-convert-function 'capitalize
            bibtex-autokey-title-terminators (rx unmatchable)
            bibtex-autokey-titlewords 3
            bibtex-autokey-titlewords-stretch 0
            bibtex-autokey-titleword-length 'infty
            bibtex-autokey-titleword-separator "")
    (defun krisb-bibtex-autokey-finalize (key)
      "Rearrange the parts of KEY to be in name-title-year order.q"
      (let* ((parts (split-string key ":::"))
             (authors (nth 0 parts))
             (year (nth 1 parts))
             (title (nth 2 parts)))
        (concat authors title year)))
    (setopt bibtex-autokey-name-year-separator ":::"
            bibtex-autokey-year-title-separator ":::"
            bibtex-autokey-before-presentation-function #'krisb-bibtex-autokey-finalize))

  (add-hook 'bibtex-mode-hook 'visual-line-mode))

;;; Persid
(setup persid
  (:package (persid :url "https://github.com/rougier/persid.git"))

  (with-eval-after-load 'persid
    (setopt persid-isbn-generate-citekey 'user)))

;;; Fancy-compilation
;; Document:
;; - `fancy-compilation-term'
;; Make compilation outputs in compilation buffers more pleasant to
;; see.
(setup fancy-compilation
  (:package fancy-compilation)
  
  (setopt fancy-compilation-override-colors nil
          fancy-compilation-quiet-prelude nil
          fancy-compilation-quiet-prolog nil)
  
  (fancy-compilation-mode 1))

;;; Paren
(setup paren
  
  (with-eval-after-load 'paren
    (setopt show-paren-not-in-comments-or-strings 'on-mismatch)))

;;; Delsel
(setup delsel
  
  (delete-selection-mode t))

;;; Activities


;;; Tab-bookmark
(setup tab-bookmark
  (:package (tab-bookmark :url "https://github.com/minad/tab-bookmark.git"))

  (bind-keys ("C-c m m" . tab-bookmark)
             ("C-c m s" . tab-bookmark-save)
             ("C-c m r" . tab-bookmark-rename)
             ("C-c m D" . tab-bookmark-delete)
             ("C-c m b" . tab-bookmark-open)
             ("C-c m o" . tab-bookmark-open)
             ("C-c m p" . tab-bookmark-push)
             ("C-c m P" . tab-bookmark-pop)))

;;; Inspector
;; Introspect list expressions.  This is similar in role to CEDET's
;; data-debug.el.  Also integrates with the debugging backtrace and
;; edebug (see
;; https://github.com/mmontone/emacs-inspector?tab=readme-ov-file#from-the-emacs-debugger).
(setup inspector
  (:package inspector)

  (with-eval-after-load 'inspector
    (setopt inspector-switch-to-buffer nil)) ; Use `display-buffer'

  (defun krisb-eval-expression (arg)
    "Call `eval-expression' unless called with the `-' prefix argument.
When called with `-' instead call `inspector-inspect-expression'."
    (interactive "P")
    (call-interactively (if (eq arg '-)
                            'inspector-inspect-expression
                          'eval-expression)))
  (bind-key [remap eval-expression] #'krisb-eval-expression))

(add-to-list 'display-buffer-alist
             '((major-mode . inspector-mode)
               nil
               (post-command-select-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Inspector pprint"
               (display-buffer-below-selected)
               (post-command-select-window . t)))

;;; Do-at-point
(setup do-at-point
  (:package do-at-point)
  
  (bind-key "C-;" #'do-at-point)

  (with-eval-after-load 'do-at-point
    (defun krisb-do-at-point-powerthesaurus (beg end)
      "Prompt for a synonym of region between BEG and END and replace it."
      ;; See the description of FUNC in the docstring of
      ;; `do-at-point-actions' for why BEG and END are the bounds of the
      ;; thing at point
      (let* ((query-term (buffer-substring-no-properties beg end))
             ;; See `powerthesaurus-supported-query-types' for the list
             ;; of types
             (query-type :synonyms))
        (funcall 'powerthesaurus-lookup query-term query-type beg end)))
    (add-to-list 'do-at-point-user-actions
                 '(word (?T "Thesaurus" krisb-do-at-point-powerthesaurus)))
    (add-to-list 'do-at-point-user-actions
                 '(region (?T "Thesaurus" krisb-do-at-point-powerthesaurus)))))

;;; Which-func
;; TODO 2025-12-03: Document:
;; - `which-func-display'
(setup which-func
  
  (setopt which-func-modes '(emacs-lisp-mode diff-mode)
          which-func-update-delay 1)

  (which-function-mode 1))

;;; Buff-menu
;; TODO 2025-12-03: Document:
;; - `Buffer-menu-use-frame-buffer-list'
;; - `Buffer-menu-group-sort-by'
(setup buff-menu

  (setopt Buffer-menu-human-readable-sizes t
          Buffer-menu-group-by
          '(Buffer-menu-group-by-root Buffer-menu-group-by-mode))

  ;; Addition to `display-buffer-alist'
  (add-to-list 'display-buffer-alist
               '((major-mode . Buffer-menu-mode)
                 (display-buffer-in-new-tab))))

;;; `mode-line-format'
;; TODO 2025-12-03: Document also:
;; - `mode-line-compact'
;; - `mode-line-percent-position'
;; - `mode-line-percent-position'
;; - `mode-line-position-line-format'
;; - `mode-line-position-column-line-format'
(setopt mode-line-right-align-edge 'right-fringe
        ;; TODO 2025-07-10: Ask on emacs-devel why this isn't a
        ;; defcustom.
        mode-line-defining-kbd-macro (propertize " Macro" 'face 'mode-line-emphasis)
        mode-line-format
        '("%e" mode-line-front-space
          ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-window-dedicated)
          mode-line-frame-identification
          mode-line-buffer-identification "   "
          mode-line-position
          mode-line-format-right-align
          (project-mode-line project-mode-line-format)
          (vc-mode vc-mode) "  "
          mode-line-modes
          mode-line-misc-info
          mode-line-end-spaces))

;;; Regexp-disasm
;; Learned about this project from Ihor's config
(setup regexp-disasm
  (:package (regexp-disasm :url "https://github.com/mattiase/regexp-disasm.git")))

;;; Guile
(setup guix
  (:package guix)

  (with-eval-after-load 'scheme
    (add-hook 'scheme-mode-hook #'guix-devel-mode)
    (add-hook 'scheme-mode-hook #'guix-prettify-mode)))

(setup geiser
  (:package geiser))

(setup geiser-guile
  (:package geiser-guile))

;;; Nftables-mode
(setup nftables-mode
  (:package nftables-mode))

;;; Caddyfile-mode
(setup caddyfile-mode
  (:package caddyfile-mode))

;;; Uniquify
;; TODO 2025-12-12: Document also:
;; - `uniquify-ignore-buffers-re'
;; - `uniquify-strip-common-suffix'
;; - `uniquify-after-kill-buffer-flag'

(setup uniquify
  
  (with-eval-after-load 'uniquify
    (setopt uniquify-buffer-name-style 'forward
            uniquify-trailing-separator-flag t
            ;; uniquify-min-dir-content 1
            uniquify-dirname-transform #'project-uniquify-dirname-transform)))

;;; Sops
;; TODO 2025-06-16: Document the following information.  `sops-mode’
;; and `global-sops-mode' only conditionally keep themselves enabled
;; in files encrypted with SOPS.  If it is, we can use the available
;; commands to edit the file.
(setup sops
  (:package (sops :url "https://github.com/krisbalintona/sops.git"
                  :branch "devel"))
  
  (global-sops-mode 1)
  
  (:bind-keys ("C-c e e" . sops-edit-file)
              :map sops-mode-map
              ("C-c e e" . sops-edit-file)
              ("C-c e c" . sops-save-file)
              ("C-c e C-c" . sops-save-file)
              ("C-c e k" . sops-cancel)
              ("C-c e C-k" . sops-cancel)))

;;; Yaml-mode
(setup yaml-mode
  (:package yaml-mode))

;;; Startup time
;; Message for total init time after startup
(defun krisb-startup-time ()
  "Report Emacs startup time."
  (message "Total startup time: %s" (emacs-init-time)))
(add-hook 'after-init-hook #'krisb-startup-time)
