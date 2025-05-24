;; -*- lexical-binding: t; -*-

;;; Necessary for startup and the remainder of the config

;;;; Add modules and lisp to load path
(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))

;;;; Package.el
;; TODO 2025-05-20: Because I use elpaca, "archive" my package.el
;; configuration somewhere.
;; TODO 2025-05-21: I keep this here and changed so that commands like
;; `describe-package' can be used on non-ELPA archives.
;; Initialize package resources
(setopt package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                           ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-archive-priorities '(("gnu-elpa" . 4)
                                     ("nongnu" . 3)
                                     ("gnu-elpa-devel" . 2)
                                     ("melpa" . 1))
        package-install-upgrade-built-in t

        load-prefer-newer t) ; Do not load outdated byte code files
;; (package-initialize)

;;;; Elpaca
;; Get build date for Emacs installed via Guix.  Run this before
;; elpaca is installed.  This is necessary for the Guix and Nix
;; operating systems wherein `emacs-build-time', which elpaca uses
;; internally, is not set.  See
;; https://github.com/progfolio/elpaca/wiki/Usage-with-Nix#manually-setting-elpaca-core-date
;; for the Nix version.
(defun krisb-guix-emacs-build-date ()
  "Extract YYYYMMDD from a datetime string like '2025-05-19 15:20:57.782742938 -0500'."
  (let* ((emacs-build-path (when (string-match "--prefix=\\([^ ]+\\)" system-configuration-options)
                             (match-string 1 system-configuration-options)))
         (datetime-string (shell-command-to-string (concat "stat -c %w " emacs-build-path))))
    (when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" datetime-string)
      (concat (match-string 1 datetime-string)
              (match-string 2 datetime-string)
              (match-string 3 datetime-string)))))

(unless emacs-build-time
  (setq elpaca-core-date (list (krisb-guix-emacs-build-date))))

;; Installer
(defvar elpaca-installer-version 0.11)
;; NOTE: I have manually modified `elpaca-directory' to adhere to
;; no-littering's convention to place data files in the var
;; subdirectory
(defvar elpaca-directory (expand-file-name "var/elpaca/" user-emacs-directory))
;; Support multiple Emacs versions.  Taken from
;; https://github.com/progfolio/elpaca/wiki/Using-multiple-Emacs-versions
(defvar elpaca-builds-directory (expand-file-name (concat "builds-" emacs-version) elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; TODO 2025-05-20: Add note about enabling the mode below on systems
;; that cannot create symlinks
;; (elpaca-no-symlink-mode 1)

;;;; Use-package
;; Although `use-package' is built-in starting Emacs 29.1, I should make sure
;; it's installed just in case I test/use an earlier Emacs version
(unless (featurep 'use-package)
  (if (featurep 'elpaca)
      (elpaca (use-package :wait t))
    (package-install 'use-package)))

;; TODO 2025-05-20: Document `use-package-lint' in literate config

(require 'use-package)

;; TODO 2025-05-20: Ensure that my decision below is well-documented
;; in my literate configuration.
;; In my opinion, it is better to be as explicit as possible when I
;; want functionality.  This reduces complexity in a thorough
;; configuration with cross dependencies across packages.
;; Consequently, debugging is easier and sneaky bugs are avoided.
(setopt use-package-always-ensure nil
        ;; Be explicit with deferral via e.g. through :defer, :bind,
        ;; :after, and so on
        use-package-always-defer nil
        use-package-always-demand nil
        ;; Also be explicit with hook names
        use-package-hook-name-suffix nil)

;; Let use-package collect data that is useful for introspecting my
;; Emacs session and use-package configurations
(setopt use-package-expand-minimally nil
        ;; Only be verbose when interpreted, otherwise errors are
        ;; caught at compile time
        use-package-verbose (not (bound-and-true-p byte-compile-current-file))
        ;; Compile statistics to be shown in `use-package-report'
        use-package-compute-statistics t)

;; Install elpaca and use-package integration (via use-package's
;; :ensure keyword)
(with-eval-after-load 'elpaca
  (elpaca (elpaca-use-package :wait t)
    (elpaca-use-package-mode 1)))

;; Imenu integration: create a "packages" group that searches for
;; use-package declarations
(setopt use-package-enable-imenu-support t)

;;;; No-littering
;; Set better default package paths
(use-package no-littering
  :ensure (:wait t)
  :demand t
  :init
  ;; According to the package instructions, these variables must be
  ;; set prior to loading the feature
  (eval-and-compile                 ; Ensure values don't differ at compile time
    (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory) ; Config files
          no-littering-var-directory (expand-file-name "var/" user-emacs-directory))) ; Persistent files
  ;; Ensure the directories exist
  (mkdir no-littering-etc-directory t)
  (mkdir no-littering-var-directory t)
  :config
  ;; Read docstring.  Sets more secure values for
  ;; `auto-save-file-name-transforms', `backup-directory-alist', and
  ;; `undo-tree-history-directory-alist'.
  (no-littering-theme-backups))

;;;; Set and load custom file
(setopt custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror)

;;;; Fontaine
;; Define then apply face presets
(use-package fontaine
  :ensure (:wait t) ; To have faces set ASAP during startup
  :demand t
  :custom
  (fontaine-latest-state-file (no-littering-expand-var-file-name "fontaine/fontaine-latest-state.eld"))
  (fontaine-presets
   '((default-wsl2
      :default-height 180
      :inherit iosevka-variants)
     (iosevka-variants
      ;; NOTE 2025-04-14: On Arch Linux, Iosevka fonts have associated packages
      ;; for each variant in the AUR (though not necessarily the Nerd Fonts
      ;; versions).
      :default-family "IosevkaSS 11 Nerd Font" ; 2025-04-14: Must be a bug that there is a space between "SS" and "11" in the font name
      :fixed-pitch-family "Iosevka Nerd Font"
      :term-family "IosevkaTermSS 11 Nerd Font"
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
  :config
  ;; 2025-04-14: I manually create the parent directory if it doesn't already
  ;; exist; this is not yet implemented upstream, so I do it manually here for
  ;; fresh installs of Emacs.
  (make-directory (file-name-directory fontaine-latest-state-file) t)

  ;; Set the last preset or fall back to desired style from `fontaine-presets'
  (when (file-exists-p fontaine-latest-state-file)
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default)))

  ;; Persist the latest font preset when closing/starting Emacs and while
  ;; switching between themes.
  (fontaine-mode 1)

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

;;;; El-patch
(use-package el-patch
  :ensure (:wait t)
  :demand t)

;;;; On
;; Package exposes a number of utility hooks and functions ported from
;; Doom Emacs.  The hooks make it easier to speed up Emacs startup by
;; providing finer-grained control of the timing at which packages are
;; loaded.  Provides the following hooks:
;; - on-first-input-hook
;; - on-init-ui-hook
;; - on-first-file-hook
;; - on-switch-frame-hook
;; - on-first-buffer-hook
;; - on-switch-buffer-hook
;; - on-switch-window-hook
(use-package on
  :ensure t
  :demand t)

;;;; My variables, functions, macros, and keymaps
(setopt user-full-name "Kristoffer Balintona"
        user-mail-address "krisbalintona@gmail.com")

;;;;; Keymaps
(defvar-keymap krisb-note-keymap
  :doc "Prefix for my note-taking needs.")
(bind-key "C-c n" krisb-note-keymap 'global-map)

;; TODO 2025-05-22: Revisit this.
;; (defvar-keymap krisb-file-keymap
;;   :doc "Prefix for file-related commands.")
;; (bind-key "C-c f" krisb-file-keymap 'global-map)

;; TODO 2025-05-22: Revisit this.
;; (defvar-keymap krisb-yank-keymap
;;   :doc "Prefix for yanking stuff.")
;; (bind-key "C-c i" krisb-yank-keymap 'global-map)

(defvar-keymap krisb-open-keymap
  :doc "Prefix for opening various hings.")
(bind-key "C-c o" krisb-open-keymap 'global-map)

(defvar-keymap krisb-toggle-keymap
  :doc "Prefix for toggling stuff.")
(bind-key "C-c t" krisb-toggle-keymap 'global-map)

;;;;; Directories
;; FIXME 2025-05-20: If the path denoted by `krisb-folio-directory'
;; does not exist, other packages that depend on this value are given
;; a non-existing path, likelly resulting in errors.  We might solve
;; this by turning this into a function instead, returning nil if it
;; doesn't exist, thereby avoiding passing a non-existing file path to
;; these packages.
(defvar krisb-folio-directory (expand-file-name "org-database" "~/Documents")
  "The directory holding my org files.")

(defvar krisb-notes-directory (expand-file-name "notes" krisb-folio-directory)
  "My notes directory.")

(defvar krisb-blog-manuscripts-directory (expand-file-name "manuscripts/blog" krisb-notes-directory)
  "The directory for my pre-export blog files.")

(defvar krisb-org-archive-directory (expand-file-name "archive" krisb-folio-directory)
  "The archive directory for my org files.")

(defvar krisb-org-agenda-directory (expand-file-name "agenda" krisb-folio-directory)
  "The directory holding my main org-agenda files.")

(defvar krisb-email-directory (expand-file-name "emails/" "~/Documents/")
  "Directory that houses my local email files.")

(defvar krisb-email-drafts-directory (expand-file-name "drafts" krisb-email-directory)
  "Directory that houses my local email drafts.")

;;;;; Files
(defvar krisb-org-agenda-main-file (expand-file-name "todo.org" krisb-org-agenda-directory)
  "My main org-agenda file.")

(defvar krisb-bibliography-files (list (expand-file-name "master-lib.bib" krisb-folio-directory))
  "A list of my bibliography (.bib) files.")

;;; A step below

;;;; Exec-path-from-shell
;; Ensure Emacs inherits specified variables from the user environment
(use-package exec-path-from-shell
  :ensure (:wait t)
  :demand t
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH" "BROWSER"
     ;; `ssh-agent' environment variables.  See
     ;; https://wiki.archlinux.org/title/SSH_keys#Start_ssh-agent_with_systemd_user
     "SSH_AGENT_PID" "SSH_AUTH_SOCK"))
  :config
  (exec-path-from-shell-initialize))

;;;; Theming

;;;;; Enable theme based on time of day
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

;;;;; Doric-themes
;; Minimalistic but visible and effective themes.  (Cf. modus-themes
;; and standard-themes.)
;;
;; TODO 2025-05-20: Document this in literate config.
;; See also `doric-themes-to-toggle' and `doric-themes-to-rotate'
(use-package doric-themes
  :disabled t
  :ensure t
  :demand t
  :bind
  (("<f8>" . doric-themes-toggle)
   ("C-<f8>" . doric-themes-select)
   ("M-<f8>" . doric-themes-rotate))
  :custom
  (doric-themes-to-toggle '(doric-marble doric-dark))
  :config
  (krisb-enable-theme-time-of-day (car doric-themes-to-toggle) (cadr doric-themes-to-toggle)))

;;;;; Ef-themes
(use-package ef-themes
  :ensure t
  :demand t
  :bind
  (("<f8>" . ef-themes-toggle)
   ("C-<f8>" . ef-themes-select)
   ("M-<f8>" . ef-themes-rotate))
  :custom
  (ef-themes-to-toggle '(ef-duo-light ef-duo-dark))
  :config
  (krisb-enable-theme-time-of-day (car ef-themes-to-toggle) (cadr ef-themes-to-toggle)))

;;;;; Electric
;; Convenient DWIM, out-of-the-way while you edit
;;
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;;
;; - `electric-quote-inhibit-functions'
;; - `electric-pair-delete-adjacent-pairs'
;; NOTE: 2025-05-22: For some reason lisp-mode sets these buffer
;; locally.  See `lisp-mode-variables'.
;; - `electric-pair-skip-whitespace'
;; - `electric-pair-open-newline-between-pairs'
(use-package electric
  :ensure nil
  :demand t
  :hook
  (text-mode-hook . electric-quote-local-mode)
  :custom
  (electric-pair-inhibit-predicate ; Applies to `electric-quote-mode’ too
   'krisb-electric-pair-conservative-inhibit)
  ;; TODO 2025-05-20: Revisit this.
  ;; (electric-quote-comment nil)
  ;; TODO 2025-05-20: Revisit this.
  ;; (electric-quote-string nil)
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  :config
  (electric-pair-mode 1)
  (electric-indent-mode 1)
  (electric-quote-mode 1)

  ;; Bespoke inhibit predicate
  (defun krisb-electric-pair-conservative-inhibit (char)
    (or
     ;; Always allow `electric-quote-chars’
     (member char electric-quote-chars)
     ;; Regular predicates
     (eq char (char-after))
     (and (eq char (char-before))
          (eq char (char-before (1- (point)))))
     (eq (char-syntax (following-char)) ?w))))

;;;; Garbage collection
;; NOTE 2024-02-11: Please reference
;; https://emacsconf.org/2023/talks/gc/ for a statistically-informed
;; recommendation for GC variables
(setopt garbage-collection-messages t
        gc-cons-percentage 0.15)

;; Restore `gc-cons-threshold’ to its default value.  We set it to an
;; exceptionally high value in early-init.el, so we restore it after
;; initialization.
(add-hook 'after-init-hook
          (lambda () (setopt gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))

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
;; Garbage collect on when idle
;; TODO 2025-05-23: Document
;; - `gcmh-low-cons-threshold’
(use-package gcmh
  :ensure t
  :hook
  (on-first-buffer-hook . gcmh-mode)
  (minibuffer-setup-hook . krisb-gcmh-minibuffer-setup)
  (minibuffer-exit-hook . krisb-gcmh-minibuffer-exit)
  :custom
  ;; For a related discussion, see
  ;; https://www.reddit.com/r/emacs/comments/bg85qm/comment/eln27qh/?utm_source=share&utm_medium=web2x&context=3.
  ;; 2025-04-06: The value below is taken from Doom Emacs; it was
  ;; bumped up from 16mb on commit
  ;; 80566503646dd80c7604220f184076e190144675, on Dec 6, 2024.
  (gcmh-high-cons-threshold (* 64 1024 1024)) ; 64 mb
  ;; If the idle delay is too long, we run the risk of runaway memory
  ;; usage in busy sessions.  And if it's too low, then we may as well
  ;; not be using gcmh at all.
  (gcmh-idle-delay 'auto)               ; Taken from Doom Emacs
  (gcmh-auto-idle-delay-factor 10)      ; Taken from Doom Emacs
  (gcmh-verbose nil)
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'gcmh-mode)
  (setopt garbage-collection-messages gcmh-verbose)

  ;; Increase GC threshold when in minibuffer
  (defvar krisb-gc-minibuffer--original gcmh-high-cons-threshold
    "Temporary variable to hold `gcmh-high-cons-threshold'")

  (defun krisb-gcmh-minibuffer-setup ()
    "Temporarily have \"limitless\" `gc-cons-threshold'."
    ;; (message "[krisb-gcmh-minibuffer-setup] Increasing GC threshold")
    (setq gcmh-high-cons-threshold most-positive-fixnum))

  (defun krisb-gcmh-minibuffer-exit ()
    "Restore value of `gc-cons-threshold'."
    ;; (message "[krisb-gcmh-minibuffer-exit] Restoring GC threshold")
    (setq gcmh-high-cons-threshold krisb-gc-minibuffer--original))

  ;; Increase `gc-cons-threshold' while using corfu too, like we do
  ;; for the minibuffer
  (with-eval-after-load 'corfu
    (advice-add 'completion-at-point :before #'krisb-gcmh-minibuffer-setup)
    (advice-add 'corfu-quit :before #'krisb-gcmh-minibuffer-exit)
    (advice-add 'corfu-insert :before #'krisb-gcmh-minibuffer-exit)))

;;; Two steps below

;;;; Savehist
;; Make history of certain things (e.g. minibuffer) persistent across sessions
(use-package savehist
  :ensure nil
  :demand t
  :custom
  (history-length 1000)
  ;; TODO 2025-05-19: Revisit this.  This should be placed elsewhere
  ;; too.
  ;; (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 30)
  :config
  (savehist-mode 1)

  (dolist (var '((Info-history-list . 250)))
    (add-to-list 'savehist-additional-variables var)))

;;;; Desktop
;; Save buffers across Emacs sessions
;;
;; TODO 2025-05-20: Document in literate configuration prose.
;; See also `desktop-locals-to-save'
(use-package desktop
  :ensure nil
  :hook
  ((text-mode-hook prog-mode-hook conf-mode-hook) . krisb-desktop--save-narrowing)
  ;; 2025-05-22: This is a workaround for elpaca.  Look at the bottom
  ;; of desktop.el’s file: we must avoid enabling `desktop-save-mode’
  ;; immediately when desktop is loaded because the desktop.el file
  ;; automatically adds to `after-init-hook’ to call `desktop-read’ if
  ;; `desktop-save-mode’ is enabled. This is problematic because, on
  ;; account of elpaca’s asynchronicity, some packages will end up not
  ;; being ready by the time desktop loads a buffer relevant to its
  ;; behavior.  For example, outli is not enabled in buffers it should
  ;; be because desktop opened them before outli could load.  Another
  ;; example is org: if desktop loads an org buffer before the org
  ;; declaration for elpaca, the built-in version is loaded before the
  ;; use-package declarataion for org, meaning that elpaca will
  ;; recognize the package as already available and therefore not
  ;; install and use a more upgraded version.
  ;;
  ;; So we should avoid having desktop open buffers before elpaca is
  ;; done initializing the packages.  One solution would be to just
  ;; enable `desktop-save-hook’ after elpaca initializes, then call
  ;; `desktop-read’ manually.  However, this would ignore the
  ;; --no-desktop flag called with Emacs.  Instead, we opt to enabl
  ;; `desktop-save-mode’ then add the very hook that desktop adds to
  ;; `after-init-hook’ `elpaca-after-init-hook’.
  (elpaca-after-init-hook
   . (lambda ()
       (desktop-save-mode 1)
       (let ((key "--no-desktop"))
         (when (member key command-line-args)
           (setq command-line-args (delete key command-line-args))
           (desktop-save-mode 0)))
       (when desktop-save-mode
         (desktop-read)
         (setq inhibit-startup-screen t))))
  :custom
  (desktop-load-locked-desktop 'check-pid)
  (desktop-save 'ask-if-new)
  (desktop-auto-save-timeout 3)
  ;; TODO 2025-05-19: Revisit this.
  ;; (desktop-files-not-to-save
  ;;  (rx (or (regexp "\\(\\`/[^/:]*:\\|(ftp)\\'\\)")
  ;;          ;; Don't save files from other Emacs repos because sometimes they
  ;;          ;; have local variables that mess with desktop's loading of files
  ;;          (literal (expand-file-name "emacs-repos/" "~"))
  ;;          ;; Don't want to open my large org-agenda files which I'll open
  ;;          ;; eventually anyway
  ;;          (literal krisb-org-agenda-directory))))

  (desktop-restore-eager 15)
  (desktop-restore-forces-onscreen nil)
  (desktop-restore-frames t)
  (desktop-restore-in-current-display nil)
  :config
  ;; Also save these variable values
  (add-to-list 'desktop-globals-to-save '(kill-ring . 10000))

  ;; Also save and restore narrowing state
  (defun krisb-desktop--save-narrowing ()
    "Save narrowed information.
Taken from
https://www.reddit.com/r/emacs/comments/162cjki/comment/jxzrthx/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1."
    (setq desktop-save-buffer
          (lambda (_) (if (buffer-narrowed-p) (list 'narrowed (point-min) (point-max))))))

  (defun krisb-desktop--restore-narrowing (_f n misc &rest _)
    "Restore narrowing of buffer.
Taken from
https://www.reddit.com/r/emacs/comments/162cjki/comment/jxzrthx/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1."
    (when (and misc (eq (car misc) 'narrowed))
      (apply #'narrow-to-region (cdr misc))))
  (advice-add 'desktop-restore-file-buffer :after #'krisb-desktop--restore-narrowing))

;;;; Isearch
;; Incremental search
(use-package isearch
  :ensure nil
  :defer t
  :custom
  (isearch-repeat-on-direction-change t)
  (isearch-allow-scroll 'unlimited)
  (isearch-allow-motion t)
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no)
  ;; Make regular isearch interpret the empty space as a regular
  ;; expression that matches any character between the words you give
  ;; it.  Also be aware of `isearch-toggle-lax-whitespace'
  (isearch-lax-whitespace t)
  (search-whitespace-regexp ".*?"))

;;;; Help
(use-package help
  :ensure nil
  :defer t
  :hook
  (help-fns-describe-function-functions . shortdoc-help-fns-examples-function)
  :bind
  ("C-h C-k" . describe-keymap)
  :custom
  (help-window-select t)
  (help-window-keep-selected t)

  (help-enable-variable-value-editing t)
  (help-clean-buttons t)
  (help-enable-symbol-autoload t)

  (describe-bindings-outline t)
  (describe-bindings-show-prefix-commands t)

  ;; TODO 2025-05-20: Revisit this.
  ;; (help-at-pt-display-when-idle t)
  )

;;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :ensure t
  :hook
  (help-fns-describe-function-functions . elisp-demos-advice-describe-function-1)
  :config
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;;; Insert spaces instead of tab characters
(setopt indent-tabs-mode nil)

;;;; Clipboard stuff
;; Don’t wait until yanking to put clipboard text into `kill-ring’
(setopt save-interprogram-paste-before-kill t)

;; Wayland compatibility
(when (getenv "WAYLAND_DISPLAY")
  (setopt interprogram-cut-function
          (lambda (text)
            (start-process "wl-copy" nil "wl-copy"
                           "--trim-newline" "--type" "text/plain;charset=utf-8" text))))

;;; Three steps below

;;;; Vc-jj
;; Integration between vc.el and the jujutsu (JJ) version control
;; system.  Best jj integration with vc currently (2025-03-13).
(use-package vc-jj
  :ensure t
  :after (:any vc project)
  :demand t
  :config
  (require 'project-jj))

;;;; Outline
(use-package outline
  :ensure nil
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-cycle-filter nil)
  (outline-minor-mode-highlight 'append)
  (outline-blank-line t)
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'outline-minor-mode))

;;;; Outli
;; TODO 2025-05-20: Document that I prefer this over the heavier,
;; less-compatible outshine.el as well as outline-indent.
;; Coding language-agnostic file outlines.  Lightweight and close to
;; the built-in outline.el.
(use-package outli
  :ensure (:repo "https://github.com/jdtsmith/outli")
  :hook
  ((prog-mode-hook text-mode-hook) . outli-mode)
  :bind
  ( :map outline-minor-mode-map
    ;; 2025-04-02: Assumes `outline-minor-mode-prefix' is "C-c @"
    ("C-c @ C-<return>" . outli-insert-heading-respect-content)
    ("C-c @ ?" . outli-speed-command-help)
    ("C-c @ s" . outli-toggle-narrow-to-subtree))
  :custom
  (outli-allow-indented-headlines t)
  (outli-default-nobar nil)             ; Show a horizontal rule?
  (outli-blend nil)
  :config
  ;; Add "Heading" (which outli headings are categorized as) imenu
  ;; group.  Taken from
  ;; https://github.com/jdtsmith/outli?tab=readme-ov-file#faq
  (with-eval-after-load 'consult-imenu
    (push '(?h "Headings")
          (plist-get (cdr (assoc 'emacs-lisp-mode consult-imenu-config)) :types))))

;;;; Repeat
(use-package repeat
  :ensure nil
  :hook
  (on-first-input-hook . repeat-mode))

;;;; Minibuffer.el
;; TODO 2025-05-20: Document the following options below in the
;; literate configuration:
;;
;; - `completion-cycle-threshold'
(use-package minibuffer
  :ensure nil
  :demand t
  :hook
  (elpaca-after-init-hook . krisb-completion-styles-setup)
  :custom
  ;; Completions buffer
  (completions-max-height 20) ; Otherwise the completions buffer can grow to fill the entire frame
  (completion-auto-help 'visible)
  (completion-lazy-hilit t) ; Lazy highlighting for drastic performance increase; added Emacs 30.1
  (completion-auto-select 'second-tab)
  (completions-format 'one-column)
  (completions-detailed t) ; Show annotations for candidates (like `marginalia')
  (completions-group t)    ; Emacs 28
  (completions-sort 'historical) ; Emacs 30.1

  ;; Category settings. A non-exhaustve list of known completion categories:
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;; - `embark-keybinding'
  (completion-category-defaults
   '((calendar-month (display-sort-function . identity))))
  (completion-category-overrides
   '((file (styles . (basic partial-completion flex))))) ; Include `partial-completion' to enable wildcards and partial paths.

  ;; TODO 2025-05-20: Revisit this.
  ;; (completion-ignore-case t)
  ;; TODO 2025-05-20: Revisit this.
  ;; (completion-flex-nospace t)
  (minibuffer-default-prompt-format " [%s]") ; Format for portion of minibuffer showing default value
  (enable-recursive-minibuffers t)
  :config
  (minibuffer-electric-default-mode 1)

  ;; Set up `completion-styles'
  (defun krisb-completion-styles-setup ()
    "Set up `completion-styes'."
    ;; I do this manually last because the final styles I want depend
    ;; on the packages I want enabled, and so setting this within each
    ;; use-package, independently of other use-packages, means I have
    ;; to make sure various packages are loaded after other ones so my
    ;; `completion-styles' setting isn't overridden in an undesirable
    ;; way.  Instead, I opt to just set it finally after all those
    ;; packages are set.
    (setopt completion-styles (list (if (featurep 'orderless)
                                        'orderless 'basic)
                                    (if (featurep 'hotfuzz)
                                        'hotfuzz 'flex))))

  ;; TODO 2025-05-20: Revisit this.
  ;; ;; Add prompt indicator to `completing-read-multiple'.  We display
  ;; ;; [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;; ;; Taken from https://github.com/minad/vertico
  ;; (defun krisb-crm-indicator (args)
  ;;   (cons (format "[completing-read-multiple: %s]  %s"
  ;;                 (propertize
  ;;                  (replace-regexp-in-string
  ;;                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
  ;;                   crm-separator)
  ;;                  'face 'error)
  ;;                 (car args))
  ;;         (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter-args #'krisb-crm-indicator)
  )

;;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :ensure t
  :demand t
  :custom
  (orderless-matching-styles
   '(orderless-regexp
     orderless-prefixes
     orderless-initialism
     ;; orderless-literal
     ;; orderless-flex
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  :config
  ;; TODO 2025-05-20: Revisit this.
  ;; ;; Eglot forces `flex' by default.
  ;; (add-to-list 'completion-category-overrides '(eglot (styles . (orderless flex))))
  )

;;;; Hotfuzz
;; Faster version of the flex completion style.  Hotfuzz is a much
;; faster version of the built-in flex style.  See
;; https://github.com/axelf4/emacs-completion-bench#readme
;;
;; NOTE 2025-05-20: See `krisb-completion-styles-setup' for how I use
;; this
(use-package hotfuzz
  :ensure t
  :demand t)

;;;; Vertico
(use-package vertico
  :ensure t
  :demand t
  :bind
  (("C-c v r" . vertico-repeat)
   :map vertico-map
   ("C-c v s" . vertico-suspend))
  :hook
  (minibuffer-setup-hook . vertico-repeat-save)
  ;; TODO 2025-05-20: Revisit this.
  ;; :custom
  ;; (vertico-count 13)
  ;; (vertico-resize 'grow-only)
  ;; (vertico-cycle nil)
  :config
  (vertico-mode 1)
  ;; TODO 2025-05-20: Revisit this.
  ;; (require 'krisb-vertico)
  )

;; More convenient path modification commands
(use-package vertico-directory
  :ensure nil
  :after vertico
  :hook
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :bind
  ( :map vertico-map
    ("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    ("M-DEL" . vertico-directory-delete-word)))

;; On-demand change the type of UI
(use-package vertico-multiform
  :ensure nil
  :after vertico
  :hook
  (vertico-mode-hook . vertico-multiform-mode)
  :custom
  (vertico-multiform-categories
   '((buffer (vertico-sort-function . nil))
     (file grid)
     (color (vertico-sort-function . vertico-sort-history-length-alpha))
     ;; TODO 2025-05-20: Revisit this.
     ;; (jinx grid
     ;;       (vertico-grid-annotate . 20)
     ;;       (vertico-grid-max-columns . 12)
     ;;       (vertico-grid-separator
     ;;        . #("    |    " 4 5 (display (space :width (1)) face (:inherit shadow :inverse-video t)))))
     ))
  (vertico-multiform-commands
   `((pdf-view-goto-label (vertico-sort-function . nil))
     (".+-history" (vertico-sort-function . nil))
     (,(rx bol (or (seq "recentf" (* (any alnum))) "consult-recent-file"))
      (vertico-sort-function . nil))
     ;; TODO 2025-05-20: Revisit this.
     ;; (,(rx bol (or (literal "org-node-") (literal "org-roam-")) "-find" eol)
     ;;  (completion-styles . (orderless ,(if (featurep 'hotfuzz) 'hotfuzz 'flex))) ; FIXME 2025-05-08: But what if hotfuzz is loaded after vertico-multiform?
     ;;  (orderless-matching-styles . (orderless-prefixes orderless-regexp orderless-literal)))
     )))

;;;; Corfu
;; Faster, minimal, and more lightweight autocomplete that is more
;; faithful to the Emacs infrastructure
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;;
;; - `corfu-auto'
;; - `corfu-cycle'
(use-package corfu
  :ensure t
  :demand t
  :hook
  (minibuffer-setup-hook . krisb-corfu-enable-in-minibuffer-conditionally)
  :bind
  (;; TODO 2025-05-20: Revisit this.
   ;; ("M-i" . completion-at-point) ; For harmony with "M-i" in `completion-preview-active-mode-map'
   :map corfu-map
   ("M-d" . corfu-info-documentation)
   ("M-m" . krisb-corfu-move-to-minibuffer))
  :custom
  (corfu-count 14)
  (corfu-scroll-margin 3)
  ;; Always have the same width
  (corfu-min-width 75)
  (corfu-max-width corfu-min-width)

  ;; Allow spaces and don't quit on boundary to leverage orderless's
  ;; space-separated components
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s) ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  :custom-face
  ;; Always use a fixed-pitched font for corfu; variable pitch fonts
  ;; (which will be adopted in a variable pitch buffer) have
  ;; inconsistent spacing
  (corfu-default ((t (:inherit 'default))))
  :config
  (global-corfu-mode 1)

  ;; Enable corfu in minibuffer if `vertico-mode' is disabled.  From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun krisb-corfu-enable-in-minibuffer-conditionally ()
    "Enable Corfu in the minibuffer if vertico is not active."
    (unless (bound-and-true-p vertico-mode)
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))

  ;; Transfer corfu completion to the minibuffer
  (defun krisb-corfu-move-to-minibuffer ()
    "Transfer corfu completion to the minibuffer.
Taken from
https://github.com/minad/corfu?tab=readme-ov-file#transfer-completion-to-the-minibuffer."
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (add-to-list 'corfu-continue-commands #'krisb-corfu-move-to-minibuffer))

;;;; Embark
;; Allow an equivalent to ivy-actions to regular `completing-read'
;; minibuffers
;;
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;;
;; - `embark-prompter'
(use-package embark
  :ensure t
  :bind
  (("C-.". embark-act)
   ("C-h B". embark-bindings)
   :map vertico-map
   ("C-.". embark-act)
   :map embark-symbol-map
   ("R". raise-sexp))
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command) ; Use completing read when typing ? after prefix key

  (embark-mixed-indicator-delay 1.5)
  (embark-collect-live-initial-delay 0.8)
  (embark-collect-live-update-delay 0.5)
  :config
  (add-to-list 'embark-keymap-alist '(raise-sexp . embark-symbol-map)))

(use-package embark-org
  :ensure nil
  :bind
  ( :map embark-org-heading-map
    ("C-j" . org-clock-goto)))

;;;; Embark-consult
;; TODO 2025-05-20: Revisit this.  Do I need it?
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;;;; Recentf
;; Enable logging of recent files
(use-package recentf
  :ensure nil
  :hook
  (on-first-file-hook . recentf-mode)
  :bind
  ( :map ctl-x-map
    ("M-f" . recentf-open))
  :custom
  (recentf-auto-cleanup 600)
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  (recentf-show-messages nil))

;;;; Grep
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;;
;; - `grep-save-buffers'
(use-package grep
  :ensure nil
  :custom
  (grep-use-headings t)
  ;; :config
  ;; TODO 2025-05-20: Revisit this.
  ;; (with-eval-after-load 'krisb-reveal
  ;;     (defun kris-reveal-grep-find-information ()
  ;;       "Return information required by `krisb-reveal-fold-commands'.
  ;; See the docstring of `krisb-reveal-fold-commands'."
  ;;       (save-window-excursion
  ;;         (save-excursion
  ;;           (compile-goto-error)
  ;;           (cons (point) (current-buffer)))))
  ;;     (dolist (command '(next-error-no-select
  ;;                        previous-error-no-select
  ;;                        compilation-display-error))
  ;;       (add-to-list 'krisb-reveal-fold-commands
  ;;                    (list :command command
  ;;                          :location #'kris-reveal-grep-find-information
  ;;                          :predicate (lambda () (eq major-mode 'grep-mode))))))
  )

;;; Four steps below

;;;; Pulsar
;; Alternative to `pulse.el'
(use-package pulsar
  :ensure t
  :demand t
  :custom
  (pulsar-pulse t)
  (pulsar-face 'pulsar-red)
  (pulsar-delay 0.05)
  (pulsar-iterations 5)
  :config
  (pulsar-global-mode 1))

;;;; Consult
;; Get enhanced or fancy versions of many built-in commands
;; TODO 2025-05-23: Document:
;; - `consult-recent-file’
(use-package consult
  :ensure t
  :bind
  (("C-x B" . consult-buffer)
   ;; Remaps
   ([remap bookmark-jump] . consult-bookmark)
   ([remap yank-pop] . consult-yank-pop)
   ([remap goto-line] . consult-goto-line)
   ([remap Info-search] . consult-info)
   ;; TODO 2025-05-20: Revisit this.
   ;; ([remap point-to-register] . consult-register-store)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap imenu] . consult-imenu)
   ([remap flymake-show-buffer-diagnostics] . consult-flymake)
   :map minibuffer-local-map
   ([remap next-matching-history-element] . consult-history)
   ([remap previous-matching-history-element] . consult-history)
   :map consult-narrow-map
   ("?" . consult-narrow-help)          ; Show available narrow keys
   :map goto-map                  ; The `M-g' prefix
   ("f" . consult-flymake)
   ("o" . consult-outline)
   ("e" . consult-compile-error)
   ("l" . consult-line)
   ("a" . consult-org-agenda)
   :map search-map                ; The `M-s' prefix
   ("i" . consult-info)
   ("g" . consult-git-grep)
   ("G" . consult-grep)
   ("r" . consult-ripgrep)
   ("f" . consult-find)
   ("F" . consult-locate))
  :custom
  ;; TODO 2025-05-20: Revisit this.
  ;; (consult-preview-key "C-M-;")
  (consult-bookmark-narrow
   '((?f "File" bookmark-default-handler)
     (?i "Info" Info-bookmark-jump)
     (?h "Help" help-bookmark-jump Info-bookmark-jump
         Man-bookmark-jump woman-bookmark-jump)
     (?p "PDFs" pdf-view-bookmark-jump-handler)
     (?a "Activities" activities-bookmark-handler)
     (?d "Docview" doc-view-bookmark-jump)
     (?s "Eshell" eshell-bookmark-jump)
     (?w "Web" eww-bookmark-jump xwidget-webkit-bookmark-jump-handler)
     (?v "VC Directory" vc-dir-bookmark-jump)
     (?o "Org headings" org-bookmark-heading-jump)
     (nil "Other")))
  (consult-ripgrep-args
   (concat
    "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --search-zip"
    ;; Additional args
    " --line-number --hidden"))
  :init
  (with-eval-after-load 'org
    (bind-key [remap consult-outline] #'consult-org-heading org-mode-map))
  :config
  ;; TODO 2025-05-20: Revisit this.
  ;; (require 'krisb-consult-ext)

  ;; Add log-edit histories to `consult-mode-histories'
  (add-to-list 'consult-mode-histories
               '(log-edit-mode
                 log-edit-comment-ring
                 log-edit-comment-ring-index
                 log-edit-beginning-of-line))

  ;; Use the faster plocate rather than locate
  (when (executable-find "plocate")
    (setopt consult-locate-args "plocate --ignore-case --existing --regexp"))

  ;; TODO 2025-05-20: Revisit this.
  ;; ;; Use consult UI with xref
  ;; (with-eval-after-load 'xref
  ;;   ;; Use Consult to select xref locations with preview
  ;;   (setopt xref-show-definitions-function #'consult-xref
  ;;           xref-show-xrefs-function #'consult-xref))

  ;; TODO 2025-05-20: Revisit this.
  ;; ;; Registers
  ;; (with-eval-after-load 'register
  ;;   ;; Fancier formatting of preview
  ;;   (setopt register-preview-function #'consult-register-format)
  ;;   ;; Fancier formatting of preview window. Adds thin lines, sorting and hides
  ;;   ;; the mode line of the register preview window. Copied from
  ;;   ;; https://github.com/minad/consult#use-package-example
  ;;   (advice-add 'register-preview :override #'consult-register-window))

  ;; Pulsar pulses
  (with-eval-after-load 'pulsar
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)))

;;;; Ultra-scroll
;; TODO 2025-05-20: Document that this package was the result of the
;; trial-and-error across many years and different packages to get
;; better mouse scrolling in Emacs.
;; TODO 2025-05-20: Document that a fancy mode line can cause
;; noticable dips in performance with this package:
;; https://github.com/jdtsmith/ultra-scroll?tab=readme-ov-file#take-aways
;; Package that makes Emacs mouse scrolling buttery smooth
(use-package ultra-scroll
  :ensure (:repo "https://github.com/jdtsmith/ultra-scroll")
  :demand t
  :config
  ;; As instructed by the README: `scroll-margin' > 0 not yet
  ;; supported
  (setopt scroll-conservatively 101
          scroll-margin 0)
  (ultra-scroll-mode 1))

;;;; Marginalia
;; Enable richer annotations in minibuffer
(use-package marginalia
  :ensure t
  :demand t
  :custom
  (marginalia-max-relative-age 0)       ; Always use absolute age
  (marginalia-align 'left)              ; Causes most alignment in my experience
  :config
  (marginalia-mode 1))

;;;; Tab-bar
(use-package tab-bar
  :ensure nil
  :bind
  ( :map tab-prefix-map
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
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice 'clone)
  (tab-bar-close-last-tab-choice 'delete-frame)
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-tab-hints t)
  (tab-bar-show t)
  (tab-bar-separator " ")
  (tab-bar-format
   '(tab-bar-format-tabs-groups
     tab-bar-separator
     tab-bar-format-align-right
     ;; TODO 2025-05-20: Revisit this.
     ;; tab-bar-format-global
     ))
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;;;; Mode line format
(setq mode-line-defining-kbd-macro (propertize " Macro" 'face 'mode-line-emphasis))

(setopt mode-line-compact 'long ; Emacs 28
        mode-line-right-align-edge 'window
        mode-line-percent-position '(-3 "%p") ; Don't show percentage of position in buffer
        mode-line-position-line-format '(" %l")
        mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28

;; TODO 2025-05-20: Revisit this.
(setopt mode-line-format
        '("%e" mode-line-front-space
          (:propertize
           ("" mode-line-mule-info mode-line-client mode-line-modified
            mode-line-remote mode-line-window-dedicated)
           display (min-width (6.0)))
          mode-line-frame-identification
          (project-mode-line ("" project-mode-line-format "   "))
          mode-line-buffer-identification "   "
          mode-line-position
          mode-line-format-right-align
          (vc-mode vc-mode) "  "
          mode-line-modes
          mode-line-misc-info
          mode-line-end-spaces))

;; Add segments to `global-mode-string'
(add-to-list 'global-mode-string '(vc-mode (:eval (concat vc-mode " "))))

;;;; Window
(use-package window
  :ensure nil
  :bind* ("M-o" . other-window)
  :custom
  (quit-restore-window-no-switch t)     ; Emacs 31
  (kill-buffer-quit-windows t))         ; Emacs 31

;;;; EAT
(use-package eat
  ;; 2024-12-29: See https://codeberg.org/akib/emacs-eat/pulls/133 for why we
  ;; use this fork of eat.
  :ensure ( :repo "https://codeberg.org/vifon/emacs-eat.git"
            :branch "fish-integration")
  :hook
  (fontaine-set-preset-hook . krisb-eat--setup)
  (eshell-load-hook . eat-eshell-mode)
  (eshell-load-hook . eat-eshell-visual-command-mode)
  :bind
  ( :map krisb-open-keymap
    ("s" . eat)
    :map project-prefix-map
    ("s" . eat-project))                ; Overshadow `project-shell’
  :config
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
                          :family (fontaine--get-preset-property fontaine-current-preset :term-family)))))

;;;; Ibuffer
;; TODO 2025-05-23: Document these packages’ relation to ibuffer:
;; - buffer-terminator
;; - bufler
(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer)
  :bind
  ( :map ibuffer-mode-map
    ("* d" . krisb-ibuffer-mark-displayed-buffers))
  :bind*
  ( :map ibuffer-mode-map
    ("SPC" . scroll-up-command)
    ("DEL" . scroll-down-command))
  :config
  ;; Bespoke command for marking buffers displayed across all tab-bar
  ;; tabs.  Useful for using `ibuffer’ to clean up buffers after a
  ;; long-running Emacs session.
  (defun krisb-ibuffer--get-displayed-buffers ()
    "Return a list of buffers visible in all windows across all tab-bar tabs."
    (let (displayed-buffers)
      (dolist (frame (frame-list))
        (save-window-excursion
          (dolist (tab (tab-bar-tabs frame))
            ;; The current tab does not have a stored window
            ;; configuration, so we don't need to switch to its window
            ;; configuration for its window list
            (when (eq 'tab (car tab))
              (let* ((tab-info (cdr tab))
                     (tab-window-conf (cdr (assq 'wc tab-info))))
                (set-window-configuration tab-window-conf)))
            (dolist (win (window-list nil 'never))
              (cl-pushnew (window-buffer win) displayed-buffers)))))
      displayed-buffers))

  (defun krisb-ibuffer-mark-displayed-buffers ()
    "Mark all buffers visible in any windows across all tab-bar tabs."
    (interactive)
    ;; We re-implement `ibuffer-mark-on-buffer' to call the expensive
    ;; `krisb-ibuffer--get-displayed-buffers' only once
    (let* ((displayed-buffers (krisb-ibuffer--get-displayed-buffers))
           (count
            (ibuffer-map-lines
             (lambda (buf _mark)
               (when (member buf displayed-buffers)
                 (ibuffer-set-mark-1 ibuffer-marked-char)
                 t))
             nil nil)))
      (ibuffer-redisplay t)
      (message "Marked %s buffers" count))))

;;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  :ensure t
  :hook
  (scratch-create-buffer-hook . krisb-scratch-buffer-setup)
  :bind
  ("C-c |" . scratch)
  :config
  (defun krisb-scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
 Taken from
 https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/"
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t))))

;;;; Imenu
(use-package imenu
  :ensure nil
  :custom
  (org-imenu-depth 7)                ; Show more than just 2 levels...
  (imenu-auto-rescan t)
  (imenu-flatten 'group)
  :config
  (with-eval-after-load 'pulsar
    (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)))

;;;; VC
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;;
;; - `vc-annotate-display-mode'
;; - `vc-revert-show-diff'
;; - `vc-git-revision-complete-only-branches'
(use-package vc
  :ensure nil
  :defer t
  :custom
  (vc-handled-backends '(Git))
  (vc-follow-symlinks t)
  (vc-allow-rewriting-published-history 'ask) ; Emacs 31
  ;; Improves performance by not having to check for other
  ;; backends. Expand this list when necessary
  (vc-async-checkin t)
  (vc-revert-show-diff t)
  (vc-find-revision-no-save t))         ; Emacs 31

(use-package vc-git
  :ensure nil
  :hook
  (vc-git-log-edit-mode-hook . auto-fill-mode)
  :custom
  (vc-git-log-edit-summary-target-len (+ 50 (length "Summary")))
  (vc-git-log-edit-summary-max-len (+ 70 (length "Summary")))
  (vc-git-diff-switches    ; Show summary diff summary in diff headers
   '("--patch-with-stat" "--histogram"))
  (vc-git-root-log-format
   `("%h %ad (%ar) %aN%d%n  %s"
     ;; The first shy group matches the characters drawn by
     ;; --graph. We use numbered groups because `log-view-message-re'
     ;; wants the revision number to be group 1.
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

;;;; Log-edit
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;;
;; - `log-edit-headers-alist'
;; - `log-edit-setup-add-author'
(use-package log-edit
  :ensure nil
  :defer t
  :custom-face
  (log-edit-summary ((t (:family ,(face-attribute 'variable-pitch :family))))))

;;;; Find-func
;; Binds useful commands for jumping to variables, functions, and libraries
(use-package find-func
  :ensure nil
  :demand t
  :bind
  ( :map tab-prefix-map
    ("F" . krisb-find-function-other-tab)
    ("L" . krisb-find-library-other-tab))
  :init
  ;; Useful keybinds for my usage
  (defun krisb-find-library-other-tab (library)
    "Find LIBRARY in other tab."
    (interactive (list (read-library-name)))
    (switch-to-buffer-other-tab (save-window-excursion (funcall-interactively #'find-library library))))

  (defun krisb-find-function-other-tab (function)
    "Find FUNCTION in other tab."
    (interactive (find-function-read))
    (find-function-do-it function nil 'switch-to-buffer-other-tab))
  :config
  (find-function-mode 1))               ; Emacs 31

;;;; Project.el
;; TODO 2025-05-22: Document:
;; - `project-vc-extra-root-markers’
(use-package project
  :ensure nil
  :defer t
  :bind
  ( :map project-prefix-map
    ("e" . project-eshell)
    ("C" . project-recompile))
  :custom
  (project-vc-merge-submodules nil)
  (project-file-history-behavior 'relativize)
  ;; The commands in `project-switch-commands' must be found in
  ;; `project-prefix-map'
  (project-switch-commands
   `((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-vc-dir "VC-Dir")
     (project-eshell "Eshell")
     ,(if (locate-library "eat")
          '(eat-project "EAT")
        (project-shell "Shell"))
     ,(if (locate-library "compile-multi")
          '(compile-multi "Compile-multi")
        '(project-compile "Compile"))
     (project-recompile "Recompile")
     (project-any-command "Other")))
  (project-mode-line t)
  (project-mode-line-face 'italic)
  :config
  ;; TODO 2025-05-22: Revisit this
  ;; ;; On startup, remove non-existent directories from remembered projects list
  ;; (project-forget-zombie-projects)
  )

;;;; Info
(use-package info
  :ensure nil
  :hook
  (Info-selection-hook . mixed-pitch-mode)
  (Info-selection-hook . krisb-info-font-resize)
  :custom
  (Info-isearch-search nil)         ; Limit isearch to the node we see
  :config
  (defun krisb-info-font-resize ()
    "Increase the font size of text in Info buffers."
    (face-remap-set-base 'default `(:height 1.2))))

;;;; Mixed-pitch
;; Locally remap default face to variable-pitch.
(use-package mixed-pitch
  :ensure t
  :custom
  ;; We don't want to set the height of variable-pitch faces because
  ;; non-variable-pitch faces will be "out of sync" with the height.
  ;; Therefore, to have larger font sizes in these buffers, we have to
  ;; remap those faces manually and locally.
  (mixed-pitch-set-height nil)
  (mixed-pitch-variable-pitch-cursor nil)
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'mixed-pitch-mode))

;;;; Cape
;; Expand capf functionality with corfu! See an updated list of the
;; defined capf functions in the package's commentary.
(use-package cape
  :ensure t
  :bind
  (("C-c . p" . completion-at-point)
   ("C-c . d" . cape-dabbrev)
   ("C-c . h" . cape-history)
   ("C-c . f" . cape-file)
   ("C-c . k" . cape-keyword)
   ("C-c . s" . cape-elisp-symbol)
   ("C-c . a" . cape-abbrev)
   ("C-c . w" . cape-dict)
   ([remap ispell-complete-word] . cape-dict)
   ("C-c . l" . cape-line)
   ("C-c . \\" . cape-tex)
   ("C-c . _" . cape-tex)
   ("C-c . ^" . cape-tex)
   ("C-c . &" . cape-sgml)
   ("C-c . r" . cape-rfc1345)
   ([remap dabbrev-completion] . cape-dabbrev))
  :custom
  (cape-dabbrev-min-length 2)
  ;; Recommended in
  ;; https://github.com/minad/corfu?tab=readme-ov-file#configuration:
  ;; Emacs 30 and newer: Disable Ispell completion function.  Try
  ;; `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)
  :init
  ;; These are added to the global definition of
  ;; `completion-at-point-functions', which acts as a fallback if
  ;; buffer-local values end in `t'. See
  ;; (info "(cape) Configuration") for an explanation.

  ;; TODO 2025-03-26: Should I just add these as separate capfs? The use for
  ;; super-capfs is described here:
  ;; (info "(cape) Super-Capf - Merging multiple Capfs")
  (defun krisb-cape-super-capf--dict-dabbrev ()
    "Super-capf of `cape-dict' and `cape-dabbrev'."
    (cape-wrap-super 'cape-dict :with 'cape-dabbrev))

  ;; Capfs added to the end of the global value of
  ;; `completion-at-point-functions'.  Consequently, they act as fallback backends.
  (dolist (capf (reverse '(cape-elisp-symbol krisb-cape-super-capf--dict-dabbrev)))
    (add-hook 'completion-at-point-functions capf 100))

  ;; Macro to help adding capfs via hooks
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

  (krisb-cape-setup-capfs
    "elisp"
    '(emacs-lisp-mode-hook lisp-interaction-mode-hook)
    (list #'cape-file #'cape-elisp-symbol))

  (krisb-cape-setup-capfs
    "commit"
    '(git-commit-setup-hook log-edit-mode-hook)
    (list #'cape-elisp-symbol #'cape-dabbrev))

  (krisb-cape-setup-capfs
    "shells"
    '(eshell-mode-hook comint-mode-hook)
    (list #'cape-file #'cape-history))
  :config
  ;; Use enchant en_US dictionary
  (with-eval-after-load 'jinx
    (setopt cape-dict-file
            (list (expand-file-name "enchant/en_US.dic" (xdg-config-home)))))

  ;; Resolve the undesirable behavior of `cape-elisp-symbol' and the
  ;; *Help* buffer described in
  ;; https://github.com/minad/corfu/discussions/504#discussioncomment-12592545.
  (defun krisb-corfu-popupinfo--doc-buffer (str)
    "Wrapper around `elisp--company-doc-buffer'.
This function is a replacement for `elisp--company-doc-buffer', which
normally returns the main Help buffer (returned by `help-buffer').
Instead, this function returns a separate buffer to use as the Help
buffer.

Accepts the same argument as `elisp--company-doc-buffer' (STR).

Meant to be used with `cape-capf-properties' on the `cape-elisp-symbol'
completion at point function.  This ameliorates the sometimes
undesirable issue described in
https://github.com/minad/corfu/discussions/504#discussioncomment-12592545.

This solution was taken from the suggestion of
https://github.com/minad/corfu/discussions/504#discussioncomment-12593463."
    (let* ((help-xref-following t)
           (new-help-buf-name
            "*corfu-popupinfo documentation*")
           (new-help-buf (get-buffer-create new-help-buf-name)))
      (with-current-buffer new-help-buf
        (help-mode)
        (elisp--company-doc-buffer str))))

  (defun krisb-cape-elisp--around-advice (orig-fun &rest _args)
    "Advice to use a different doc buffer for documentation.
This solution was taken from the suggestion of
https://github.com/minad/corfu/discussions/504#discussioncomment-12593463."
    (cape-wrap-properties orig-fun :company-doc-buffer #'krisb-corfu-popupinfo--doc-buffer))

  (dolist (capf '(cape-elisp-symbol elisp-completion-at-point))
    (advice-add capf :around #'krisb-cape-elisp--around-advice))

  ;; NOTE 2025-03-26: The below does not apply because I've set
  ;; `text-mode-ispell-word-completion' to nil.  I've left it here for
  ;; future reference and just in case I revert the value to
  ;; 'completion-at-point.  Resolve `ispell-completion-at-point' error
  ;; when there is no dictionary available
  (defun krisb-cape-ispell--around-advice (orig-fun &rest _args)
    "Advice to remove an error from missing ispell dictionary.
There is an error when using `ispell-completion-at-point' without a
dictionary.  The error is this:

(error \"ispell-lookup-words: No plain word-list found at systemdefault locations.  Customize ‘ispell-alternate-dictionary’ to set yours.\")

ORIG-FUN should be `ispell-completion-at-point'."
    (cape-wrap-silent orig-fun))
  (advice-add 'ispell-completion-at-point :around #'krisb-cape-ispell--around-advice)

  ;; Make eglot's capf non-exclusive
  (with-eval-after-load 'eglot
    (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)))

;;;; Xref
;; TODO 2025-05-22: Document:
;; - `xref-file-name-display’
(use-package xref
  :ensure nil
  :bind
  ("C-M-?". xref-find-references-and-replace) ; Emacs 29.1
  :custom
  ;; TODO 2025-05-22: Revisit this.
  ;; (xref-show-definitions-function 'xref-show-definitions-completing-read)
  ;; (xref-show-xrefs-function 'xref-show-definitions-buffer)
  (xref-search-program (if (executable-find "rg") 'ripgrep 'grep))
  (xref-history-storage 'xref-window-local-history) ; Per-window history of `xref-go-*'
  :config
  ;; We remove the fallback backend, `etags--xref-backend', which prompts the
  ;; user for an etags table -- this is undesirable for me.
  (setq-default xref-backend-functions nil)
  ;; Then add `elisp--xref-backend' as the global value of
  ;; `xref-backend-functions', which means it is run when the local
  ;; value ends with `t'. See (info "(elisp) Running Hooks") for an
  ;; explanation.
  (add-hook 'xref-backend-functions #'elisp--xref-backend)

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

;;;; Dabbrev
(use-package dabbrev
  :ensure nil
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (dolist (mode '(doc-view-mode
                  pdf-view-mode
                  tags-table-mode))
    (add-to-list 'dabbrev-ignored-buffer-modes mode)))

;;;; Hippie-expand
(use-package hippie-exp
  :ensure nil
  :bind
  ([remap dabbrev-expand] . hippie-expand))

;;;; Autorevert
;; Automatically update buffers as files are externally modified
;; TODO 2025-05-22: Document:
;; - `auto-revert-verbose’
(use-package autorevert
  :ensure nil
  :hook
  (on-first-file-hook . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-avoid-polling t)     ; Has caveats.  Read its docstring
  (auto-revert-check-vc-info t)
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'auto-revert-mode))

;;; Fluff

;;;; Recursion-indicator
;; Enhanced `minibuffer-depth-indicate-mode'.  Indicates recursion for
;; more than just the minibuffer
(use-package recursion-indicator
  :ensure t
  :demand t
  :hook
  (recursion-indicator-mode-hook . minibuffer-depth-indicate-mode)
  :custom
  (recursion-indicator-symbols
   '((completion "C" recursion-indicator-completion)
     (prompt "P" recursion-indicator-prompt)
     (suspend "S" recursion-indicator-suspend)
     (t "R" recursion-indicator-default)))
  :config
  (recursion-indicator-mode 1))

;;;; Time
(use-package time
  :ensure nil
  :demand t
  :custom
  (display-time-24hr-format t)
  (display-time-format "%R")
  (display-time-interval 60)
  (display-time-default-load-average nil)
  (world-clock-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Nicosia" "Nicosia (capital of Cyprus)")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Tokyo" "Tokyo")
     ("Asia/Shanghai" "Beijing")))
  :config
  (display-time-mode 1))

;;;; Eldoc
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;;
;; - `eldoc-print-after-edit'
;; - `eldoc-echo-area-display-truncation-message'
;; - `eldoc-echo-area-use-multiline-p'
(use-package eldoc
  :bind
  ( :map help-map
    ("\." . eldoc-doc-buffer)) ; I don't find much use for `display-local-help'
  :custom
  (eldoc-idle-delay 0.2)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly) ; TODO 2025-05-20: Revisit this.
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-help-at-pt t)                  ; Emacs 31.1
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'eldoc-mode))

;;;; Newcomment
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;;
;; - `comment-fill-column'
;; - `comment-multi-line'
;; - `comment-style'
(use-package newcomment
  :ensure nil
  :custom
  (comment-empty-lines t))

;;;; Prog-mode
(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode-hook . goto-address-prog-mode) ; Buttonize URLs and e-mail addresses in comments and strings
  (prog-mode-hook . bug-reference-prog-mode)) ; Buttonize bug references in comments and strings

;;;; Abdridge-diff
;; Abridge (shorten) refined diff hunks with long lines.  You can
;; enable and disable showing the abridged version using
;; `abridge-diff-toggle-hiding'.
(use-package abridge-diff
  :ensure t
  :after diff
  :demand t
  :config
  (abridge-diff-mode 1)
  (add-to-list 'mode-line-collapse-minor-modes 'abridge-diff-mode))

;;;; Cursory
;; Global and local cursor presets
(use-package cursory
  :ensure t
  :hook
  (prog-mode-hook . (lambda () (cursory-set-preset 'code :local)))
  ((org-mode-hook markdown-mode-hook git-commit-setup-hook log-edit-mode-hook message-mode-hook)
   . (lambda () (cursory-set-preset 'prose :local)))
  :custom
  (cursory-latest-state-file (no-littering-expand-var-file-name "cursory/cursory-latest-state"))
  (cursory-presets
   '((code
      :cursor-type box
      :cursor-in-non-selected-windows hollow
      :blink-cursor-mode 1)
     (prose
      :cursor-type (bar . 2)
      :blink-cursor-mode -1
      :cursor-in-non-selected-windows (hbar . 3))
     (default)
     (t                                 ; The fallback values
      :cursor-type box
      :cursor-in-non-selected-windows hollow
      :blink-cursor-mode 1
      :blink-cursor-blinks 10
      :blink-cursor-delay 5
      :blink-cursor-interval 0.5)))
  :config
  ;; 2025-04-14: I manually create the parent directory if it doesn't
  ;; already exist; this is not yet implemented upstream, so I do it
  ;; manually here for fresh installs of Emacs.
  (make-directory (file-name-directory cursory-latest-state-file) t)

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (when (file-exists-p cursory-latest-state-file)
    (cursory-set-preset (or (cursory-restore-latest-preset) 'default)))

  ;; Persist latest preset used across Emacs sessions
  (cursory-mode 1))

;;;; Completion-preview
(use-package completion-preview
  :ensure nil
  :hook
  ((prog-mode-hook log-edit-mode-hook eval-expression-minibuffer-setup-hook)
   . completion-preview-mode)
  (eshell-mode-hook . krisb-completion-preview-mode-setup-eshell)
  :bind
  ( :map completion-preview-active-mode-map
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate))
  :custom
  (completion-preview-ignore-case t)
  (completion-preview-minimum-symbol-length 3)
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'completion-preview-mode)

  ;; TODO 2025-05-20: Revisit this.
  ;;   ;; Use prescient or corfu-prescient's sorting function if they are
  ;;   ;; available.  With this, the completion candidates shown by corfu
  ;;   ;; align with the completion candidate shown by
  ;;   ;; `completion-preview-mode'.  The reason we use this variable
  ;;   ;; watcher is that it is an inexpensive solution to changing
  ;;   ;; `corfu-sort-function' values.
  ;;   (with-eval-after-load 'prescient
  ;;     ;; Use this as a fallback value: if `corfu-sort-function' isn't
  ;;     ;; changed, `completion-preview-sort-function' will remain
  ;;     ;; `prescient-completion-sort'
  ;;     (setopt completion-preview-sort-function #'prescient-completion-sort))
  ;;   (add-variable-watcher 'corfu-sort-function
  ;;                         (lambda (_symbol newval operation where)
  ;;                           "Match the value of `completion-preview-sort-function' to `corfu-sort-function'.
  ;; If `corfu-sort-function' is set buffer-locally, also set
  ;; `completion-preview-sort-function' buffer-locally.  Otherwise, change
  ;; the default value of `completion-preview-sort-function' accordingly.
  ;;
  ;; This action only applies when the value of `corfu-sort-function' is
  ;; set (i.e., OPERATION is \\='set).  This excludes, e.g., let bindings."
  ;;                           (when (equal operation 'set)
  ;;                             (if where
  ;;                                 (with-current-buffer where
  ;;                                   (setq-local completion-preview-sort-function newval))
  ;;                               (setopt completion-preview-sort-function newval)))))

  ;; Add these bespoke self-insert commands to the list of recognized
  ;; preview commands
  (dolist (command '(org-self-insert-command
                     outshine-self-insert-command))
    (add-to-list 'completion-preview-commands command))

  ;; Special settings for eshell buffers
  (defun krisb-completion-preview-mode-setup-eshell ()
    "Set specific settings in eshell buffers."
    (setq-local completion-preview-minimum-symbol-length 1)
    (completion-preview-mode 1)))

;;;; Dash
;; Popular library for list manipulation
(use-package dash
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'info-look
    (dash-register-info-lookup))
  :config
  (global-dash-fontify-mode 1))

;;;; Smart-mark
;; When pressing C-g while marking a region, move point to the
;; location the marking command was invoked from.
(use-package smart-mark
  :ensure t
  :demand t
  :config
  (smart-mark-mode 1))

;;;; Man
(use-package man
  :ensure nil
  :defer t
  :custom
  (Man-notify-method 'aggressive)) ; Instead of `display-buffer-alist', use this

;;;; Tempel
;; Like tempel.el but updated to modern standards.
(use-package tempel
  :ensure t
  :hook
  ((prog-mode-hook text-mode-hook) . krisb-tempel-setup-capf)
  :bind
  ("M-*" . tempel-insert)
  :custom
  ;; Applies to `tempel-expand' and `tempel-complete'.  We prefer
  ;; non-pair characters to avoid inserting an extra pair from
  ;; `electric-pair-mode'.  If set, it should be an unused (or at
  ;; least very rarely used) comment delimiter to avoid indenting the
  ;; line when pressing the TAB key and with `tab-always-indent' set
  ;; to \\='complete.  If this is set to nil, then template names
  ;; should not be ambiguous, otherwise trying to complete other
  ;; symbol names will get hijacked by completing for tempel templates
  ;; (assuming the tempel `completion-at-point’ functions are set).
  (tempel-trigger-prefix nil)
  :init
  ;; Element that expands other templates by name.  E.g., (i header)
  ;; expands the template named "header."
  (defun krisb-tempel-include (elt)
    (when (eq (car-safe elt) 'include)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))
  :config
  (add-to-list 'tempel-user-elements #'krisb-tempel-include)

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

;;;; Dired
;; Emacs' file manager
;; TODO 2025-05-22: Document:
;; - `dired-recursive-deletes'
(use-package dired
  :ensure nil
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . turn-on-gnus-dired-mode) ; Email attachment integration with dired
  :bind
  ( :map dired-mode-map
    ("e" . krisb-dired-eval-form))
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target 'dired-dwim-target-recent)
  (dired-hide-details-hide-symlink-targets nil) ; Don't hide symlink targets
  (dired-kill-when-opening-new-dired-buffer t) ; Basically `dired-single'
  (dired-listing-switches "--group-directories-first --time-style=long-iso -alhgv")
  (dired-movement-style 'bounded-files)
  (dired-recursive-copies  'always)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  :config
  ;; Mark files and do a sexp in their buffers. Based off
  ;; https://superuser.com/a/176629
  (defun krisb-dired-eval-form (sexp &optional prefix)
    "Run SEXP in marked dired files. If called with
PREFIX (`universal-argument' if interactively), run a particular
command."
    (interactive (list (if current-prefix-arg
                           (read-extended-command) ; Command
                         (read--expression "Run expression on marked files: ")) ; Sexp
                       current-prefix-arg))
    (save-window-excursion
      (mapc #'(lambda (filename)
                (with-current-buffer (find-file-noselect filename)
                  (if prefix
                      (call-interactively (intern sexp))             ; Command
                    (funcall-interactively 'eval-expression sexp)))) ; Sexp
            (dired-get-marked-files)))))

;;;; Dired-hist
;; History for dired buffers
(use-package dired-hist
  :ensure t
  :hook
  (dired-mode-hook . dired-hist-mode)
  :bind
  ( :map dired-mode-map
    ("l" . dired-hist-go-back)
    ("r" . dired-hist-go-forward)))

;;;; Auto saves
;; TODO 2025-05-23: Document:
;; - `auto-save-include-big-deletions’
;; - `delete-auto-save-files’ and
;;   `kill-buffer-delete-auto-save-files’
;; - `remote-file-name-inhibit-auto-save-visited’
;; TODO 2025-05-23: Note that auto-save is distinct from
;; `auto-save-visited-mode’
(use-package files
  :ensure nil
  :hook
  (on-first-file-hook . auto-save-visited-mode)
  :custom
  (auto-save-default t) ; Only a local minor mode exists; this variable influences the global value
  (auto-save-timeout 5)
  (auto-save-interval 150)
  ;; TODO 2025-05-23: Revisit this.
  ;; (auto-save-no-message t)
  ;; `auto-save-visited-mode’
  (auto-save-visited-interval 8)
  (auto-save-visited-predicate        ; Value Inspired by `super-save'
   (lambda ()
     (or
      ;; TODO 2025-05-23: Revisit this.
      ;; Don’t auto save buffers that are too long, since that may
      ;; lead to noticable delays
      (< (save-restriction (widen) (count-lines (point-min) (point-max)))
         5000)
      ;; Don’t auto-save `pdf-view-mode’ buffers
      (derived-mode-p 'pdf-view-mode))))
  :config
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

;;;; Backups
;; Backup files. "Emacs makes a backup for a file only the first time
;; the file is saved from the buffer that visits it."
;; TODO 2025-05-22: Document:
;; - `make-backup-files’
;; - `dired-kept-versions’
;; - `kept-old-versions’
(use-package files
  :ensure nil
  :custom
  (backup-by-copying t)          ; See (info "(emacs) Backup Copying")
  (vc-make-backup-files t)
  ;; Numbering backups
  (version-control t)
  (kept-new-versions 4)
  (delete-old-versions t)
  :config
  ;; TODO 2025-05-23: Mention no-littering's
  ;; `no-littering-theme-backups'.
  ;; Modified from Doom Emacs.  Backup files have names that are hashed.
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

;;;; Don't create lock files
(setopt create-lockfiles nil)

;;;; Hide-mode-line
(use-package hide-mode-line
  :ensure t
  :bind
  ( :map krisb-toggle-keymap
    ("m" . hide-mode-line-mode)))

;;; Coding

;;;; Org-src
;; TODO 2025-05-23: Document:
;; - `org-edit-src-auto-save-idle-delay’
;; - `org-src-block-faces’
(use-package org-src
  :ensure nil
  :after org
  :custom
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-edit-src-turn-on-auto-save t))   ; Auto-save for source buffers

;;; Writing

;;;; Cascading-dir-locals
;; "Provides a global minor mode that changes how Emacs handles the
;; lookup of applicable dir-locals files (".dir-locals.el"): instead
;; of starting at the directory of the visited file and moving up the
;; directory tree only until a first dir-locals file is found, collect
;; and apply all (!) dir-locals files found from the current directory
;; up to the root one."
(use-package cascading-dir-locals
  :ensure t
  :hook
  (on-first-file-hook . cascading-dir-locals-mode))

;;;; Org
(use-package org
  :ensure t
  :hook
  (org-mode-hook . variable-pitch-mode)
  (org-mode-hook . visual-line-mode)
  (org-mode-hook . (lambda () (setq-local line-spacing 0.2 fill-column 100)))
  :bind
  ("C-c s" . org-store-link)
  :custom
  (org-directory krisb-folio-directory)

  ;; Headlines
  ;; TODO 2025-05-22: Document:
  ;; - `org-hide-leading-stars'
  ;; - `org-n-level-faces'
  ;; - `org-cycle-separator-lines'
  ;; - `org-cycle-level-faces'
  ;; - `org-insert-heading-respect-content'
  ;; - `org-fontify-todo-headline’
  ;; (org-ellipsis " ⮷")                        ; TODO 2025-05-22: Revisit this
  (org-tags-column 0)
  (org-blank-before-new-entry
   '((heading . auto)
     (plain-list-item . nil)))
  (org-M-RET-may-split-line
   '((table . nil)
     (default . t)))
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-startup-folded 'nofold)
  (org-fontify-done-headline nil)

  ;; Plain lists
  ;; TODO 2025-05-22: Document the "Org Plain List" customize group as
  ;; well as these options:
  ;; - `org-list-use-circular-motion'
  ;; TODO 2025-05-22: Document that `org-list-demote-modify-bullet' is
  ;; almost like a more versatile version of org-bulletproof
  (org-list-allow-alphabetical t)
  (org-list-demote-modify-bullet
   '(("+" . "-")
     ("-" . "+")))

  ;; Markup
  ;; TODO 2025-05-22: Document:
  ;; - `org-hide-macro-markers'
  ;; - `org-pretty-entities-include-sub-superscripts' - see also `org-export-with-sub-superscripts'
  ;; - `org-hidden-keywords'
  (org-hide-emphasis-markers t)
  (org-pretty-entities t) ; Show as UTF-8 characters (useful for math)
  (org-use-sub-superscripts '{}) ; Requires brackets to recognize superscripts and subscripts

  ;; Movement
  ;; TODO 2025-05-22: Document:
  ;; - `org-special-ctrl-k'
  (org-special-ctrl-a/e t)
  (org-ctrl-k-protect-subtree 'error)

  ;; Org blocks
  (org-structure-template-alist
   '(("s" . "src")
     ("e" . "src emacs-lisp")
     ("q" . "quote")
     ("c" . "comment")
     ("C" . "center")
     ("e" . "example")
     ("e" . "export")
     ("v" . "verse")))
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line nil)

  ;; Timestamps
  (org-edit-timestamp-down-means-later t)

  ;; TODO 2025-05-22: Revisit this.
  ;; (org-file-apps
  ;;  '((directory . emacs)
  ;;    ("\\.mm\\'" . default)
  ;;    ("\\.x?html?\\'" . default)
  ;;    ("\\.pdf\\'" . default)
  ;;    ("\\.docx\\'" . system)
  ;;    ("\\.odt\\'" . system)
  ;;    ;; Default to `auto-mode-alist'
  ;;    (auto-mode . emacs)))
  ;; TODO 2025-05-22: Revisit these.
  ;; :custom-face
  ;; (org-quote ((t (:family ,(face-attribute 'variable-pitch :family) :extend t :inherit 'org-block))))
  ;; (org-ellipsis ((t (:box unspecified :inherit default)))) ; Don't make line taller because of org-ellipsis
  :config
  ;; Make org-open-at-point follow file links in the same window
  (setf (alist-get 'file org-link-frame-setup) 'find-file)

  ;; Pulsar pulses
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-agenda-after-show-hook
                    org-follow-link-hook))
      (add-hook hook #'pulsar-recenter-center)
      (add-hook hook #'pulsar-reveal-entry))))

(use-package org-contrib
  :ensure t
  :after org)

;;; Org-expiry
(use-package org-expiry
  :ensure nil
  :after org-contrib
  :hook
  (org-capture-before-finalize . org-expiry-insert-created)
  :custom
  (org-expiry-inactive-timestamps t))

;;;; Org-capture
(use-package org-capture
  :ensure nil
  :bind
  ("C-c c" . org-capture)
  :custom
  (org-capture-use-agenda-date t)
  ;; See also `org-capture-templates-contexts'
  (org-capture-templates
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
         (let* ((node (krisb-org-capture--org-node-by-tags '("^__journal$"))))
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
      :clock-in t
      :clock-resume t)
     ("w" "Just write" entry
      (file+olp+datetree
       (lambda ()
         (let* ((node (org-node-by-id "20241006T214800.000000")))
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
         (let* ((node (org-node-by-id "20241114T091749.707997")))
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
         (let* ((node (org-node-by-id "20250422T171216.767702")))
           (org-capture-put :krisb-node node)
           (org-node-get-file node)))
       (lambda ()
         (let ((node (org-capture-get :krisb-node)))
           ;; Should return nil if node is a file
           (when (org-node-is-subtree node)
             (org-node-get-olp-with-self node)))))
      "* %? %^g\n"
      :tree-type (year month)
      :jump-to-captured t
      :immediate-finish t
      :empty-lines 1
      :hook org-id-get-create
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
      :immediate-finish t)))
  :config
  ;; Bespoke functions that selects an org-node node based on TAGS.
  ;; Used for my datetree capture templates
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
             org-node--candidate<>entry)))

;;;; Org-Mem
(use-package org-mem
  :ensure t
  :defer t
  :custom
  (org-mem-do-sync-with-org-id t)
  (org-mem-watch-dirs (list krisb-folio-directory))
  (org-mem-do-warn-title-collisions nil)
  :config
  (org-mem-updater-mode 1)
  ;; TODO 2025-05-22: Revisit this.
  ;; 2025-05-23: I’ve enabled the mode, although I have no use for the
  ;; generated sqlite database for now.
  ;; (org-mem-db1-mode 1)

  ;; Load things related to org-mem’s interaction with org-roam
  (with-eval-after-load 'org-roam
    ;; emacsql is required for `org-mem-roamy-db-mode’, as it will
    ;; error otherwise.
    (use-package emacsql
      :ensure t
      :demand t
      :config
      (org-mem-roamy-db-mode 1))
    ;; End dependence on `org-roam-db-sync'
    (setopt org-roam-db-update-on-save nil
            org-mem-roamy-do-overwrite-real-db t)
    (org-roam-db-autosync-mode -1)))

;;;; Org-node
(use-package org-node
  :ensure t
  :bind
  ( :map krisb-note-keymap
    ("l" . org-node-context-toggle)
    ([remap org-roam-buffer-toggle] . org-node-context-toggle)
    ("f" . org-node-find)
    ("i" . org-node-insert-link)
    ("t a" . org-node-add-tags-here))
  :custom
  (org-node-ask-directory t)
  (org-node-datestamp-format "%Y%m%dT%H%M%S--")
  (org-node-context-persist-on-disk t)
  (org-node-affixation-fn 'krisb-org-node-affixation-fn)
  (org-node-alter-candidates t)
  (org-node-one-empty-candidate nil)
  (org-node-custom-link-format-fn 'krisb-org-node-custom-link-format-fn)
  (org-node-filter-fn 'krisb-org-node-filter-fn)
  (org-node-warn-title-collisions nil)
  (org-node-renames-allowed-dirs (list krisb-notes-directory))
  :init
  ;; Rename buffer to the file's title if the file is an org-node
  ;; node.
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
  (add-hook 'org-mode-hook #'krisb-org-node-rename-buffer-name-to-title)
  :config
  (org-node-cache-mode 1)
  (org-node-context-follow-mode 1)

  ;; Make the org-roam buffer performant for free
  (with-eval-after-load 'org-roam
    (org-node-roam-accelerator-mode 1))

  ;; Bespoke filtering (exclusion) function.
  (defun krisb-org-node-filter-fn (node)
    "Predicate for whether to include NODE.
If non-nil, include.  If nil, exclude.  This predicate excludes these
nodes:
- With non-nil ROAM_EXCLUDE property value."
    (let ((exclude-val (cdr (assoc "ROAM_EXCLUDE" (org-node-get-properties node)))))
      (not (or
            (when exclude-val (string= "t" (string-trim exclude-val)))
            ;; More conditions here
            ))))

  ;; Bespoke `org-node-find'
  (cl-defmethod krisb-org-node-get-box ((node org-mem-entry))
    "Return the value of the ROAM_BOX property of NODE."
    (cdr (assoc "ROAM_BOX" (org-node-get-properties node) #'string-equal)))

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
    (cdr (assoc "ROAM_PLACE" (org-node-get-properties node))))

  (cl-defmethod krisb-org-node-get-type ((node org-mem-entry))
    "Return the value of the ROAM_TYPE property of NODE."
    (cdr (assoc "ROAM_TYPE" (org-node-get-properties node) #'string-equal)))

  (cl-defmethod krisb-org-node-get-person ((node org-mem-entry))
    "Return the value of the ROAM_PERSON property of NODE."
    (cdr (assoc "ROAM_PERSON" (org-node-get-properties node) #'string-equal)))

  (cl-defmethod krisb-org-node-olp-full-propertized ((node org-mem-entry))
    "Return the full outline path of NODE fontified.
The full outline path of NODE (given by `org-node-get-olp-full')
surrounded by parentheses and whose parts are separated by \" > \".
Additionally, the entire string is fontified to the shadow face."
    (let ((olp (propertize (string-join (org-node-get-olp-full node) " > ") 'face 'shadow)))
      (unless (string-empty-p olp)
        (concat
         (propertize "(" 'face 'shadow)
         olp
         (propertize ")" 'face 'shadow)))))

  (cl-defmethod krisb-org-node-tags-propertized ((node org-mem-entry))
    "Return the full outline path of NODE fontified."
    (when-let ((tags (org-node-get-tags node)))
      (propertize (concat "#" (string-join tags " #")) 'face 'org-tag)))

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
                  (propertize (concat " (" file-title ")") 'face 'shadow)))))))

;;;; Olivetti
(use-package olivetti
  :ensure t
  :hook
  ((org-mode-hook Info-mode-hook emacs-news-view-mode-hook org-msg-edit-mode-hook markdown-mode-hook)
   . olivetti-mode)
  ;; (olivetti-mode-hook . krisb-olivetti-set-bookmark-face)
  :custom
  (olivetti-body-width 0.55)
  (olivetti-minimum-body-width 80)
  (olivetti-margin-width 8)
  (olivetti-style 'fancy)              ; Fancy makes the buffer look like a page
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
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'olivetti-mode)

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
  )

;;;; Adaptive-wrap
(use-package adaptive-wrap
  :ensure t
  :hook
  (visual-line-mode-hook . adaptive-wrap-prefix-mode))

;;;; Jinx
;; JIT spell checker that uses `enchant'. The executable is
;; enchant-2. See the manual for more information:
;; https://abiword.github.io/enchant/src/enchant.html
(use-package jinx
  ;; Installed via Guix because it needs to compile a C module
  :ensure nil
  ;; For AUR:
  ;; :ensure-system-package ((enchant-2 . enchant)
  ;;                         (pkgconf)
  ;;                         ;; Don't forget to install spell checker libraries!
  ;;                         (hunspell)
  ;;                         ("/usr/share/hunspell/en_US-large.dic" . hunspell-en_us)
  ;;                         (hspell)      ; Hebrew
  ;;                         (nuspell) ; Newest spell checker to be used by Firefox, Thunderbird, etc.
  ;;                         (voikkospell . libvoikko)) ; Finnish
  :hook
  (on-first-buffer-hook . global-jinx-mode)
  :bind
  ( :map jinx-mode-map
    ([remap ispell-word] . jinx-correct)
    ("C-," . jinx-correct)
    ("C-M-$" . jinx-languages))
  :custom
  (jinx-delay 1.5)
  :config
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
  ;; `jinx-local-words', a local variable. Taken from
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
  (setf (alist-get ?* jinx--save-keys) #'krisb-jinx-save-as-ispell-localword))

;;;; Org-web-tools
(use-package org-web-tools
  :ensure t
  :defer t
  :bind
  ;; TODO 2025-05-22: Revisit this.
  ;; ( :map krisb-yank-keymap
  ;;   ("b" . org-web-tools-insert-link-for-url))
  :init
  ;; Add an org-attach entry for `org-web-tools-archive-attach’.  We
  ;; place this in :init since we want this added to the menu even if
  ;; org-web-tools isn’t currently loaded
  (with-eval-after-load 'org-attach
    (add-to-list 'org-attach-commands
                 '((?w) org-web-tools-archive-attach
                   "Download then attach an archive of a webpage using `org-web-tools'\n")))
  :config
  (advice-add 'org-web-tools-read-url-as-org :after #'view-mode))

;;;; Dictionary
;; See definitions of words from an online dictionary.
;; TODO 2025-05-23: Document these options:
;; - `dictionary-create-buttons’
;; - `dictionary-read-word-function’
;; - `dictionary-search-interface’
(use-package dictionary
  :ensure nil
  ;; Don't forget to install the following packages from the AUR:
  ;; paru -S dict-wn dict-gcide dict-moby-thesaurus dict-foldoc
  ;; :ensure-system-package (dict . dictd) ; Localhost (offline). Don't forget to enable the systemd service
  :hook
  (dictionary-mode-hook . hide-mode-line-mode)
  :bind
  ("C-h =" . krisb-dictionary-dwim)
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-read-dictionary-function 'dictionary-completing-read-dictionary)
  (dictionary-server
   (if (string-equal (string-trim (shell-command-to-string "systemctl is-active dictd"))
                     "active")
       "localhost" "dict.org"))
  :init
  ;; FIXME 2025-05-23: For some reason, if we use :bind to set these
  ;; commands, they are gone in the respective embark keymaps if
  ;; embark is loaded after this package.  So we use this solution
  ;; below.  Am I mistaken?
  (with-eval-after-load 'embark
    (bind-keys :map embark-region-map
               ("=" . krisb-dictionary-dwim)
               :map embark-identifier-map
               ("=" . krisb-dictionary-dwim)))
  :config
  (defun krisb-dictionary-dwim ()
    "Show dictionary definition for word at point.
If region is active, use the region's contents instead."
    (interactive)
    (if-let ((word (if (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end))
                     (thing-at-point 'word :no-properties))))
        (dictionary-search word)
      (message "No word or region selected."))))

;;;; Powerthesaurus
;; Search for synonyms using an online thesaurus.
(use-package powerthesaurus
  :ensure t
  :defer t
  :init
  ;; FIXME 2025-05-23: For some reason, if we use :bind to set these
  ;; commands, they are gone in the respective embark keymaps if
  ;; embark is loaded after this package.  So we use this solution
  ;; below.  Am I mistaken?
  (with-eval-after-load 'embark
    (bind-keys :map embark-region-map
               ("t" . powerthesaurus-lookup-synonyms-dwim)
               ("T" . powerthesaurus-lookup-dwim)
               :map embark-identifier-map
               ("t" . powerthesaurus-lookup-synonyms-dwim)
               ("T" . powerthesaurus-lookup-dwim))))

;;;; Org-modern
;; TODO 2025-05-23: Also document:
;; - `org-modern-fold-stars’
(use-package org-modern
  :ensure t
  :hook
  (org-mode-hook . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda)
  :custom
  ;; Keywords
  (org-modern-keyword nil)

  ;; Headlines
  (org-modern-hide-stars "· ") ; Is affected by the value of `org-hide-leading-stars'
  (org-modern-star 'replace)
  (org-modern-replace-stars "✦⦾‣⬢")

  ;; Todos
  (org-modern-todo t)
  (org-modern-priority t)
  ;; See my value for `org-priority-faces'
  (org-modern-priority-faces
   '((?A :inverse-video t :inherit (bold org-priority))
     (?B :inverse-video t :inherit (bold org-priority))
     (?C :inverse-video t :inherit org-priority)
     (?D :inverse-video t :inherit org-priority)
     (?E :inverse-video t :inherit (shadow org-priority))
     (?F :inverse-video t :inherit (shadow org-priority))))
  ;; See my value for `org-todo-keyword-faces'
  (org-modern-todo-faces
   '(("NEXT" :inherit (bold success org-modern-todo))
     ("TODO" :inherit (org-todo org-modern-todo))
     ("HOLD" :inherit (shadow error org-modern-todo))
     ("MAYBE" :inherit (shadow org-todo org-modern-todo))
     ("DONE" :inherit (bold org-done org-modern-todo))
     ("CANCELED" :inherit (error org-modern-todo))))

  ;; Tags
  (org-modern-label-border 3)
  (org-modern-tag t)

  (org-modern-block-fringe nil) ; Doesn't work well with `olivetti-style' set to 'fancy
  (org-modern-block-name '("⌜" . "⌞"))

  ;; Footnotes
  (org-modern-footnote '(nil (raise 0.15) (height 0.9)))

  ;; Lists
  (org-modern-list '((?+ . "◦")
                     (?- . "–")
                     (?* . "•")))

  ;; Timestamps
  (org-modern-timestamp t)

  ;; Tables
  (org-modern-table t)
  (org-modern-table-vertical 3)
  (org-modern-table-horizontal 0.1)
  :custom-face
  (org-modern-label
   ((t :height 0.9 :width condensed :weight regular :underline nil)))
  (org-modern-todo ((t :weight semibold :inverse-video t :inherit org-modern-label)))
  :config
  ;; TODO 2025-05-23: Revisit this.
  ;; (krisb-modus-themes-setup-faces
  ;;  "org-modern"
  ;;  (setopt org-modern-tag-faces
  ;;          `(("project"
  ;;             :foreground ,(face-background 'default nil t)
  ;;             :background ,(face-foreground 'modus-themes-fg-magenta-cooler nil t)))))
  )

;;;; Org-footnote
;; TODO 2025-05-23: Document:
;; - `org-footnote-define-inline’
(use-package org-footnote
  :ensure nil
  :after org
  :custom
  (org-footnote-section nil)            ; Don't create footnote headline
  (org-footnote-auto-adjust 'sort))

;;;; Org-id
(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-track-globally t)
  (org-id-method 'ts)
  (org-id-link-to-org-use-id 'use-existing))

;;; Emails

;;;; Message
;; Universal email composition mode.  See (info "(message) Variables")
;; for more information.
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
(use-package message
  :ensure nil
  :defer t
  :hook
  (message-setup-hook . message-sort-headers)
  ;; I like to use prose linters.  See my flymake and
  ;; flymake-collection configurations that leverage vale
  (message-mode-hook . flymake-mode)
  (message-mode-hook . olivetti-mode)
  (message-mode-hook . mixed-pitch-mode)
  (message-send-mail-hook . krisb-message-check-subject)
  (message-send-mail-hook . krisb-message-check-from)
  :custom
  (message-directory krisb-email-directory)
  (message-mail-user-agent t)           ; Use `mail-user-agent'

  ;; Citations. See e.g. `message-cite-style-gmail' for the options relevant to
  ;; citations. Importantly, I can set these options buffer locally.
  (message-cite-function 'message-cite-original-without-signature)
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a, %b %d %Y, %N wrote:\n")
  (message-cite-reply-position 'below)

  ;; Composition
  (message-hidden-headers nil)          ; Show all headers
  ;; Generates all headers in the variables
  ;; `message-required-headers’, `message-required-news-headers', and
  ;; `message-required-mail-headers'.  Otherwise, unless another
  ;; package manually adds headers (e.g. mu4e), those headers won't be
  ;; inserted into a message draft buffer.  I enable this to make sure
  ;; that the date header is inserted in a draft.  (No date header
  ;; means the date is set to time 0, which is annoying for querying
  ;; emails via their date using e.g. notmuch.)
  (message-generate-headers-first t)
  (message-wide-reply-confirm-recipients t)
  (message-elide-ellipsis "> [... %l lines elided]\n")
  (message-signature-insert-empty-line t)
  (message-signature "Kind regards,\nKristoffer\n")
  (message-signature-separator "^-- *$")
  (mml-dnd-attach-options t)
  ;; REVIEW 2025-05-23: `message-auto-save-directory’ should be set
  ;; relative to `message-directory’, but based on the order of
  ;; evaluation, it never does so correctly when we set
  ;; `message-directory’ via :custom.  Submit a patch upstream?
  (message-auto-save-directory krisb-email-drafts-directory) ; Directory where drafts are saved
  (message-subject-trailing-was-query 'ask)
  (message-kill-buffer-on-exit t)

  ;; Forwarding
  (message-forward-as-mime t)          ; NOTE 2024-09-27: Experimental
  ;; TODO 2025-05-23: Change value per-email depending on
  ;; `message-cite-reply-position'?
  (message-forward-before-signature nil)
  :config
  ;; TODO 2025-05-23: Revisit this.
  ;; (krisb-modus-themes-setup-faces
  ;;  "message"
  ;;  (set-face-attribute 'message-mml nil :weight 'bold :background bg-sage))

  (with-eval-after-load 'mu4e
    (setq mu4e-attachment-dir (expand-file-name ".attachments/" message-directory)))

  ;; Taken from Doom. Detect empty subjects, and give users an opportunity to
  ;; fill something in
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
                                     (read-string "Set FROM to: " user-mail-address))))))))

;;;; Notmuch
;; TODO 2025-05-23: Document:
;; - `notmuch-identities’
(use-package notmuch
  :ensure t
  ;; For AUR:
  ;; :ensure-system-package (notmuch
  ;;                         (gmi . lieer-git))
  :hook
  (notmuch-show-hook . olivetti-mode)
  (notmuch-show-hook . visual-line-mode)
  (notmuch-show-hook . adaptive-wrap-prefix-mode)
  (notmuch-mua-send-hook . notmuch-mua-attachment-check) ; Also see `notmuch-mua-attachment-regexp'
  (message-send-mail-hook . krisb-notmuch-set-sendmail-args)
  :bind
  (([remap compose-mail] . notmuch-mua-new-mail)
   :map krisb-open-keymap
   ("n" . notmuch)
   :map notmuch-search-mode-map
   ("a" . nil)           ; The default is too easy to hit accidentally
   ("/" . notmuch-search-filter)
   ("r" . notmuch-search-reply-to-thread)
   ("R" . notmuch-search-reply-to-thread-sender)
   :map notmuch-show-mode-map
   ("a" . nil)
   ("r" . notmuch-show-reply)
   ("R" . notmuch-show-reply-sender)
   ;; TODO 2025-05-23: Revisit these.  Also move to krisb-notmuch-ext
   ;; configuration
   ;; ("T" . krisb-notmuch-show-trash-thread-then-next)
   ;; ([remap notmuch-show-advance-and-archive] . krisb-notmuch-show-advance-and-tag)
   )
  :custom
  (mail-user-agent 'notmuch-user-agent)
  
  ;; Hello UI
  (notmuch-hello-sections (list #'notmuch-hello-insert-saved-searches
                                #'notmuch-hello-insert-alltags
                                #'notmuch-hello-insert-recent-searches))
  (notmuch-hello-thousands-separator ",")
  (notmuch-show-all-tags-list t)

  ;; Notmuch-searches
  (notmuch-saved-searches
   '((:name "inbox"                 :query "tag:inbox and (not tag:list or to:krisbalintona@gmail.com)" :sort-order oldest-first :key "i")
     (:name "to-read mailing lists" :query "tag:list and tag:inbox "                                    :sort-order oldest-first :key "l")
     (:name "all mailing lists"     :query "tag:list and not to:krisbalintona@gmail.com"                                         :key "L" )
     (:name "sent"                  :query "tag:sent"                                                   :sort-order newest-first :key "s")
     (:name "drafts"                :query "tag:draft or path:drafts/"                                  :sort-order newest-first :key "d" :search-type unthreaded)
     (:name "archived"              :query "not tag:trash"                                                                       :key "a")
     (:name "all"                   :query "path:**"                                                                             :key "A")
     (:name "trash"                 :query "tag:trash"                                                                           :key "t")))
  ;; See `man' for mbsync and notmuch to see valid search terms. See
  ;; https://www.emacswiki.org/emacs/NotMuch#h5o-2 on how to expunge local files
  ;; via cli
  (notmuch-search-hide-excluded t)
  (notmuch-show-empty-saved-searches t)
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format '(("date" . "%14s ")
                                  ("count" . "%-7s ")
                                  ("authors" . "%-30s ")
                                  ("subject" . "%-75.75s ")
                                  ("tags" . "(%s)")))

  ;; Tags
  (notmuch-archive-tags '("-inbox"))
  (notmuch-message-replied-tags '("+replied"))
  (notmuch-message-forwarded-tags '("+forwarded"))
  (notmuch-show-mark-read-tags '("-unread"))
  (notmuch-draft-tags '("+draft"))
  (notmuch-draft-folder krisb-email-drafts-directory)
  (notmuch-draft-save-plaintext 'ask)
  (notmuch-tagging-keys
   `(("a" notmuch-archive-tags "Archive")
     ("r" notmuch-show-mark-read-tags "Mark read")
     ("f" ("+flagged") "Flag")
     ("s" ("+spam" "-inbox") "Mark as spam")
     ("t" ("+trash" "-inbox") "Trash")))
  (notmuch-tag-formats
   '(("unread" (propertize tag 'face 'notmuch-tag-unread))
     ("flagged" (propertize tag 'face 'notmuch-tag-flagged))))
  (notmuch-tag-deleted-formats
   '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
     (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

  ;; Notmuch-show-mode (i.e. reading emails)
  (notmuch-show-relative-dates t)
  (notmuch-show-all-multipart/alternative-parts nil)
  (notmuch-show-indent-multipart nil)
  (notmuch-show-indent-messages-width 3) ; We can toggle indentation anyway
  (notmuch-show-part-button-default-action 'notmuch-show-interactively-view-part)
  (notmuch-show-text/html-blocked-images ".") ; Block everything
  (notmuch-wash-wrap-lines-length nil)
  (notmuch-unthreaded-show-out t)
  (notmuch-message-headers-visible nil)
  (notmuch-message-headers '("To" "Cc" "Date" "Subject"))
  (notmuch-multipart/alternative-discouraged
   '("text/html" "multipart/related"
     ;; FIXME 2025-05-23: This doesn’t work?
     "text/x-patch"))

  ;; Notmuch-tree-mode
  (notmuch-tree-show-out nil)
  (notmuch-tree-result-format '(("date" . "%12s  ")
                                ("authors" . "%-20s  ")
                                ((("tree" . "%s")
                                  ("subject" . "%s"))
                                 . " %-85.85s  ")
                                ("tags" . "(%s)")))
  (notmuch-tree-outline-enabled nil)

  ;; Email composition
  (notmuch-mua-compose-in 'current-window)
  (notmuch-mua-hidden-headers nil)
  (notmuch-address-command 'internal)
  (notmuch-address-internal-completion '(sent nil))
  (notmuch-always-prompt-for-sender t)  ; See also the `notmuch-mua-prompt-for-sender' function
  (notmuch-mua-cite-function 'message-cite-original-without-signature)
  (notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (notmuch-mua-user-agent-function nil)
  (notmuch-maildir-use-notmuch-insert t)
  (notmuch-wash-citation-lines-prefix 0)
  (notmuch-wash-citation-lines-suffix 0)
  (notmuch-crypto-process-mime t)
  (notmuch-crypto-get-keys-asynchronously t)
  ;; See `notmuch-mua-send-hook'
  (notmuch-mua-attachment-regexp (concat "\\b\\("
                                         "attache\?ment\\|attached\\|attach\\|"
                                         "pi[èe]ce\s+jointe?"
                                         "\\)\\b"))

  ;; Sending emails.
  ;; Use Lieer to send emails.  Also see
  ;; `krisb-notmuch-set-sendmail-args'.  Read
  ;; https://github.com/gauteh/lieer/wiki/Emacs-and-Lieer.
  (sendmail-program (if (executable-find "gmi") "sendmail"))
  (send-mail-function 'sendmail-send-it)
  (notmuch-fcc-dirs nil) ; Gmail already copies sent emails, so don't move them elsewhere locally
  :config
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

  ;; TODO 2025-05-23: Revisit this.
  ;; REVIEW 2024-09-26: Prot's lin package apparently makes disabling
  ;; this better?
  (with-eval-after-load 'lin
    (remove-hook 'notmuch-search-hook #'notmuch-hl-line-mode))

  ;; TODO 2025-05-23: Revisit this.
  ;; Prefer not to have emails recentered as I readjust them
  (advice-add 'notmuch-show-message-adjust :override #'ignore))

;;; Guix
(use-package guix
  :ensure nil      ; Although available via MELPA, we install via Guix
  :defer t
  :hook
  (guix-build-log-mode-hook . (lambda () (setq truncate-lines t)))
  (guix-build-log-mode-hook . guix-build-log-phase-toggle-all))

;;; Uncategorized

;; Restart or close Emacs
(defun krisb-restart-or-kill-emacs (&optional arg restart)
  "Kill Emacs.
If called with RESTART (`universal-argument’ interactively) restart
Emacs instead. Passes ARG to `save-buffers-kill-emacs'."
  (interactive "P")
  (save-buffers-kill-emacs arg (or restart (equal arg '(4)))))
(bind-key [remap save-buffers-kill-terminal] #'krisb-restart-or-kill-emacs)

;; Unfill paragraph.  Protesilaos's
;; `prot-simple-unfill-region-or-paragraph'
(defun krisb-unfill-region-or-paragraph (&optional beg end)
  "Unfill paragraph or, when active, the region.
Join all lines in region delimited by BEG and END, if active, while
respecting any empty lines (so multiple paragraphs are not joined, just
unfilled).  If no region is active, operate on the paragraph.  The idea
is to produce the opposite effect of both `fill-paragraph' and
`fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph))))
(bind-key "M-Q" #'krisb-unfill-region-or-paragraph)

;; Single-line scrolling
(bind-keys
 ("C-M-S-p" . scroll-down-line)
 ("C-M-S-n" . scroll-up-line))

;; Rapid joining lines and inserting newlines tailored for prose
;; writing
(defun krisb-open-line-above-goto ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current
mode. Credit to https://emacsredux.com/blog/2013/06/15/open-line-above/"
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
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

;; `indent-for-tab-command' functionality: what happens when you press
;; TAB?
(setopt tab-always-indent 'complete
        tab-first-completion 'word)

;; Keep the cursor out of the read-only portions of the minibuffer
(setopt minibuffer-prompt-properties
        '( read-only t
           cursor-intangible t
           face minibuffer-prompt))

;; Use DWIM case commands
(bind-keys
 ([remap upcase-word] . upcase-dwim)
 ([remap downcase-word] . downcase-dwim)
 ([remap capitalize-word] . capitalize-dwim))

;; Recenter upon `next-error'
(setopt next-error-recenter '(4))

;; Disable the ring-bell (it's annoying)
(setopt ring-bell-function #'ignore)

;; Enable `delete-selection-mode'.  When selecting text, if typing new
;; text, replace the selected text with the new text
(delete-selection-mode t)

;; Show context menu from right-click
(when (display-graphic-p) (context-menu-mode 1))

;; Avoid collision of mouse with point
(mouse-avoidance-mode 'jump)

;; Visual-line-mode in *Messages* buffer
(add-hook 'messages-buffer-mode-hook #'visual-line-mode)

;; Undo frame deletions
(undelete-frame-mode 1)

;; So-long-mode everywhere
(global-so-long-mode 1)

;; Stretch cursor to the glyph width
(setopt x-stretch-cursor t)

;; Middle-click pastes at point, not at mouse
(setopt mouse-yank-at-point t)

;; Don't do anything with inactive mark
(setopt mark-even-if-inactive nil)

;; Strategy for uniquifying buffer names
(setopt uniquify-buffer-name-style 'forward)

;; Behavior for `cycle-spacing-actions'.  Read the docstring for an
;; explanation (or try it out!)
(setopt cycle-spacing-actions '(just-one-space (delete-all-space -) restore))

;; Word wrapping.  Continue wrapped lines at whitespace rather than
;; breaking in the middle of a word.
(setopt word-wrap t)

;; Don’t warn when advising
(setopt ad-redefinition-action 'accept)

;; Prefer UTF-8 file and selection encoding
(prefer-coding-system 'utf-8)
;; The clipboard on Windows is often a wider encoding (UTF-16), so
;; leave Emacs to its own devices there.  Otherwise, encode text into
;; the clipboard into UTF-8
(unless (eq system-type 'windows-nt)
  (setopt selection-coding-system 'utf-8))

;; Prefer unicode charset
(set-charset-priority 'unicode)

;; Set `sentence-end-double-space' conditionally
(defun krisb-sentence-end-double-space-setup ()
  "Set up the value for `sentence-end-double-space'."
  (setq-local sentence-end-double-space
              (cond ((derived-mode-p '(prog-mode conf-mode log-edit-mode)) t)
                    ((derived-mode-p 'text-mode) nil))))

(dolist (mode '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode #'krisb-sentence-end-double-space-setup))

;; Trash
(setopt trash-directory (no-littering-expand-var-file-name "trash")
        delete-by-moving-to-trash t)

;; Message for total init time after startup
(add-hook 'elpaca-after-init-hook (lambda () (message "Total startup time: %s" (emacs-init-time))))

;; Frame backaground transparency toggle
(add-to-list 'default-frame-alist '(alpha-background . 100))
(defun krisb-toggle-window-transparency (&optional arg)
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
(bind-key "<f9>" #'krisb-toggle-window-transparency)

;; `duplicate-dwim' binding
;; TODO 2025-05-22: Document the `duplicate-line-final-position'and
;; `duplicate-region-final-position' user options
(bind-key "C-x ;" #'duplicate-dwim)

;; Enable all disabled commands
(setopt disabled-command-function nil)
