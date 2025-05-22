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
(when (package-installed-p 'use-package)
  ;; Use package.el unless we're using elpaca
  (if (and package-enable-at-startup (not elpaca-installer-version))
      (package-install 'use-package)
    (elpaca (use-package :wait t))))

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

;;;; Enable theme based on time of day
(defun krisb-enable-theme-time-of-day (&optional day-start night-start)
  "Enables the theme based on time of day.
Night time begins at NIGHT-START hour and daytime begins at DAY-START
hour.  If NIGHT-START is nil, default to 19.  If DAY-START is nil,
default to 8."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (let ((hour (string-to-number (format-time-string "%H")))
	(day-start (or day-start 8))
	(night-start (or night-start 19))
	(light-theme 'doric-marble)
	(dark-theme 'doric-dark))
    ;; Dark theme between NIGHT-START and DAY-START
    (load-theme (if (or (<= night-start hour) (<= hour day-start))
		    dark-theme light-theme))))

;;;; Doric-themes
;; Minimalistic but visible and effective themes.  (Cf. modus-themes
;; and standard-themes.)
;;
;; TODO 2025-05-20: Document this in literate config.
;; See also `doric-themes-to-toggle' and `doric-themes-to-rotate'
(use-package doric-themes
  ;; TODO 2025-05-20: Remove the :repo specification after
  ;; doric-themes gets added to ELPA
  :ensure (:repo "https://github.com/protesilaos/doric-themes")
  :demand t
  :bind
  (("<f8>" . doric-themes-toggle)
   ("C-<f8>" . doric-themes-select)
   ("M-<f8>" . doric-themes-rotate))
  :custom
  (doric-themes-to-toggle '(doric-marble doric-dark))
  :config
  (krisb-enable-theme-time-of-day))

;;;; Electric
;; Convenient DWIM, out-of-the-way while you edit
;;
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;; 
;; - `electric-quote-inhibit-functions'
(use-package electric
  :ensure nil
  :demand t
  :hook
  (text-mode-hook . electric-quote-local-mode)
  :custom
  ;; TODO 2025-05-20: Revisit this.
  ;; (electric-quote-comment nil)
  ;; TODO 2025-05-20: Revisit this.
  ;; (electric-quote-string nil)
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (electric-pair-mode 1)
  (electric-indent-mode 1))

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
  :demand t
  :hook
  ((text-mode-hook prog-mode-hook conf-mode-hook) . krisb-desktop--save-narrowing)
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
  
  (desktop-restore-eager 10)
  (desktop-restore-forces-onscreen nil)
  (desktop-restore-frames t)
  (desktop-restore-in-current-display nil)
  :config
  (desktop-save-mode 1)

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
      (apply #'narrow-to-region (cdr misc))
      (message "Narrowed %s" n)))
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

;;; Three steps below

;;;; Vc-jj
;; Integration between vc.el and the jujutsu (JJ) version control
;; system
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
  (minibuffer-setup . krisb-corfu-enable-in-minibuffer-conditionally)
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
  (corfu-separator ?\s)	; Use space
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
  :demand t
  ;; TODO 2025-05-20: Revisit this.
  ;; :bind
  ;; ( :map krisb-file-keymap
  ;;   ("r" . recentf-open-files))
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  :config
  (recentf-mode 1))

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
(use-package consult
  :ensure t
  :bind
  (("C-x B" . consult-buffer)
   ;; Remaps
   ([remap bookmark-jump] . consult-bookmark)
   ([remap yank-pop] . consult-yank-pop)
   ([remap goto-line] . consult-goto-line)
   ([remap recentf-open-files] . consult-recent-file)
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

(setopt mode-line-compact 'long	; Emacs 28
        mode-line-right-align-edge 'window
        mode-line-percent-position '(-3 "%p") ; Don't show percentage of position in buffer
        mode-line-position-line-format '(" %l")
        mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28

;; TODO 2025-05-20: Revisit this.
;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 (:propertize
;;                  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
;;                   mode-line-window-dedicated)
;;                  display (min-width (6.0)))
;;                 mode-line-frame-identification
;;                 mode-line-buffer-identification "   "
;;                 mode-line-position
;;                 mode-line-format-right-align
;;                 (project-mode-line project-mode-line-format) "   "
;;                 mode-line-modes
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))
(setopt mode-line-format
	'("%e" mode-line-front-space
	  (:propertize
	   ("" mode-line-mule-info mode-line-client mode-line-modified
	    mode-line-remote mode-line-window-dedicated)
	   display (min-width (6.0)))
	  mode-line-frame-identification
	  mode-line-buffer-identification "   "
	  mode-line-position
	  mode-line-format-right-align
	  (project-mode-line project-mode-line-format)
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
  (quit-restore-window-no-switch t)	; Emacs 31
  (kill-buffer-quit-windows t))		; Emacs 31

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
  ;; TODO 2025-05-20: Revisit this.
  ;; ( :map krisb-open-keymap
  ;;   ("s" . eat)
  ;;   :map project-prefix-map
  ;;   ("s" . eat-project))
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
(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer)
  :bind*
  ( :map ibuffer-mode-map
    ("SPC" . scroll-up-command)
    ("DEL" . scroll-down-command)
    ("* d" . krisb-ibuffer-mark-displayed-buffers)))

;;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  :ensure t
  :defer t
  :hook
  (scratch-create-buffer-hook . krisb-scratch-buffer-setup)
  ;; TODO 2025-05-20: Revisit this.
  ;; :bind
  ;; ( :map krisb-open-keymap
  ;;   ("S". scratch))
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
  (org-imenu-depth 7)		     ; Show more than just 2 levels...
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
  (vc-find-revision-no-save t))		; Emacs 31

(use-package vc-git
  :ensure nil
  :hook
  (vc-git-log-edit-mode-hook . auto-fill-mode)
  :custom
  (vc-git-log-edit-summary-target-len (+ 50 (length "Summary")))
  (vc-git-log-edit-summary-max-len (+ 70 (length "Summary")))
  (vc-git-diff-switches	   ; Show summary diff summary in diff headers
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
  (eldoc-help-at-pt t)			; Emacs 31.1
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
  ;; `electric-pair-mode'.  It should also ideally be an unused (or at
  ;; least very rare) comment delimiter to avoid TAB indenting the
  ;; line when `tab-always-indent' is \\='complete
  (tempel-trigger-prefix "=")
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

;;; Uncategorized

;; Bind `restart-emacs'

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
