;; -*- lexical-binding: t; -*-

;;; Necessary for startup and the remainder of the config

;;;; Add modules and lisp to load path
(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))


;;;; Package.el
;; TODO 2025-05-20: Because I use elpaca, "archive" my package.el
;; configuration somewhere.
;; Initialize package resources
;; (setopt package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
;;                            ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
;;                            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;                            ("melpa" . "https://melpa.org/packages/"))
;;         package-archive-priorities '(("gnu-elpa" . 4)
;;                                      ("nongnu" . 3)
;;                                      ("gnu-elpa-devel" . 2)
;; 				     ("melpa" . 1))
;;         package-install-upgrade-built-in t

;;         load-prefer-newer t) ; Do not load outdated byte code files
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
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

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
  (("<f5>" . doric-themes-toggle)
   ("C-<f5>" . doric-themes-select)
   ("M-<f5>" . doric-themes-rotate))
  :config
  (doric-themes-select 'doric-dark))

;;;; Electric
;; Convenient DWIM, out-of-the-way while you edit
;;
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;; 
;; - `electric-pair-inhibit-predicate'
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

  (dolist (var '((kill-ring . 10000)
                 Info-history-list
		 register-alist))
    (add-to-list 'savehist-additional-variables var)))

;;;; Desktop
;; Save buffers across Emacs sessions
;;
;; TODO 2025-05-20: Document in literate configuration prose.
;; See also `desktop-globals-to-save' and `desktop-locals-to-save'
(use-package desktop
  :ensure nil
  :demand t
  :hook
  (text-mode-hook . krisb-desktop--save-narrowing)
  (prog-mode-hook . krisb-desktop--save-narrowing)
  (conf-mode-hook . krisb-desktop--save-narrowing)
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

  ;; Also save and restore narrowing state
  (defun krisb-desktop--save-narrowing ()
    "Save narrowed information.
Taken from
https://www.reddit.com/r/emacs/comments/162cjki/comment/jxzrthx/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1."
    (setq desktop-save-buffer
          (lambda (_d) (if (buffer-narrowed-p)
                           (list 'narrowed (point-min) (point-max))))))

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
  :demand t
  :config
  (repeat-mode 1))

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
  :requires vertico
  :ensure nil
  :hook
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :bind
  ( :map vertico-map
    ("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    ("M-DEL" . vertico-directory-delete-word)))

;; On-demand change the type of UI
(use-package vertico-multiform
  :requires vertico
  :ensure nil
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
     ))
  :config
  (vertico-multiform-mode 1))

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
   ;; TODO 2025-05-20: Revisit this.
   ;; :map search-map                ; The `M-s' prefix
   ;; ("i" . consult-info)
   ;; ("g" . consult-git-grep)
   ;; ("G" . consult-grep)
   ;; ("r" . consult-ripgrep)
   ;; ("f" . consult-find)
   ;; ("F" . consult-locate)
   :map org-mode-map
   ([remap consult-outline] . consult-org-heading))
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
;; Package that makes Emacs mouse scrolling buttery smooth
(use-package ultra-scroll
  :ensure (:repo "https://github.com/jdtsmith/ultra-scroll")
  :demand t
  :init
  (setq scroll-conservatively 101       ; As instructed by the README
        scroll-margin 0)
  :config
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
