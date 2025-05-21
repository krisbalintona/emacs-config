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

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;; Support multiple Emacs versions.  Taken from
;; https://github.com/progfolio/elpaca/wiki/Using-multiple-Emacs-versions
(defvar elpaca-builds-directory (expand-file-name (concat "builds-" emacs-version)) elpaca-directory)
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

;;; Set and load custom file
;; (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
;; (add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

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
      :default-weight regular
      :default-slant normal
      :default-width normal
      :default-height 165

      ;; Alternatives
      ;; "Hack Nerd Font Mono"
      :fixed-pitch-family "Iosevka"
      :fixed-pitch-weight nil
      :fixed-pitch-slant nil
      :fixed-pitch-width nil
      :fixed-pitch-height nil

      :fixed-pitch-serif-family nil
      :fixed-pitch-serif-weight nil
      :fixed-pitch-serif-slant nil
      :fixed-pitch-serif-width nil
      :fixed-pitch-serif-height nil

      ;; 2025-04-21: This is my own bespoke setting.  Fontaine works fine with
      ;; it set; I use it elsewhere (e.g., eat.el).
      :term-family "IosevkaTermSS04 Nerd Font" ; For terminals

      ;; Alternatives:
      ;; "LiterationSerif Nerd Font"       ; Variable
      ;; "Latin Modern Mono Prop"          ; Monospace
      ;; "Sans Serif"
      ;; "Open Sans" (1.1 height)
      :variable-pitch-family "Overpass Nerd Font Propo"
      :variable-pitch-weight nil
      :variable-pitch-slant nil
      :variable-pitch-width nil
      :variable-pitch-height 1.2

      :mode-line-active-family "JetBrainsMono Nerd Font"
      :mode-line-active-weight nil
      :mode-line-active-slant nil
      :mode-line-active-width nil
      :mode-line-active-height 0.93

      :mode-line-inactive-family "JetBrainsMono Nerd Font"
      :mode-line-inactive-weight nil
      :mode-line-inactive-slant nil
      :mode-line-inactive-width nil
      :mode-line-inactive-height 0.93

      :header-line-family nil
      :header-line-weight nil
      :header-line-slant nil
      :header-line-width nil
      :header-line-height 1.0

      :line-number-family nil
      :line-number-weight nil
      :line-number-slant nil
      :line-number-width nil
      :line-number-height nil

      :tab-bar-family "Overpass Nerd Font"
      :tab-bar-weight nil
      :tab-bar-slant nil
      :tab-bar-width nil
      :tab-bar-height 0.93

      :tab-line-family nil
      :tab-line-weight nil
      :tab-line-slant nil
      :tab-line-width nil
      :tab-line-height nil


      :bold-slant nil
      :bold-weight bold
      :bold-width nil
      :bold-height nil

      :italic-family nil
      :italic-weight nil
      :italic-slant italic
      :italic-width nil
      :italic-height nil

      :line-spacing nil)))
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
  :ensure (:wait t))

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
  :ensure t)

;;; A step below
;;; Savehist
;; Make history of certain things (e.g. minibuffer) persistent across sessions
(use-package savehist
  :custom
  (history-length 10000)
  ;; TODO 2025-05-19: Revisit this.
  ;; (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 30)
  :config
  ;; TODO 2025-05-19: Revisit this.
  ;; (dolist (var '(kill-ring
  ;;                Info-history-list
  ;;                last-kbd-macro
  ;;                kmacro-ring))
  ;;   (add-to-list 'savehist-additional-variables var))
  (savehist-mode 1))

;;; Two steps below
;;;; Desktop
;; Save buffers across Emacs sessions
;;
;; TODO 2025-05-20: Document in literate configuration prose.
;; See also `desktop-globals-to-save' and `desktop-locals-to-save'
(use-package desktop
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
