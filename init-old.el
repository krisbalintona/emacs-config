;; -*- lexical-binding: t; -*-

;;; Necessary for startup and the remainder of the config

;; (package-initialize)

;;;; Elpaca
;; Get build date for Emacs installed via Guix.  Run this before
;; elpaca is installed.  This is necessary for the Guix and Nix
;; operating systems wherein `emacs-build-time', which elpaca uses
;; internally, is not set.  See
;; https://github.com/progfolio/elpaca/wiki/Usage-with-Nix#manually-setting-elpaca-core-date
;; for the Nix version.
(defun krisb-guix-emacs-build-date ()
  "Extract YYYYMMDD from a datetime string.
For example, \“2025-05-19 15:20:57.782742938 -0500\”."
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

;; Install elpaca and use-package integration (via use-package's
;; :ensure keyword)
(with-eval-after-load 'use-package
  (elpaca (elpaca-use-package :wait t)
    (elpaca-use-package-mode 1)))

;; TODO 2025-05-20: Add note about enabling the mode below on systems
;; that cannot create symlinks
;; (elpaca-no-symlink-mode 1)

;;;;; Upgrade built-ins
;; In order to ensure built-in packages are upgraded by elpaca, we
;; need to ensure they are activated by elpaca before they are loaded.
;; Otherwise unexpected behavior may occur, including not using the
;; upgraded version.  As such, we force elpaca to activate them early
;; in the configuration, prior to anything else potentially loading
;; them.
(elpaca '(org :wait t))
(elpaca '(org-capture :wait t
                      :repo "https://github.com/krisbalintona/org-mode.git"
                      :branch "org-capture"
                      :files ("lisp/org-capture.el")))
(elpaca '(transient :wait t))

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

;; Imenu integration: create a "packages" group that searches for
;; use-package declarations
(setopt use-package-enable-imenu-support t)

;;;; My variables, functions, macros, and keymaps

;;; A step below

;;;; Theming

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

;;;; Fixing M-SPC under WSLg
;; 2024-10-29: There is currently an issue in WSLg that prevents
;; Alt+Space from being caught by X11.  A workaround is described in
;; https://github.com/microsoft/wslg/issues/1068#issuecomment-1817786154.
;; Use PowerToys Keyboard Manager to remap the Alt+Space shortcut to
;; Alt+F13 in WSLg (inside msrdc.exe processes), then use xmodmap to
;; redirect Alt+F13 to M-SPC.  (Instead of creating an xmodmap file,
;; we can just use the shell command below.)  Of course, xmodmap is an
;; X11 tool, so this is incompatible with the PGTK toolkit.
(when (and (not (string-match-p "--with-pgtk" system-configuration-options))
           (executable-find "xmodmap"))
  (shell-command "xmodmap -e 'keycode 191 = space'"))

;;; Two steps below
;;;; Don’t let GTK override key sequences on wayland
;; See the section titled “Certain keys such as 'C-S-u' are not
;; reported correctly.” in etc/PROBLEMS (M-x C-h C-p).
(when (fboundp 'pgtk-use-im-context)
  ;; 2025-06-03: In my experience, this function should only be called
  ;; after the frame has been set up.
  (add-hook 'window-setup-hook (lambda () (pgtk-use-im-context nil))))

;;; Three steps below
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

;;;; Grep
;; TODO 2025-05-20: Document the user options below in the literate
;; config:
;;
;; - `grep-save-buffers'
(use-package grep
  :ensure nil
  :custom
  (grep-use-headings t)
  (grep-scroll-output t)
  :config
  ;; Additions to `display-buffer-alist’
  (add-to-list 'display-buffer-alist
               '((major-mode . grep-mode)
                 (display-buffer-pop-up-window)
                 (post-command-select-window . t)))

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

;;;; Avy
;; Quickly jump to any character.  For a great resource on the power
;; of avy, see https://karthinks.com/software/avy-can-do-anything/.

;; TODO 2025-05-24: Document:
;; - `avy-background’
(use-package avy
  :ensure t
  :bind
  (("C-; C-;" . avy-goto-char-timer)
   ("C-; s" . avy-goto-symbol-1)
   ("C-; l" . avy-goto-line)
   ("C-; p" . krisb-avy-goto-parens))
  :custom
  ;; For descriptions of all the options, see the avy wiki:
  ;; https://github.com/abo-abo/avy/wiki/defcustom
  (avy-all-windows t)
  (avy-case-fold-search nil)
  (avy-single-candidate-jump nil)
  (avy-timeout-seconds 0.3)
  (avy-keys '(?a ?w ?r ?u ?i ?o ?p))
  ;; TODO 2025-05-24: Revisit this.
  ;; (avy-highlight-first t)
  ;; 2025-04-03: For a description of the styles see
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style.  The
  ;; primary considerations are readability with many candidates and
  ;; ability see and input the sequences quickly.  I typically favor
  ;; the speedy input of sequences over readability.  So, I set a
  ;; default to 'words but for commands that have many candidates I
  ;; prefer 'at (which shows single characters only; this is good for
  ;; readability but unfavorable for speed).  (Note: The 'pre and
  ;; 'post options make the text shift.)
  (avy-style 'words)
  (avy-styles-alist
   '((krisb-avy-goto-parens . at)))
  ;; Alist of avy actions.  See the "Remembering to Avy" section in in
  ;; https://karthinks.com/software/avy-can-do-anything/ for the
  ;; rationale behind the bindings below
  (avy-dispatch-alist
   '((?k . avy-action-kill-stay)
     (?K . krisb-avy-action-kill-whole-line)
     (?t . avy-action-teleport)
     (?T . krisb-avy-action-teleport-whole-line)
     (?w . avy-action-copy)
     (?W . krisb-avy-action-copy-whole-line)
     (?y . avy-action-yank)
     (?Y . krisb-avy-action-yank-whole-line)
     (?m . avy-action-mark)
     (?z . avy-action-zap-to-char)
     (?. . krisb-avy-action-embark)
     (?h . krisb-avy-action-help)
     (?= . krisb-avy-action-dictionary)
     (?e . krisb-avy-action-eval)
     (?w . krisb-avy-action-kill-ring-save)))
  ;; TODO 2025-05-24: Revisit this.
  ;; (avy-orders-alist
  ;;  '((avy-goto-char . krisb-avy-order-farthest)
  ;;    (avy-goto-char-2 . krisb-avy-order-farthest)
  ;;    (avy-goto-word-0 . krisb-avy-order-farthest)
  ;;    (avy-goto-word-1 . krisb-avy-order-farthest)
  ;;    (avy-goto-char-timer . krisb-avy-order-farthest)
  ;;    (krisb-avy-goto-parens . krisb-avy-order-farthest)))
  :config
  ;; TODO 2025-05-24: Revisit this.
  ;; (krisb-modus-themes-setup-faces
  ;;  "avy"
  ;;  ;; By default, modus-themes inherits from the bold face.  But this
  ;;  ;; is undesirable for variable-pitch faces since it causes the text
  ;;  ;; to shift.  To avoid this, we set the inherited face attribute to
  ;;  ;; the default with bold removed.
  ;;  (set-face-attribute 'avy-lead-face nil :inherit '(modus-themes-search-current modus-themes-reset-soft))
  ;;  (set-face-attribute 'avy-lead-face-0 nil :inherit '(modus-themes-search-current modus-themes-reset-soft))
  ;;  (set-face-attribute 'avy-lead-face-1 nil :inherit '(modus-themes-search-current modus-themes-reset-soft))
  ;;  (set-face-attribute 'avy-lead-face-2 nil :inherit '(modus-themes-search-current modus-themes-reset-soft)))

  ;; TODO 2025-05-24: Revisit this.
  (defun krisb-avy-order-farthest (x)
    (- (abs (- (if (numberp (car x))
                   (car x)
                 (caar x))
               (point)))))

  ;; Goto (i.e. selection) commands
  (defun krisb-avy-goto-parens ()
    "Go to an open or close parens."
    (interactive)
    (let ((avy-command this-command))
      (avy-with krisb-avy-goto-parens
                ;; There will be many candidates if called in an e.g. elisp
                ;; buffer.  The solution is changing the `avy-style'
                ;; appropriately, either by setting `avy-style' directly or
                ;; `avy-styles-alist'.
                (avy-jump (rx (or "(" ")")))
                ;; When jumping to a closing parens, move the point forward
                ;; one character, since in these cases, I want the point to
                ;; end after the parens, not before
                (when (eq (char-after) (string-to-char ")"))
                  (forward-char))))
    t)

  ;; Actions
  (defun krisb-avy-action-help (pt)
    "Help avy action."
    (save-excursion
      (goto-char pt)
      (if (featurep 'helpful)
          (helpful-at-point)
        (describe-symbol (symbol-at-point))))
    (when (featurep 'helpful)
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Additional avy dispatch actions. Most are inspired or taken from
  ;; https://karthinks.com/software/avy-can-do-anything/ Embark
  (defun krisb-avy-action-embark (pt)
    "Embark avy action."
    (require 'embark)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun krisb-avy-action-dictionary (pt)
    "Dictionary avy action."
    (save-excursion
      (goto-char pt)
      (krisb-dictionary-dwim))
    ;; If with `universal-arg', don't switch to help buffer
    (when current-prefix-arg
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun krisb-avy-action-eval (pt)
    "Eval avy action."
    (save-excursion
      (goto-char pt)
      (if (fboundp 'eros-eval-last-sexp)
          (call-interactively 'eros-eval-last-sexp)
        (call-interactively 'eval-last-sexp)))
    t)

  (defun krisb-avy-action-kill-whole-line (pt)
    "Kill whole line avy action.
Taken from https://karthinks.com/software/avy-can-do-anything/."
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun krisb-avy-action-teleport-whole-line (pt)
    "Teleport whole line avy action.
Taken from https://karthinks.com/software/avy-can-do-anything/."
    (krisb-avy-action-kill-whole-line pt)
    (save-excursion (yank) t))

  (defun krisb-avy-action-copy-whole-line (pt)
    "Copy whole line avy action.
Taken from https://karthinks.com/software/avy-can-do-anything/."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun krisb-avy-action-yank-whole-line (pt)
    "Yank whole line avy action.
Taken from https://karthinks.com/software/avy-can-do-anything/."
    (krisb-avy-action-copy-whole-line pt)
    (save-excursion (yank) t)))

;;;; `display-buffer-alist’
;; TODO 2025-05-27: Eventually eliminate this section since I aim to
;; disperse additions to `display-buffer-alist' across my package
;; configurations.
(setq display-buffer-alist
      `(;; Occur
        ("\\*Occur"
         (display-buffer-reuse-mode-window display-buffer-pop-up-window display-buffer-below-selected)
         (window-height . fit-window-to-buffer)
         (post-command-select-window . t))))

;;; Four steps below

;;;; Consult
;; Get enhanced or fancy versions of many built-in commands
;; TODO 2025-05-23: Document:
;; - `consult-recent-file’
(use-package consult
  :bind
  (;; TODO 2025-05-20: Revisit this.
   ;; ([remap point-to-register] . consult-register-store)
   :map minibuffer-local-map
   ([remap next-matching-history-element] . consult-history)
   ([remap previous-matching-history-element] . consult-history))
  :init
  (with-eval-after-load 'dired
    (bind-keys :map dired-mode-map
               ;; Dired binds its own commands in M-s f, so I rebind
               ;; these specially in `dired-mode-map'
               ("M-s f f" . consult-find)
               ("M-s f F" . consult-locate)))
  :config
  ;; TODO 2025-05-20: Revisit this.
  ;; (require 'krisb-consult-ext)
  
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

  ;; Eshell is not loaded at startup, so we have to delay our binding
  ;; in `eshell-mode-map’
  (with-eval-after-load 'eshell
    (bind-key [remap eshell-previous-matching-input] #'consult-history 'eshell-mode-map)))

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
  ;; 2025-06-03: As instructed by the README: `scroll-margin' > 0 not
  ;; yet supported
  (setopt scroll-margin 0)
  (ultra-scroll-mode 1))

;;;; Marginalia
;; Enable richer annotations in minibuffer
(use-package marginalia
  :disabled t     ; 2025-06-07: Trying out just `completions-detailed'
  :ensure t
  :demand t
  :custom
  (marginalia-max-relative-age 0)       ; Always use absolute age
  (marginalia-align 'left)              ; Causes most alignment in my experience
  :config
  (marginalia-mode 1))

;;;; Mode-line-maker (and `mode-line-format')
;; Library for easier alignment of mode-line, header-line, and
;; tab-line.
(use-package mode-line-maker
  :demand t
  :ensure (:repo "https://github.com/rougier/mode-line-maker")
  :custom
  (mode-line-compact 'long) ; Emacs 28
  (mode-line-right-align-edge 'right-fringe)
  (mode-line-percent-position '(-3 "%p")) ; Don't show percentage of position in buffer
  (mode-line-position-line-format '(" %l"))
  (mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
  (mode-line-format (mode-line-maker
                     '("%e" mode-line-front-space
                       (:propertize
                        ("" mode-line-mule-info mode-line-client mode-line-modified
                         mode-line-remote mode-line-window-dedicated)
                        display (min-width (6.0)))
                       mode-line-frame-identification
                       (project-mode-line ("" project-mode-line-format "   "))
                       mode-line-buffer-identification "   "
                       mode-line-position)
                     '((vc-mode vc-mode) "  "
                       mode-line-modes
                       mode-line-misc-info
                       mode-line-end-spaces)))
  (fringes-outside-margins t)
  :config
  ;; TODO 2025-07-10: Ask on emacs-devel why this isn't a defcustom.
  (setq mode-line-defining-kbd-macro (propertize " Macro" 'face 'mode-line-emphasis))

  ;; Add segments to `global-mode-string'
  (add-to-list 'global-mode-string '(vc-mode (:eval (concat vc-mode " ")))))

;;;; Mistty
(use-package mistty
  :ensure t
  :defer t)

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
            (when-let* (((eq 'tab (car tab)))
                        (tab-info (cdr tab))
                        (tab-window-conf (cdr (assq 'wc tab-info))))
              (set-window-configuration tab-window-conf))
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

;;;; Hippie-expand
(use-package hippie-exp
  :ensure nil
  :bind
  ([remap dabbrev-expand] . hippie-expand))

;;;; Bufferlo
(use-package bufferlo
  :disabled t   ; 2025-06-02: Performance issues with both minor modes
  :ensure t
  :demand t
  :bind
  (("C-c g g" . bufferlo-anywhere-disable-prefix)
   ("C-c g G" . bufferlo-anywhere-enable-prefix))
  :custom
  (bufferlo-anywhere-filter-type 'include)
  (bufferlo-anywhere-filter
   '(switch-to-buffer
     switch-to-buffer-other-frame
     switch-to-buffer-other-tab
     switch-to-buffer-other-window
     project-switch-to-buffer))
  (bufferlo-hidden-buffers
   '("\\*Org Agenda"))

  ;; I prefer ‘window over ‘clone so the tab local bufferlo list is
  ;; fresh for every new tab
  (tab-bar-new-tab-choice 'window)

  ;; Bookmarks
  (bufferlo-bookmarks-auto-save-interval 60)
  (bufferlo-bookmark-inhibit-bookmark-point t)
  (bufferlo-bookmark-tab-replace-policy 'new)
  (bufferlo-bookmark-tab-save-on-close 'when-bookmarked)
  (bufferlo-bookmarks-save-at-emacs-exit 'all)
  :config
  (bufferlo-mode 1)
  (bufferlo-anywhere-mode 1)

  ;; Customize `consult-buffer' sources in a way amenable to the
  ;; bufferlo workflow
  (with-eval-after-load 'consult
    (delq 'consult--source-buffer consult-buffer-sources)

    (defvar krisb-bufferlo-consult--source-local-buffers
      (list :name "Bufferlo Local Buffers"
            :narrow   ?b
            :category 'buffer
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'bufferlo-local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Local Bufferlo buffer candidate source for consult-buffer.")

    (defvar krisb-bufferlo-consult--source-other-buffers
      (list :name "Bufferlo Other Buffers"
            :narrow   ?B
            :category 'buffer
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :items    (lambda () (consult--buffer-query
                                  :predicate #'bufferlo-non-local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Non-local Bufferlo buffer candidate source for consult-buffer.")

    (add-to-list 'consult-buffer-sources 'krisb-bufferlo-consult--source-other-buffers)
    (add-to-list 'consult-buffer-sources 'krisb-bufferlo-consult--source-local-buffers)))

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
    ;; I don't find much use for `display-local-help'.  Additionally,
    ;; Emacs 31 has the new `eldoc-help-at-pt' option, which shows the
    ;; local help at point via eldoc
    ("\." . eldoc-doc-buffer))
  :custom
  (eldoc-idle-delay 0.2)
  (eldoc-documentation-strategy 'eldoc-documentation-enthusiast)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-help-at-pt t)                  ; Emacs 31.1
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'eldoc-mode))

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
;; TODO 2025-06-04: Revisit this.  This seems undesirable when using
;; built-in diffs via diff-mode.
(use-package abridge-diff
  :disabled t
  :ensure t
  :after diff
  :demand t
  :config
  (abridge-diff-mode 1)
  (add-to-list 'mode-line-collapse-minor-modes 'abridge-diff-mode))

;;;; Completion-preview
;; TODO 2025-05-30: Document:
;; - `completion-preview-ignore-case’ in conjunction with `completion-ignore-case'
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

;;;; Man
(use-package man
  :ensure nil
  :defer t
  :custom
  (Man-notify-method 'aggressive)) ; Instead of `display-buffer-alist', use this

;;;; Dired
;; Emacs' file manager
;; TODO 2025-05-22: Document:
;; - `dired-recursive-deletes'
(use-package dired
  :ensure nil
  :hook
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

;;;; Hide-mode-line
(use-package hide-mode-line
  :ensure t
  :bind
  ( :map krisb-toggle-keymap
    ("m" . hide-mode-line-mode)))

;;;; Info-colors
;; TODO 2025-05-26: Document that this is somewhat of a hacky solution
;; because it uses non-guaranteed regexps to find e.g. code blocks.
;; See `info-colors-fontify-node’.
;; Fontify useful parts of info buffers
(use-package info-colors
  :ensure t
  :hook
  (Info-selection-hook . info-colors-fontify-node)
  :config
  ;; TODO 2025-05-26: Place this in :custom-face?
  (set-face-attribute 'info-colors-ref-item-type nil :box t)
  ;; FIXME 2025-05-26: What if `org-inline-src-block’ isn’t defined
  ;; yet?
  (set-face-attribute 'info-colors-lisp-code-block nil :inherit '(org-inline-src-block fixed-pitch)))

;;;; Savefold
(use-package savefold
  :disabled t
  :ensure (:repo "https://github.com/jcfk/savefold.el.git")
  :demand t
  :custom
  ;; See `savefold--all-backends' for all possible values
  (savefold-backends '(outline
                       ;; org
                       hideshow
                       ;; treesit-fold
                       ))
  (savefold-directory (no-littering-expand-var-file-name "savefold"))
  :config
  (savefold-mode 1)

  ;; Hash file names.  Instead of using the absolute path of a file, turn that
  ;; absolute path into a hash.  This resolves the issue of file paths being
  ;; longer than what the OS permits.  See also `krisb-auto-save-hash-file-name'
  ;; and `krisb-backup-file-name-hash'.
  (el-patch-defun savefold-utils--get-attr-table-fpath (fpath)
    "Return the fpath of the attribute table file for FPATH.

This naively replaces path slashes with ! (/a/b/c -> !a!b!c) leading to a chance
of collision."
    (el-patch-remove
      (let* ((fpath (expand-file-name fpath))
             (fpath (string-replace "/" "!" fpath))
             (fpath (string-replace ":" "!" fpath))) ; For windows
        (expand-file-name fpath savefold-directory)))
    (el-patch-add
      (expand-file-name (sha1 (expand-file-name fpath)) savefold-directory))))

;;;; Calendar
;; TODO 2025-06-15: Document:
;; - `calendar-time-zone-style’
(use-package calendar
  :ensure nil
  :defer t
  :bind ( :map krisb-open-keymap
          ("c" . calendar))
  :custom
  (calendar-time-display-form
   '( 24-hours ":" minutes (when time-zone (format "(%s)" time-zone))))
  (calendar-week-start-day 1)           ; Monday

  ;; Diary
  (calendar-mark-diary-entries-flag t)

  ;; Holidays
  (calendar-mark-holidays-flag t)
  :config
  ;; Calendar
  (add-to-list 'display-buffer-alist
               '("\\*Calendar\\*"
                 (display-buffer-below-selected)
                 (window-height . fit-window-to-buffer))))

;;;; Miscellaneous additions to `mode-line-collapse-minor-modes'
(dolist (hook '(visual-line-mode
                buffer-face-mode))
  (add-to-list 'mode-line-collapse-minor-modes hook))

;;; Coding

;;;; Pcmpl-args
;; Extend the build in `pcomplete'.  Includes flag and argument completion in
;; the shell.
(use-package pcmpl-args
  :ensure t
  :after pcomplete
  :demand t)

;;;; Eshell
;; TODO 2025-05-24: Document:
;; - `eshell-glob-case-insensitive’
;; - `password-cache’
;; - `password-cache-expiry’
(use-package eshell
  :ensure nil
  :hook
  (eshell-mode-hook . krisb-eshell-setup)
  :bind
  ( :map krisb-open-keymap
    ("e" . eshell))
  :custom
  (eshell-banner-message "")
  (eshell-kill-processes-on-exit t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  :config
  ;; Set up `completion-at-point-functions'
  (defun krisb-eshell-setup ()
    "Buffer-local settings for eshell."
    (set-display-table-slot standard-display-table 0 ?\ )
    (setq-local scroll-margin 3
                line-spacing 0
                ;; `consult-outline' support for eshell prompts by
                ;; setting `outline-regexp’.  See
                ;; https://github.com/minad/consult/wiki#consult-outline-support-for-eshell-prompts
                outline-regexp eshell-prompt-regexp
                ;; `imenu’ support for eshell prompt history
                imenu-generic-expression `((nil ,eshell-prompt-regexp 0))))

  ;; Eshell source in `consult-buffer'
  (with-eval-after-load 'consult
    ;; For showing eshell sources in `consult-buffer'. Taken from
    ;; https://github.com/minad/consult#multiple-sources
    (defvar kb/consult-buffer--eshell-source
      (list :name     "Eshell Buffers"
            :category 'buffer
            :narrow   ?e
            :face     'consult-buffer
            :history  'buffer-name-history
            :annotate '(lambda (cand)
                         (substring-no-properties
                          (car (ring-elements
                                (buffer-local-value 'eshell-history-ring (get-buffer cand))))))
            :state    'consult--buffer-state
            :action   'display-buffer
            :items (lambda ()
                     (mapcar #'buffer-name
                             (seq-filter
                              (lambda (x)
                                (eq (buffer-local-value 'major-mode x) 'eshell-mode))
                              (buffer-list))))))
    (add-to-list 'consult-buffer-sources #'kb/consult-buffer--eshell-source 'append)))

(use-package em-hist
  :ensure nil
  :after eshell
  :demand t
  :custom
  (eshell-history-size 20000)
  (eshell-hist-ignoredups 'erase)       ; Only keep last duplicate
  (eshell-save-history-on-exit t)
  ;; Fix eshell overwriting history.  From
  ;; https://emacs.stackexchange.com/a/18569/15023.
  (eshell-save-history-on-exit nil))

;;;; Eshell-atuin
;; Use Atuin (https://github.com/atuinsh/atuin) with eshell
(use-package eshell-atuin
  :ensure t
  :after eshell
  :demand t
  :hook
  (eshell-post-command . eshell-atuin--update-cache)
  :bind*
  ( :map eshell-mode-map
    ([remap eshell-isearch-backward-regexp] . eshell-atuin-history)
    ([remap consult-history] . eshell-atuin-history))
  :custom
  (eshell-atuin-save-duration t)
  (eshell-atuin-filter-mode 'global)
  (eshell-atuin-search-options nil)
  (eshell-atuin-search-fields '(time command duration directory))
  (eshell-atuin-history-format "%-110c (in %i)")
  :config
  (eshell-atuin-mode 1)

  (defun eshell-atuin--update-cache ()
    "Ensure the eshell-atuin cache is up-to-date.
This function is intended to be used to prepare functions whose
candidates may depend on an updated eshell-atuin cache.  Users should be
careful not to call this function frequently in short periods of time
because updating the cache takes some a small amount of time."
    (when (derived-mode-p 'eshell-mode)
      ;; These two functions are called before the `completing-read' of
      ;; `eshell-atuin-history'
      (eshell-atuin--history-rotate-cache)
      (eshell-atuin--history-update)))
  (advice-add 'completion-at-point :before #'eshell-atuin--update-cache)
  (advice-add 'completion-preview-complete :before #'eshell-atuin--update-cache)

  ;; TODO 2025-05-27: Consider creating a PR to merge this upstream?
  ;; Show the filter mode in the `completing-read' prompt
  (el-patch-defun eshell-atuin-history (&optional arg)
    "Browse atuin history in Eshell.

`eshell-atuin-mode' enables storing eshell history in atuin in
addition to the built-in ring.  `eshell-atuin-history' opens
`completing-read' with the saved history, like the C-r shell binding
in the original tool.

ARG overrides the default filter mode (which is
`eshell-atuin-filter-mode').  The value is an index of
`eshell-atuin-filter-modes'.

By default, the completion UI shows only commands.  To change that,
add more fields to `eshell-atuin-search-fields' and use them in
`eshell-atuin-history-format'.  The default values are such for
backwards compatibility with \"non-vertical\" completion systems.

The completions are ordered; the first one is the most recent one.

Be sure to have the correct `eshell-prompt-regexp' set up!"
    (interactive "P")
    (let ((eshell-atuin-filter-mode
           (if arg
               (or (nth arg eshell-atuin-filter-modes)
                   (user-error "Invalid filter mode index: %s" arg))
             eshell-atuin-filter-mode)))
      (eshell-atuin--history-rotate-cache)
      (eshell-atuin--history-update))
    (let* ((commands (eshell-atuin--history-collection))
           (input (eshell-atuin--get-input))
           (completion-table (lambda (string pred action)
                               (if (eq action 'metadata)
                                   '(metadata (display-sort-function . identity)
                                              (cycle-sort-function . identity))
                                 (complete-with-action
                                  action commands string pred))))
           (el-patch-add
             (mode (when arg (nth arg eshell-atuin-filter-modes))))
           (el-patch-add
             (prompt
              (concat (capitalize (symbol-name (if mode
                                                   mode
                                                 eshell-atuin-filter-mode)))
                      " history: ")))
           (compl (completing-read (el-patch-swap "History: " prompt) completion-table nil nil input))
           (command
            (alist-get 'command
                       (gethash compl eshell-atuin--history-cache-format-index))))
      (eshell-bol)
      (delete-region (point) (line-end-position))
      (insert (or command compl))))

  ;; TODO 2025-05-08: Right now I've removed the function that used to use the
  ;; following function.  However, I keep it here just in case I decide to
  ;; create a PR/issue to upstream this missing functionlality (relative time).
  (defun krisb-eshell-atuin--relative-time (time-string)
    "Turn TIME-STRING into a relative time string.
TIME-STRING is a string that represents a time; it is in the format
returned by \"atuin history list\" CLI command.  For example:
\"2025-03-27 07:17:40\".

An example of a return value for this function is: \"9 minutes ago\"."
    ;; HACK 2025-03-27: We use `ignore-errors' to catch any malformed data
    ;; stored by upstream (which happened at least once for me...)
    (when-let* ((then-time (ignore-errors (date-to-time time-string)))
                (now-time (current-time))
                (diff-time (float-time (time-subtract then-time now-time)))
                (abs-diff (abs diff-time)))
      (cond ((< abs-diff 60)
             (format "%.0f seconds %s" abs-diff (if (< diff-time 0) "ago" "from now")))
            ((< abs-diff 3600)
             (format "%.0f minutes %s" (/ abs-diff 60) (if (< diff-time 0) "ago" "from now")))
            ((< abs-diff 86400)
             (format "%.0f hours %s" (/ abs-diff 3600) (if (< diff-time 0) "ago" "from now")))
            ((< abs-diff (* 30 86400))
             (format "%.0f days %s" (/ abs-diff 86400) (if (< diff-time 0) "ago" "from now")))
            (t (format "%.0f months %s" (/ abs-diff (* 30 86400)) (if (< diff-time 0) "ago" "from now")))))))

;;;; Eshell-syntax-highlighting
;; Zsh-esque syntax highlighting in eshell
(use-package eshell-syntax-highlighting
  :ensure t
  :after eshell
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode 1))

;;;; Remove all advice from a function
;; Thanks to
;; https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
(defun krisb-advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props)
                 (advice-remove sym advice))
               sym))

;;;; Eros
;; Overlay lisp evaluations into the current buffer (near cursor)
(use-package eros
  :ensure t
  :defer t
  :hook
  (emacs-lisp-mode-hook . eros-mode)
  :custom
  (eros-eval-result-prefix "⟹  "))

;;;; Inspector
;; Introspect list expressions.  This is similar in role to CEDET's
;; data-debug.el.  Also integrates with the debugging backtrace and edebug (see
;; https://github.com/mmontone/emacs-inspector?tab=readme-ov-file#from-the-emacs-debugger).
(use-package inspector
  :ensure t
  :defer t)

;;;; Enhancements to basic text editing
;; Open line indents too
(defun krisb-open-line (n)
  "Like `open-line’ but also indent.
For N, see the docstring of `open-line’."
  (interactive "*p")
  (open-line n)
  (save-excursion
    (forward-line n)
    (funcall indent-line-function)))
(bind-key [remap open-line] 'krisb-open-line)

;;;; Ediff
(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; Keep everything in the same frame
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-highlight-all-diffs nil)) ; Only highlight currently selected diff

;;;; Fancy-compilation
;; Make compilation outputs in compilation buffers more pleasant to
;; see.
(use-package fancy-compilation
  :ensure t
  :demand t
  :custom
  ;; The TERM environment variable to use (set to an empty string to
  ;; leave unset).  Set to \"ansi-term\" for the default of ansi-term
  (fancy-compilation-term "eterm-color")
  (fancy-compilation-override-colors nil)
  (fancy-compilation-quiet-prelude nil)
  (fancy-compilation-quiet-prolog nil)
  :config
  (fancy-compilation-mode 1))

;;;; Compile
;; TODO 2025-07-03: Document:
;; - `compilation-auto-jump-to-first-error'
(use-package compile
  :ensure nil
  :defer t
  :custom
  (compilation-scroll-output 'first-error))

;;;; Keychain-environment
(use-package keychain-environment
  ;; For AUR:
  ;; :ensure-system-package keychain
  :ensure t
  :defer t
  :hook
  (vc-before-checkin-hook
   . (lambda ()
       (unless (and (getenv "SSH_AUTH_SOCK")
                    (getenv "SSH_AGENT_PID"))
         (keychain-refresh-environment)))))

;;;; Lorem-ipsum
;; Insert sample text
(use-package lorem-ipsum
  :ensure t
  :defer t
  :config
  (setq-default lorem-ipsum-list-bullet "- "))

;;;; Comint
;; TODO 2025-06-30: Document:
;; - `comint-buffer-maximum-size'
;; Common library for REPLs in Emacs.
(use-package comint
  :ensure nil
  :custom
  (comint-terminfo-terminal "dumb-emacs-ansi")
  (comint-prompt-read-only t)
  (comint-completion-autolist t)
  (comint-scroll-to-bottom-on-input 'this)
  (comint-scroll-to-bottom-on-output 'this)
  (comint-input-autoexpand 'input)
  ;; 2025-06-30: Move this option to a more appropriate location?
  (ansi-color-for-comint-mode t))

;;; Writing

;;;; Org
(use-package org
  :ensure nil                           ; Activated by elpaca earlier
  :bind
  ("C-c s" . org-store-link)
  ;; :custom
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

;; Bespoke formatting in org buffers
(with-eval-after-load 'org
  ;; Like
  ;; https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#ensure-blank-lines-between-headings-and-before-contents
  ;; but using org-ml to modify parse trees and update regions
  ;; accordingly.  More performant and less prone to bugs (because we
  ;; rely on org's parser).
  ;;
  ;; With respect to performance, my version seems to be drastically
  ;; more performant when correcting large org buffers (dozens of
  ;; thousands of words with hundreds of headlines) but less
  ;; performant in these buffers when no corrections are
  ;; necessary--although, a singular invocation in a correct buffer is
  ;; fast enough for both my version and alphapapa's version that the
  ;; difference is unnoticed.
  (defun krisb-org-ensure-blank-lines (&optional arg)
    "Ensure blank lines surrounding headlines.
Ensure there is at least one blank line separating the contents of every
headline (after any drawers) and before every headline in the buffer.

When called with ARG, or the prefix-argument when called interactively,
widen the buffer first."
    (interactive "P" org-mode)
    (require 'org-ml)
    (let ((action
           (lambda ()
             ;; Our strategy is to iterate on each headline from the
             ;; end to beginning.  For each headline, update it
             ;; accordingly.  (We opt to update headlines individually
             ;; rather than the entire buffer at once with
             ;; `org-ml-update-this-buffer' because, as explained in
             ;; org-ml's README, such an operation will become
             ;; unbearably slow for very large buffers.  The sum of
             ;; all single headline updates in a buffer is much
             ;; faster.)
             (save-excursion
               (goto-char (point-min))
               (while (re-search-forward org-heading-regexp nil t)
                 ;; We use `org-ml-update-this-subtree' instead of
                 ;; `org-ml-update-this-headline' so subheadings are
                 ;; not removed upon updating.
                 (org-ml-update-this-subtree
                   (lambda (headline)
                     (cond
                      ;; Headline with non-empty section (has a
                      ;; property drawer or paragraphs or
                      ;; subheadlines)
                      ((org-ml-headline-get-section headline)
                       (let* ()
                         (--> headline
                              ;; Preceding empty line
                              (let ((supercontents (org-ml-headline-get-supercontents nil it)))
                                (org-ml-headline-set-supercontents
                                 nil
                                 (plist-put supercontents :blank (max 1 (plist-get supercontents :blank)))
                                 it))
                              ;; Proceeding empty line
                              (let* ((section (org-ml-headline-get-section it))
                                     (last-child (car (last section))))
                                (org-ml-headline-set-section
                                 (append (butlast section)
                                         (list (org-ml-set-property :post-blank (max 1 (org-ml-get-property :post-blank last-child))
                                                                    last-child)))
                                 it)))))
                      ;; Headline with subheadlines but an empty
                      ;; section (no content, no property drawers,
                      ;; etc.)
                      ((org-ml-headline-get-subheadlines headline)
                       (let ((supercontents (org-ml-headline-get-supercontents nil headline)))
                         (org-ml-headline-set-supercontents
                          nil
                          (plist-put supercontents :blank (max 1 (plist-get supercontents :blank)))
                          headline)))
                      ;; Entirely empty headline (no children
                      ;; elements, i.e., no property drawers, no
                      ;; content, no subheadlines, etc.)
                      (t
                       (org-ml-set-property :post-blank (max 1 (org-ml-get-property :post-blank headline))
                                            headline))))))))))
      (if arg
          (org-with-wide-buffer
           (funcall action))
        (funcall action)))))

;;;; Org-fold
(use-package org-fold
  :ensure nil
  :after org
  :bind
  ( :map org-mode-map
    ("C-c f" . org-fold-transient))
  :custom
  (org-fold-catch-invisible-edits 'show-and-error)
  :config
  (require 'transient)
  (transient-define-prefix org-fold-transient ()
    "Transient map for useful org-fold commands."
    ["Entries"
     [("s" "Show heading content" org-fold-show-entry)
      ("h" "Hide heading content" org-fold-hide-entry)]]
    ["Headlines"
     [("b" "Show branches" org-kill-note-or-show-branches)]]))

;;;; Visual-wrap
;; TODO 2025-05-27: Document history with `visual-wrap’
(use-package visual-wrap
  :ensure nil
  :hook
  (on-first-buffer-hook . global-visual-wrap-prefix-mode))

;;;; Org-archive
(use-package org-archive
  :ensure nil
  :defer t
  :config
  ;; Allow archiving according to ID of file
  (defun krisb-org-archive--compute-location-id-format-string (orig-fun &rest args)
    "Take LOCATION in `org-archive--compute-location' and expand %I.
%I is expanded to the value of the ID property of the heading or file
containing the heading at point.  If there is no node at point, then it
is expanded to the file path instead.

Meant to be used as around advice for `org-archive--compute-location'."
    ;; Modify LOCATION before normal operations
    (cl-letf (((car args)
               (replace-regexp-in-string
                    "%I"
                    (org-with-wide-buffer
                     (let (id)
                       (while (and (not id) (not (eq (point) (point-min))))
                         (if (< 1 (org-outline-level))
                             (org-up-heading 1 t)
                           (goto-char (point-min)))
                         (setq id (org-entry-get (point) "ID")))
                       (or id (buffer-file-name))))
                    (car args))))
      (apply orig-fun args)))
  (advice-add 'org-archive--compute-location :around #'krisb-org-archive--compute-location-id-format-string))

;;;; Saveplace-pdf-view
;; Save place in pdf-view buffers
(use-package saveplace-pdf-view
  :ensure t
  :after saveplace)

;;;; Org-appear
;; Show hidden characters (e.g. emphasis markers, link brackets) when
;; point is over enclosed content
(use-package org-appear
  :ensure t
  :defer t
  :hook
  (org-mode-hook . org-appear-mode)
  :custom
  (org-appear-delay 0.0)
  (org-appear-trigger 'always)
  (org-appear-autoemphasis t)
  (org-appear-autolinks 'just-brackets)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))

;;;; Markdown-mode
(use-package markdown-mode
  :ensure t
  :defer t
  :mode
  ("INSTALL\\'" "CONTRIBUTORS\\'" "LICENSE\\'" "README\\'" "\\.md\\'")
  :hook
  (markdown-mode-hook . visual-line-mode))

;;;; Harper language server
;; Support the Harper grammar checker (a language server).  See
;; https://writewithharper.com/docs/integrations/emacs for configuring
;; Harper via Eglot.  This includes configuring harper via
;; `eglot-workspace-configuration' and setting the "languaegID" based
;; on e.g. the major mode.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(org-mode . ("harper-ls" "--stdio")))
  (setq-default eglot-workspace-configuration
                `( :harper-ls ( :linters ( :SpellCheck :json-false
                                           :Spaces :json-false
                                           :LongSentences :json-false)
                                ;; We can use the dictionary enchant
                                ;; uses because they follow the same
                                ;; line-separated format.
                                ;; FIXME 2025-06-30: Is there a way to
                                ;; figure out the path of the enchant
                                ;; user dictionary when given a
                                ;; language via the CLI?
                                :userDictPath ,(expand-file-name "enchant/en_US.dic" (xdg-config-home))
                                :dialect "American"
                                :maxFileLength 120000)
                   ,@eglot-workspace-configuration)))

;;;; Indentinator
;; Automatic indentation of changed lines after idle time.
(use-package indentinator
  :ensure (:repo "https://github.com/xendk/indentinator.el.git")
  :hook ((emacs-lisp-mode-hook org-mode-hook) . indentinator-mode)
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'indentinator-mode))

;;; Reading

;;;; Pdf-tools
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
  :ensure nil                           ; We install via Guix
  :defer t
  ;; 2025-04-03: We manually add an auto-mode-alist entry to lazy-load this
  ;; package.  I want to avoid calling `pdf-loader-install' or
  ;; `pdf-tools-install' immediately at startup because I've had startup
  ;; complications when the pdf-tools install is malformed.  So I lazy load this
  ;; package and only call those functions after this package loads (i.e., in
  ;; the :config block).
  :mode
  ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :bind
  ( :map pdf-view-mode-map
    ("C-c C-r a" . pdf-view-auto-slice-minor-mode)
    ;; Additionally useful since it lets you scroll via
    ;; `scroll-other-window'
    ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
    ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page))
  :config
  ;; Must call `pdf-tools-install' or `pdf-loader-install' to have PDF
  ;; files use pdf-view-mode and have everything required loaded.  The
  ;; latter defers loading; see its docstring and
  ;; https://github.com/vedang/pdf-tools?tab=readme-ov-file#installing-pdf-tools-elisp-code
  (pdf-loader-install))

(use-package pdf-view
  :ensure nil
  :autoload krisb-pdf-view-cleanup-windows-h
  :defer t
  :hook
  (pdf-view-mode-hook . (lambda () (add-hook 'kill-buffer-hook #'krisb-pdf-view-cleanup-windows-h nil t)))
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-display-size 'fit-page)
  (pdf-view-continuous nil) ; REVIEW 2024-01-16: Change this when I get to use image-roll?
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  (pdf-view-use-unicode-ligther nil)
  :config
  ;; Taken from Doom
  (defun krisb-pdf-view-cleanup-windows-h ()
    "Kill left-over annotation buffers when the document is killed."
    ;; We add a guard here because sometimes things go wrong and this function
    ;; is called before `pdf-annot' is loaded, causing an error
    (when (featurep 'pdf-annot)
      (when (buffer-live-p pdf-annot-list-document-buffer)
        (pdf-info-close pdf-annot-list-document-buffer))
      (when (buffer-live-p pdf-annot-list-buffer)
        (kill-buffer pdf-annot-list-buffer))
      (let ((contents-buffer (get-buffer "*Contents*")))
        (when (and contents-buffer (buffer-live-p contents-buffer))
          (kill-buffer contents-buffer))))))

(use-package pdf-outline
  :ensure nil
  :defer t
  :custom
  (pdf-outline-enable-imenu t)
  (pdf-outline-display-labels t)
  (pdf-outline-imenu-use-flat-menus nil))

(use-package pdf-annot
  :ensure nil
  :defer t
  :hook
  (pdf-annot-list-mode-hook . (lambda () (hl-line-mode -1)))
  (pdf-annot-list-mode-hook . krisb-pdf-annot--setup-context-window-display-action)
  :custom
  (pdf-annot-color-history ; "Default" color list. Appears at the top of annotation color change commands
   '("yellow" "SteelBlue1" "SeaGreen3" "LightSalmon1" "MediumPurple1"))
  (pdf-annot-list-format '((page . 3)
                           (color . 8)
                           (text . 68)
                           (type . 10)))
  (pdf-annot-list-highlight-type nil)
  :config
  ;; Fit the "contents" window to buffer height
  (defun krisb-pdf-annot-list-context-function (id buffer)
    "Show the contents of an Annotation.

For an annotation identified by ID, belonging to PDF in BUFFER,
get the contents and display them on demand."
    (with-current-buffer (get-buffer-create "*Contents*")
      (set-window-buffer nil (current-buffer))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when id
          (save-excursion
            (insert
             (pdf-annot-print-annotation
              (pdf-annot-getannot id buffer)))))
        (read-only-mode 1))
      (fit-window-to-buffer)
      (visual-line-mode)))
  (advice-add 'pdf-annot-list-context-function :override #'krisb-pdf-annot-list-context-function)

  (defun krisb-pdf-annot--setup-context-window-display-action ()
    "Set the display action for the \"context buffer\".
The context buffer is the buffer that shows annotation contents in
`pdf-annot-mode'"
    (setq-local tablist-context-window-display-action
                '((display-buffer-reuse-window tablist-display-buffer-split-below-and-attach)
                  (window-height . 0.25)
                  (inhibit-same-window . t)
                  (window-parameters (no-other-window . t)
                                     (mode-line-format . none))))))

;;;; Pdf-meta-edit
(use-package pdf-meta-edit
  :ensure (:repo "https://github.com/krisbalintona/pdf-meta-edit.git")
  :defer t
  :bind
  ( :map pdf-view-mode-map
    ("C-c M" . pdf-meta-edit-modify)))

;;;; Org-keyterm-index
(use-package org-keyterm-index
  :ensure (:repo "https://github.com/krisbalintona/org-keyterm-index.git")
  :after org
  :defer t)

;;; Blogging
(require 'krisb-org-publish)

;;; Emails
;;;; Notmuch-bookmarks
(use-package notmuch-bookmarks
  :ensure t
  :after notmuch
  :demand t
  :config
  (notmuch-bookmarks-mode))

;;;; Notmuch-transient
(use-package notmuch-transient
  :disabled t                     ; 2025-09-07: Cool but not essential
  :ensure t
  :after notmuch
  :demand t
  :custom
  (notmuch-transient-prefix "C-d")
  (notmuch-transient-add-bindings t))

;;;; Shr
;; TODO 2025-06-11: Document these:
;; - `shr-cookie-policy’
;; - `shr-image-animate’
;; - `shr-width’
;; - `shr-use-fonts’
;; - `shr-fill-text’
;; Emacs' built-in web renderer
(use-package shr
  :ensure nil
  :defer t
  :custom
  (shr-use-colors nil)             ; I prefer to use my theme's colors
  (shr-discard-aria-hidden t)
  (shr-max-image-proportion 0.6))

;;; Task management
;;;; Org-agenda
(use-package org-agenda
  :custom
  (org-agenda-tags-column 0)
  ;; TODO 2025-05-24: Revisit these.
  ;; (org-agenda-format-date #'krisb-org-agenda-format-date-aligned)
  ;; (org-agenda-tags-todo-honor-ignore-options t)
  ;; (org-agenda-todo-ignore-scheduled nil)
  ;; (org-agenda-remove-times-when-in-prefix t)
  ;; (org-agenda-remove-tags 'prefix)
  
  ;; Tags
  ;; We set `org-tags-exclude-from-inheritance’ directory locally
  (org-tag-faces '(("project" . outline-1)))
  ;; TODO 2025-05-24: Revisit this.
  (org-fast-tag-selection-single-key 'expert))

;;;; Org-depend
;; Add blocking and triggering actions when an org-todo state is
;; changed.
(use-package org-depend
  :ensure nil
  :after org-contrib
  :demand t)

;;;; Org-super-agenda
(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :demand t
  :custom
  (org-super-agenda-hide-empty-groups t)
  (org-super-agenda-keep-order t)
  (org-agenda-cmp-user-defined #'krisb-org-sort-agenda-by-created-time)
  :config
  (org-super-agenda-mode 1)

  ;; Custom user-defined sorting (comparison) function for
  ;; `org-agenda-cmp-user-defined'
  (defun krisb-org-get-created-time (entry)
    "Return the CREATED time of ENTRY, or an empty string if it doesn't exist."
    (let ((marker (get-text-property 0 'marker entry)))
      (if marker
          (org-entry-get marker "CREATED")
        "")))

  (defun krisb-org-sort-agenda-by-created-time (a b)
    "Compare two agenda items, A and B, by their CREATED property."
    (let* ((time-a (krisb-org-get-created-time a))
           (time-b (krisb-org-get-created-time b)))
      (cond
       ((string= time-a "") +1)         ; A has no CREATED property, put it last
       ((string= time-b "") -1)         ; B has no CREATED property, put it last
       (t
        (if (time-less-p (date-to-time time-a) (date-to-time time-b))
            -1 +1)))))

  ;; NOTE 2025-03-14: I manually apply the changes described in the
  ;; un-pulled merge request:
  ;; https://github.com/alphapapa/org-super-agenda/pull/242.  I will
  ;; keep this until the problem it resolves is fixed in master.
  (defun krisb-org-super-agenda--sort-matches-for-original-order (matching)
    "Sort MATCHING items back into their original ordering based on `org-entries-lessp'.
Only used when `org-super-agenda-keep-order' is non-nil.

This is a helper function for my el-patch for
`org-super-agenda--group-items’."
    (--sort
     ;; Sorting has a shallow element of recursion because not all of the given items
     ;; are matched org headlines that can just be sorted using `org-entries-lessp'.
     ;; Some super-agenda matchers, like `:auto-category', will introduce sublists
     ;; whose contents need sorting of their own. In that case the lists' `:items'
     ;; properties need to be sorted instead.
     (let ((first-is-list (listp it))
           (second-is-list (listp other)))
       (when first-is-list
         (plist-put
          it :items
          (sort (plist-get it :items) #'org-entries-lessp)))
       (when second-is-list
         (plist-put
          other :items
          (sort (plist-get other :items) #'org-entries-lessp)))
       (cond
        (second-is-list t)
        (first-is-list nil)
        (t (org-entries-lessp it other))))
     matching))

  (el-patch-defun org-super-agenda--group-items (all-items)
    "Divide ALL-ITEMS into groups based on `org-super-agenda-groups'."
    (if (bound-and-true-p org-super-agenda-groups)
        ;; Transform groups
        (let ((org-super-agenda-groups (org-super-agenda--transform-groups org-super-agenda-groups)))
          ;; Collect and insert groups
          (cl-loop with section-name
                   for filter in org-super-agenda-groups
                   for custom-section-name = (plist-get filter :name)
                   for order = (or (plist-get filter :order) 0)  ; Lowest number first, 0 by default
                   for (auto-section-name non-matching matching) = (org-super-agenda--group-dispatch all-items filter)

                   do (when org-super-agenda-keep-order
                        (el-patch-swap
                          (setf matching (sort matching #'org-entries-lessp))
                          (setf matching (krisb-org-super-agenda--sort-matches-for-original-order matching))))

                   ;; Transformer
                   for transformer = (plist-get filter :transformer)
                   when transformer
                   do (setq matching (-map (pcase transformer
                                             (`(function ,transformer) transformer)
                                             ((pred symbolp) transformer)
                                             (_ `(lambda (it) ,transformer)))
                                           matching))

                   ;; Face
                   for face = (plist-get filter :face)
                   when face
                   do (let ((append (plist-get face :append)))
                        (when append (cl-remf face :append))
                        (--each matching
                          (add-face-text-property 0 (length it) face append it)))

                   ;; Auto category/group
                   if (cl-member auto-section-name org-super-agenda-auto-selector-keywords)
                   do (setq section-name (or custom-section-name "Auto category/group"))
                   and append (cl-loop for group in matching
                                       collect (list :name (plist-get group :name)
                                                     :items (plist-get group :items)
                                                     :order order))
                   into sections
                   and do (setq all-items non-matching)

                   ;; Manual groups
                   else
                   do (setq section-name (or custom-section-name auto-section-name))
                   and collect (list :name section-name :items matching :order order) into sections
                   and do (setq all-items non-matching)

                   ;; Sort sections by :order then :name
                   finally do (setq non-matching (list :name org-super-agenda-unmatched-name
                                                       :items non-matching
                                                       :order org-super-agenda-unmatched-order))
                   finally do (setq sections (--sort (let ((o-it (plist-get it :order))
                                                           (o-other (plist-get other :order)))
                                                       (cond ((and
                                                               ;; FIXME: This is now quite ugly.  I'm not sure that all of these tests
                                                               ;; are necessary, but at the moment it works, so I'm leaving it alone.
                                                               (equal o-it o-other)
                                                               (not (equal o-it 0))
                                                               (stringp (plist-get it :name))
                                                               (stringp (plist-get other :name)))
                                                              ;; Sort by string only for items with a set order
                                                              (string< (plist-get it :name)
                                                                       (plist-get other :name)))
                                                             ((and (numberp o-it)
                                                                   (numberp o-other))
                                                              (< o-it o-other))
                                                             (t nil)))
                                                     (push non-matching sections)))
                   ;; Insert sections
                   finally return (cl-loop for (_ name _ items) in sections
                                           when items
                                           collect (org-super-agenda--make-agenda-header name)
                                           and append items)))
      ;; No super-filters; return list unmodified
      all-items)))

;;;; `org-agenda-custom-commands'
(with-eval-after-load 'org-agenda
  ;; Relevant variables to set locally in `org-agenda-custom-commands'
  ;; - `org-agenda-overriding-header'
  ;; - `org-agenda-show-inherited-tags'
  ;; - `org-agenda-sorting-strategy'
  ;; - `org-agenda-start-day'
  ;; - `org-agenda-span'
  ;; - `org-agenda-prefix-format'
  ;; - `org-agenda-scheduled-leaders'
  ;; - `org-agenda-deadline-leaders'
  ;; - `org-agenda-skip-deadline-prewarning-if-scheduled'
  ;; - `org-agenda-skip-scheduled-if-deadline-is-shown'
  ;; - `org-habit-show-all-today'
  ;; - `org-habit-show-habits-only-for-today'
  ;; - `org-agenda-dim-blocked-tasks'
  ;; - `org-agenda-include-diary'
  ;; - `org-agenda-insert-diary-extract-time'
  ;; - `org-agenda-skip-function'
  ;; - `org-agenda-entry-types'
  ;; - `org-deadline-warning-days'
  ;; - `org-scheduled-delay-days'
  (setopt org-agenda-custom-commands
          '(("f" "FYP"
             ((agenda ""
                      ((org-agenda-overriding-header "Time-bound tasks")
                       (org-agenda-show-inherited-tags t)
                       (org-agenda-start-day "+0d")
                       (org-agenda-span 'day)
                       (org-habit-show-habits-only-for-today t)
                       (org-agenda-dim-blocked-tasks t)
                       (org-agenda-include-diary t)
                       (org-agenda-insert-diary-extract-time t)
                       (org-super-agenda-groups
                        '((:name "Overdue" :scheduled past :deadline past)
                          (:auto-category t)))))
              (tags-todo "-inbox"
                         ((org-agenda-overriding-header "Projects and tasks to review")
                          (org-agenda-use-tag-inheritance '(todo))
                          (org-agenda-show-inherited-tags t)
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-skip-function 'org-review-agenda-skip)
                          (org-agenda-cmp-user-defined 'org-review-compare)
                          (org-agenda-sorting-strategy '(user-defined-down))))
              (tags-todo "-project-inbox"
                         ((org-agenda-overriding-header "Non-time-bound tasks")
                          (org-agenda-use-tag-inheritance '(todo))
                          (org-agenda-show-inherited-tags t)
                          (org-agenda-dim-blocked-tasks 'invisible)
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-entry-if 'scheduled)
                                  ;; Rather than something like:
                                  ;;   (not (org-review-agenda-skip))
                                  ;; We manually invert the definition of
                                  ;; `org-review-agenda-skip' because skipping
                                  ;; functions, if successful (i.e. reporting a
                                  ;; skip), must return the point which
                                  ;; org-agenda should continue from.
                                  (and (org-review-toreview-p)
                                       (org-with-wide-buffer (or (outline-next-heading) (point-max)))))))))))
            ("i" "Inbox: process entries"
             ((tags-todo "+inbox"
                         ((org-agenda-overriding-header "Review")
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-skip-function 'org-review-agenda-skip)
                          (org-agenda-cmp-user-defined 'org-review-compare)
                          (org-agenda-sorting-strategy '(user-defined-down))))
              (tags-todo "+inbox"
                         ((org-agenda-overriding-header "Non-review inbox")
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-skip-function
                           'krisb-org-review-has-review-property-p)))
              (todo "HOLD"
                    ((org-agenda-overriding-header "Non-review holds")
                     (org-agenda-skip-function
                      'krisb-org-review-has-review-property-p)))
              (todo "MAYBE"
                    ((org-agenda-overriding-header "Non-review maybes")
                     (org-agenda-skip-function
                      'krisb-org-review-has-review-property-p)))))
            ;; FIXME 2025-05-24: Revisit this.
            ;; ("p" "Projects"
            ;;  ((tags-todo "project"
            ;;              ((org-agenda-overriding-header "")
            ;;               ;; This lets project sub-tasks be discoverable by a tags
            ;;               ;; search. One might think :auto-parent makes this
            ;;               ;; redundant, but this handles cases where I have a
            ;;               ;; sub-task but its parent is not a project -- I do this
            ;;               ;; sometimes for simple dependencies between todos
            ;;               ;; FIXME 2024-10-07: This shows the project tag for all the
            ;;               ;; sub-tasks, which can be visually noisy. I'm not sure if
            ;;               ;; there is a workaround
            ;;               (org-tags-exclude-from-inheritance
            ;;                (remove "project" org-tags-exclude-from-inheritance))
            ;;               (org-agenda-prefix-format
            ;;                ;; FIXME 2024-10-07: Not sure if this is a tags- or
            ;;                ;; todo-type view
            ;;                '((tags  . " %i %-8:c%-5e%?-12t% s")))
            ;;               (org-super-agenda-groups
            ;;                '(( :auto-parent t
            ;;                    :order 2)
            ;;                  ( :name "All projects"
            ;;                    :anything t
            ;;                    :order 1)))))))
            )))

;;;; Hammy
(use-package hammy
  :ensure t
  :bind
  ( :map krisb-open-keymap
    ("h S" . hammy-start)
    ("h n" . hammy-next)
    ("h s" . hammy-stop)
    ("h r" . hammy-reset)
    ("h t" . hammy-toggle)
    ("h a" . hammy-adjust)
    ("h v" . hammy-view-log)
    ("h R" . hammy-status)
    ("h I" . hammy-start-org-clock-in)
    ;; Bespoke commands
    ("h h" . krisb-hammy-dwim)
    ("h d" . krisb-hammy-modify-duration)
    ("h e" . krisb-hammy-modify-elapsed))
  :custom
  ;; TODO 2025-05-27: Revisit this.
  ;; ;; TODO 2024-09-25: Have this found more locally.  When I do, also change
  ;; ;; `tmr-sound' to this file
  ;; (hammy-sound-end-work "/home/krisbalintona/.emacs.d/elpa/work-timer/simple-notification.mp3")
  ;; (hammy-sound-end-break "/home/krisbalintona/.emacs.d/elpa/work-timer/simple-notification.mp3")

  ;; Mode line
  (hammy-mode-always-show-lighter nil)
  (hammy-mode-update-mode-line-continuously t)
  (hammy-mode-lighter-seconds-format "%.2m:%.2s")
  (hammy-mode-lighter-prefix "[H]")
  (hammy-mode-lighter-overdue "!")
  (hammy-mode-lighter-pie t)
  (hammy-mode-lighter-pie-height 0.65)
  :config
  ;; Mode line
  (hammy-mode 1)

  ;; Override `hammy-start-org-clock-in' to work in org-agenda
  (el-patch-defun hammy-start-org-clock-in (&rest _ignore)
    "Call `org-clock-in' and start a hammy (or use an already-started one).
If point is in an Org entry, clock into it; otherwise, offer a
list of recently clocked tasks to clock into.  The Org task will
then automatically be clocked out during the hammy's second
interval (and when the hammy is stopped), and back in when the
first interval resumes.  (If the user clocks into a different
task while the hammy is running, the task that is clocked-in when
the work interval ends will be clocked back into when the next
work interval begins.)

Returns the hammy from `hammy-start'.  Assumes that the hammy's
first interval is the work interval (i.e. the one during which
the task should be clocked in)."
    (interactive)
    (require 'org)
    ;; MAYBE: Take a point-or-marker argument for the task to clock into.
    (el-patch-swap
      (if (and (eq major-mode 'org-mode)
               (not (org-before-first-heading-p)))
          ;; At an Org entry: clock in to heading at point.
          (org-clock-in)
        ;; Not in an Org entry: offer a list to choose from.
        (org-clock-in '(4)))
      (cond
       ;; At an Org entry: clock in to heading at point.
       ((and (eq major-mode 'org-mode)
             (not (org-before-first-heading-p)))
        (org-clock-in))
       ;; At an Org-agenda entry: clock in to entry at point.
       ((eq major-mode 'org-agenda-mode)
        (org-agenda-clock-in))
       ;; Not in an Org entry: offer a list to choose from.
       (t (org-clock-in '(4)))))
    (let ((hammy (hammy-complete "Clock in with Hammy: " hammy-hammys)))
      (unless (hammy-interval hammy)
        (hammy-start hammy))
      (cl-macrolet ((pushfn (fn place)
                      `(cl-pushnew ,fn ,place :test #'equal)))
        (pushfn #'hammy--org-clock-in (hammy-interval-before (hammy-interval hammy)))
        (pushfn #'hammy--org-clock-out (hammy-interval-after (hammy-interval hammy)))
        (pushfn #'hammy--org-clock-out (hammy-stopped hammy)))
      hammy))

  ;; Custom lighter
  (defun krisb-hammy-mode-lighter ()
    "Return lighter for `hammy-mode'."
    (cl-labels
        ((format-hammy (hammy)
           (let ((remaining
                  (abs
                   ;; We use the absolute value because `ts-human-format-duration'
                   ;; returns 0 for negative numbers.
                   (- (hammy-current-duration hammy)
                      (float-time (time-subtract (current-time)
                                                 (hammy-current-interval-start-time hammy)))))))
             (format "%s(%s%s:%s)"
                     (propertize (hammy-name hammy)
                                 'face 'hammy-mode-lighter-name)
                     (if (hammy-overduep hammy)
                         (propertize hammy-mode-lighter-overdue
                                     'face 'hammy-mode-lighter-overdue)
                       "")
                     (propertize (hammy-interval-name (hammy-interval hammy))
                                 'face `(hammy-mode-lighter-interval
                                         ,(hammy-interval-face (hammy-interval hammy))))
                     (concat (when hammy-mode-lighter-pie
                               (propertize " " 'display (hammy--pie hammy)))
                             (if (hammy-overduep hammy)
                                 ;; We use the negative sign when counting down to
                                 ;; the end of an interval (i.e. "T-minus...") .
                                 "+" "-")
                             (format-seconds (if (< remaining 60)
                                                 "%2ss" hammy-mode-lighter-seconds-format)
                                             remaining))))))
      (if hammy-active
          (concat (mapconcat #'format-hammy hammy-active ",") " ")
        ;; No active hammys.
        (when hammy-mode-always-show-lighter
          (concat (propertize hammy-mode-lighter-prefix
                              'face 'hammy-mode-lighter-prefix-inactive)
                  (if hammy-mode-lighter-suffix-inactive
                      (concat ":" hammy-mode-lighter-suffix-inactive))
                  " ")))))
  (advice-add 'hammy-mode-lighter :override #'krisb-hammy-mode-lighter)

  ;; Dwim command
  (defun krisb-hammy-dwim ()
    "DWIM with hammy."
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (call-interactively 'hammy-start-org-clock-in)
      (if hammy-active
          (call-interactively 'hammy-next)
        (call-interactively 'hammy-start))))

  ;; Hammy definitions
  (defun krisb-hammy-play-sound ()
    "Play end of timer sound."
    (interactive)
    (call-process-shell-command
     (format "ffplay -nodisp -autoexit %s >/dev/null 2>&1" hammy-sound-end-work) nil 0))

  (setq hammy-hammys nil)
  (hammy-define "Fractional"
                :documentation "Breaks that are ⅓ as long as the last work interval."
                :intervals
                (list
                 (interval :name "Work"
                           :duration "40 minutes"
                           :before (do (announce "Starting work time (advance to break when ready)."))
                           :after (do (krisb-hammy-play-sound))
                           :advance t
                           ;; (do (krisb-hammy-play-sound)
                           ;;     (let* ((current-duration
                           ;;             (ts-human-format-duration
                           ;;              (float-time
                           ;;               (time-subtract (current-time)
                           ;;                              current-interval-start-time))))
                           ;;            (message (format "You've worked for %s!" current-duration)))
                           ;;       (announce message)
                           ;;       (notify message)))
                           )
                 (interval :name "Break"
                           :duration (do (cl-assert (equal "Work" (hammy-interval-name (caar history))))
                                         (let ((duration (cl-loop for (interval start end) in history
                                                                  while (equal "Work" (hammy-interval-name interval))
                                                                  sum (float-time (time-subtract end start))
                                                                  into work-seconds
                                                                  finally return (* work-seconds 0.33))))
                                           (when (alist-get 'unused-break etc)
                                             (cl-incf duration (alist-get 'unused-break etc))
                                             (setf (alist-get 'unused-break etc) nil))
                                           duration))
                           :before (do (let ((message (format "Starting break for %s."
                                                              (ts-human-format-duration current-duration))))
                                         (announce message)))
                           :after (do (krisb-hammy-play-sound)
                                      (let* ((elapsed
                                              (float-time
                                               (time-subtract (current-time) current-interval-start-time)))
                                             (unused (- current-duration elapsed)))
                                        (when (> unused 0)
                                          (if (alist-get 'unused-break etc)
                                              (cl-incf (alist-get 'unused-break etc) unused)
                                            (setf (alist-get 'unused-break etc) unused)))))
                           :advance t
                           ;; (remind "5 minutes"
                           ;;         (do (krisb-hammy-play-sound)))
                           )))
  (hammy-define "Ramp and decline"
                :documentation "Get your momentum going!"
                :intervals (list (interval :name "Work"
                                           :face 'font-lock-builtin-face
                                           :duration (climb "5 minutes" "40 minutes"
                                                            :descend t :step "5 minutes")
                                           :before (do (announce "Work time!"))
                                           :advance (do (announce "Work time is over!")
                                                        (notify "Work time is over!")
                                                        (remind "5 minutes"
                                                                (do (krisb-hammy-play-sound)))))
                                 (interval :name "Rest"
                                           :face 'font-lock-type-face
                                           :duration (do (let ((duration (cl-loop for (interval start end) in history
                                                                                  while (equal "Work" (hammy-interval-name interval))
                                                                                  sum (float-time (time-subtract end start))
                                                                                  into work-seconds
                                                                                  finally return (max (* 60 2) (* work-seconds 0.33)))))
                                                           (when (alist-get 'unused-break etc)
                                                             (cl-incf duration (alist-get 'unused-break etc))
                                                             (setf (alist-get 'unused-break etc) nil))
                                                           duration))
                                           :before (do (announce "Rest time!"))
                                           :after (do (let* ((elapsed
                                                              (float-time
                                                               (time-subtract (current-time) current-interval-start-time)))
                                                             (unused (- current-duration elapsed)))
                                                        (when (> unused 0)
                                                          (if (alist-get 'unused-break etc)
                                                              (cl-incf (alist-get 'unused-break etc) unused)
                                                            (setf (alist-get 'unused-break etc) unused)))))
                                           :advance (remind "5 minutes"
                                                            (do (announce "Rest time is over!")
                                                                (notify "Rest time is over!")
                                                                (krisb-hammy-play-sound)))))
                :complete-p (do (and (> cycles 1)
                                     interval
                                     (equal "Work" interval-name)
                                     (>= (duration "5 minutes") current-duration)))
                :after (do (announce "Flywheel session complete!")
                           (notify "Flywheel session complete!")))

  (hammy-define (propertize "🍅" 'face '(:foreground "tomato"))
                :documentation "The classic pomodoro timer."
                :intervals
                (list
                 (interval :name "Working"
                           :duration "25 minutes"
                           :before (do (announce "Starting work time.")
                                       (notify "Starting work time."))
                           :advance (remind "10 minutes"
                                            (do (announce "Break time!")
                                                (notify "Break time!"))))
                 (interval :name "Resting"
                           :duration (do (if (and (not (zerop cycles))
                                                  (zerop (mod cycles 3)))
                                             ;; If a multiple of three cycles have
                                             ;; elapsed, the fourth work period was
                                             ;; just completed, so take a longer break.
                                             "30 minutes"
                                           "5 minutes"))
                           :before (do (announce "Starting break time.")
                                       (notify "Starting break time."))
                           :advance (remind "10 minutes"
                                            (do (announce "Break time is over!")
                                                (notify "Break time is over!"))))))

  (hammy-define "1-shot"
                :documentation "Single-use timer that prompts for name and duration."
                :complete-p (do (> cycles 0))
                :before
                (lambda (hammy)
                  (hammy-reset hammy)
                  (setf (hammy-intervals hammy)
                        (ring-convert-sequence-to-ring
                         (list (interval
                                :name (read-string "Interval name (optional): " nil nil "")
                                :duration (read-string "Duration: ")
                                :advance (remind "5 minutes"
                                                 (do (let ((message (format "%s is over!" interval-name)))
                                                       (krisb-hammy-play-sound)
                                                       (notify message))))))))))

  ;; Bespoke commands
  (defun krisb-hammy-modify-duration (hammy)
    "Modify the duration of HAMMY timer.
Interactively, prompt for a currently active hammy.

Like `hammy-adjust', also sets the \"original-durations\" variable
(which contains hammy-intervals) stored in the etc slot of HAMMY if it
is not already set.

See `timer-duration-words' for the units available when prompted for a
duration."
    (interactive (list (hammy-complete "Select which hammy's current duration to modify:" hammy-active)))
    (cl-symbol-macrolet
        ((original-interval-duration
           (alist-get (car (member (hammy-interval hammy)
                                   (ring-elements (hammy-intervals hammy))))
                      (alist-get 'original-durations (hammy-etc hammy)))))
      (let* ((input-duration
              (read-string "Duration (as number or string): "
                           nil nil (prin1-to-string (hammy-interval-duration (hammy-interval hammy)))))
             (new-duration (pcase-exhaustive input-duration
                             ((and (pred numberp) it) it)
                             ((and (pred stringp) it) (timer-duration it)))))
        (setf (hammy-current-duration hammy) new-duration)
        ;; Only save the original duration the first time the interval is
        ;; adjusted, like `hammy-adjust'
        (unless original-interval-duration
          (setf original-interval-duration new-duration)))))

  (defun krisb-hammy-modify-elapsed (hammy)
    "Modify the elapsed time of HAMMY timer.
This command opts to add (a positive or negative) offset to modify the
start time of the hammy (current-interval-start-time slot of the
hammy-interval slot of the hammy).  This means moving the start time
backward to increase the elapsed time and forward to decrease the
elapsed time of HAMMY.

The original value of current-interval-start-time is stored in the
original-interval-start-time cons in the etc slot of the
hammy-interval.

Interactively, prompt for a currently active hammy.

See `timer-duration-words' for the units available when prompted for a
duration."
    (interactive (list (hammy-complete "Select which hammy's current elapsed time to modify:" hammy-active)))
    (let* ((input-duration
            (read-string "Duration (as number or string): "
                         nil nil (prin1-to-string (hammy-interval-duration (hammy-interval hammy)))))
           (offset (pcase-exhaustive input-duration
                     ((and (pred numberp) it) it)
                     ;; TODO 2025-03-19: Figure out a more elegant solution to
                     ;; negative durations.  Currently, since `timer-duration'
                     ;; always returns positive numbers, even with a prefixing
                     ;; "-", we manually negate the number.
                     ((and (pred stringp) it) (let ((dur (timer-duration it)))
                                                (if (string-prefix-p "-" input-duration)
                                                    (- dur) dur)))))
           (new-start-time (time-subtract (hammy-current-interval-start-time hammy)
                                          (time-convert offset 'list))))
      (setf (hammy-current-interval-start-time hammy) new-start-time))))

;;; Guix-management

;;;; Guix
(use-package guix
  :ensure nil      ; Although available via MELPA, we install via Guix
  :defer t
  :hook
  (guix-build-log-mode-hook . guix-build-log-phase-toggle-all))

;;;; Geiser-guile
(use-package geiser-guile
  :ensure nil
  :defer t
  :custom
  ;; TODO 2025-06-10: Avoid hard-coding this directory?
  (geiser-guile-load-path (list "/home/krisbalintona/guix-config/")))

;;;; Sops
;; Edit SOPS files inside Emacs.
;; TODO 2025-06-16: Document the following information.  `sops-mode’
;; and `global-sops-mode' only conditionally keep themselves enabled
;; in files encrypted with SOPS.  If it is, we can use the available
;; commands to edit the file.
(use-package sops
  :ensure t
  :defer t
  :hook
  (on-first-file-hook . global-sops-mode)
  :bind
  (("C-c e C-c" . sops-save-file)
   ("C-c e C-k" . sops-cancel)
   ("C-c e C-d" . sops-edit-file)))

;;; Package authorship

;;;; Package-lint-flymake
(use-package package-lint-flymake
  :ensure t
  :defer t
  :hook
  (emacs-lisp-mode-hook . package-lint-flymake-setup))

;;;; Org-make-toc
(use-package org-make-toc
  :ensure t
  :defer t
  :custom
  (org-make-toc-insert-custom-ids t))

;;; Uncategorized

;; Restart or close Emacs
(defun krisb-restart-or-kill-emacs (&optional arg restart)
  "Kill Emacs.
If called with RESTART (`universal-argument’ interactively) restart
Emacs instead.  Passes ARG to `save-buffers-kill-emacs'."
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

;; `indent-for-tab-command' functionality: what happens when you press
;; TAB?
(setopt tab-always-indent 'complete
        tab-first-completion 'word)

;; Keep the cursor out of the read-only portions of the minibuffer
(setopt minibuffer-prompt-properties
        '( read-only t
           cursor-intangible t
           face minibuffer-prompt))

;; Recenter upon `next-error'
(setopt next-error-recenter '(4))

;; Avoid collision of mouse with point
(mouse-avoidance-mode 'jump)

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

;; Prefer UTF-8 file and selection encoding
(prefer-coding-system 'utf-8)
;; The clipboard on Windows is often a wider encoding (UTF-16), so
;; leave Emacs to its own devices there.  Otherwise, encode text into
;; the clipboard into UTF-8
(unless (eq system-type 'windows-nt)
  (setopt selection-coding-system 'utf-8))

;; Prefer unicode charset
(set-charset-priority 'unicode)

;; Don't prompt user to confirm killing running sub-processes when
;; quitting Emacs
(setopt confirm-kill-processes nil)
