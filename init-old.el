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
;; Until the vtable work I’ve been testing is upstreamed, we manually
;; load that file for now.  NOTE: I have not figured out a way to
;; replace the built-in vtable Info manual with this one.
(elpaca '(vtable :wait t
                 :repo "https://github.com/krisbalintona/emacs.git"
                 :branch "vtable-ship-mints"
                 :files ("lisp/emacs-lisp/vtable.el")))
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

;;;;; Keymaps
;; TODO 2025-05-22: Revisit this.
;; (defvar-keymap krisb-file-keymap
;;   :doc "Prefix for file-related commands.")
;; (bind-key "C-c f" krisb-file-keymap 'global-map)

;; TODO 2025-05-22: Revisit this.
;; (defvar-keymap krisb-yank-keymap
;;   :doc "Prefix for yanking stuff.")
;; (bind-key "C-c i" krisb-yank-keymap 'global-map)

(defvar-keymap krisb-toggle-keymap
  :doc "Prefix for toggling stuff.")
(bind-key "C-c t" krisb-toggle-keymap 'global-map)

;;;;; Directories
(defvar krisb-blog-manuscripts-directory (expand-file-name "manuscripts/blog" krisb-notes-directory)
  "The directory for my pre-export blog files.")

(defvar krisb-email-directory (expand-file-name "emails/" "~/Documents/")
  "Directory that houses my local email files.")

(defvar krisb-email-drafts-directory (expand-file-name "drafts" krisb-email-directory)
  "Directory that houses my local email drafts.")

;;;;; Files
(defvar krisb-org-agenda-main-file (expand-file-name "todo.org" krisb-org-agenda-directory)
  "My main org-agenda file.")

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

;;;; Don’t let GTK override key sequences on wayland
;; See the section titled “Certain keys such as 'C-S-u' are not
;; reported correctly.” in etc/PROBLEMS (M-x C-h C-p).
(when (fboundp 'pgtk-use-im-context)
  ;; 2025-06-03: In my experience, this function should only be called
  ;; after the frame has been set up.
  (add-hook 'window-setup-hook (lambda () (pgtk-use-im-context nil))))

;;; Three steps below
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
    ("s" . eat-project)                 ; Overshadow `project-shell’
    ;; Unbind M-<NUMBER> keybinds because I use them for switching
    ;; betweeb tab-bar tabs
    :map eat-semi-char-mode-map
    ("M-1" . nil)
    ("M-2" . nil)
    ("M-3" . nil)
    ("M-4" . nil)
    ("M-5" . nil)
    ("M-6" . nil)
    ("M-7" . nil)
    ("M-8" . nil)
    ("M-9" . nil)
    ("M-0" . nil))
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
                          :family (fontaine--get-preset-property fontaine-current-preset :term-family))))

  ;; Replace `project-shell’ with `eat-project’ in `project-switch-commands’
  (with-eval-after-load 'project
    (cl-nsubstitute '(eat-project "EAT") '(project-shell "Shell")
                    project-switch-commands :test #'equal)))

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
     (project-switch-to-buffer "Switch to buffer")
     (project-vc-dir "VC-Dir")
     (project-eshell "Eshell")
     (project-shell "Shell")
     (project-compile "Compile")
     (project-recompile "Recompile")
     (project-any-command "Other")))
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-mode-line t)
  (project-mode-line-face 'italic)
  :config
  ;; TODO 2025-05-22: Revisit this
  ;; ;; On startup, remove non-existent directories from remembered projects list
  ;; (project-forget-zombie-projects)
  )

;;;; Info
;; TODO 2025-06-16: Document:
;; - `Info-hide-note-references’
;; More easily distinguish between paragraphs and function signatures
;; in Info documentation manuals.
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
    (face-remap-set-base 'default `(:height 1.2)))

  ;; TODO 2025-05-26: Place this in :custom-face?
  ;; Increase font size of title faces
  (set-face-attribute 'info-title-1 nil :height 1.4)
  (set-face-attribute 'info-title-2 nil :height 1.3)
  (set-face-attribute 'info-title-3 nil :height 1.2)
  (set-face-attribute 'info-title-4 nil :height 1.1))

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
  (cape-dabbrev-buffer-function 'cape-text-buffers)
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

  ;; Additions to `display-buffer-alist’
  (add-to-list 'display-buffer-alist
               `((or (major-mode . xref--xref-buffer-mode)
                     (,(rx (literal xref-buffer-name))))
                 (display-buffer-below-selected display-buffer-at-bottom)
                 (window-height . 0.25)))
  (add-to-list 'display-buffer-alist
               '(((category . xref)
                  (display-buffer-reuse-window display-buffer-use-some-window)
                  (some-window . mru))))

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

;;;; Flymake
;; TODO 2025-05-24: Document:
;; - `elisp-flymake-byte-compile-load-path’
;; - `flymake-suppress-zero-counters’
(use-package flymake
  :ensure nil
  :hook
  ;; TODO 2025-05-24: Revisit this.
  ;; (prog-mode-hook . (lambda ()
  ;;                     (setq-local flymake-indicator-type nil
  ;;                                 flymake-show-diagnostics-at-end-of-line 'fancy) ; Emacs 31 value
  ;;                     (flymake-mode 1)))
  (text-mode-hook . (lambda ()
                      (setq-local flymake-indicator-type nil)
                      (flymake-mode 1)))
  :custom
  (flymake-wrap-around nil)
  (flymake-mode-line-format
   '(" " flymake-mode-line-title flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format
   '("["
     flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter
     "]"))

  ;; Indicators
  (flymake-indicator-type nil)
  (flymake-fringe-indicator-position nil) ; Position for fringe position type
  (flymake-margin-indicator-position 'right-margin) ; Position for margin position type
  (flymake-show-diagnostics-at-end-of-line nil)
  :config
  (setq flymake-mode-line-counters
        '(:eval (if (mode-line-window-selected-p)
                    (flymake--mode-line-counters)
                  (propertize (format-mode-line (flymake--mode-line-counters))
                              'face '(:inherit (bold mode-line-inactive)))))))

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

;;;; Backups
;; Backup files. "Emacs makes a backup for a file only the first time
;; the file is saved from the buffer that visits it."
;; TODO 2025-05-22: Document:
;; - `make-backup-files’
;; - `dired-kept-versions’
;; - `kept-old-versions’
;; - `dired-backup-overwrite'
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

;;;; Hide-mode-line
(use-package hide-mode-line
  :ensure t
  :bind
  ( :map krisb-toggle-keymap
    ("m" . hide-mode-line-mode)))

;;;; Lin
;; Lin is a stylistic enhancement for Emacs' built-in
;; `hl-line-mode'. It remaps the `hl-line' face (or equivalent)
;; buffer-locally to a style that is optimal for major modes where
;; line selection is the primary mode of interaction.
(use-package lin
  :ensure t
  :demand t
  :custom
  (lin-face 'lin-cyan)
  :config
  (lin-global-mode 1)

  (add-to-list 'lin-mode-hooks 'LaTeX-mode-hook))

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

;;;; Apropos
(use-package apropos
  :ensure nil
  :defer t
  :bind
  ("C-h u" . apropos-user-option))

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

;;;; Orgmdb
;; Use the OMdb API to populate org headings with IMDB information on
;; movies, shows, and episodes.
(use-package orgmdb
  :ensure t
  :custom
  ;; FIXME 2025-10-06: Should I obfuscate this?  There's a limit of
  ;; 1000 requests per day.
  (orgmdb-omdb-apikey "8ab4d64e")
  (orgmdb-show-tag "show")
  (orgmdb-type-prop "CATEGORY")
  (orgmdb-fill-property-list
   '( imdb-link
      genre director writer actors box-office
      tomatometer metascore metacritic imdb-rating)))

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

;;;; Highlight-function-calls
(use-package highlight-function-calls
  :ensure t
  :defer t
  :hook
  ((emacs-lisp-mode-hook lisp-interaction-mode-hook) . highlight-function-calls-mode)
  :custom
  (highlight-function-calls-not t)
  (highlight-function-calls-macro-calls t)
  (highlight-function-calls-special-forms t)
  :custom-face
  (highlight-function-calls-face ((t (:underline nil :inherit font-lock-function-call-face)))))

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

;;;; Fish-mode
(use-package fish-mode
  :ensure t
  :mode
  "\\.fish\\'")

;;;; Enhancements to basic text editing
;; Puni: major-mode agnostic structural editing.  We use some of its
;; commands.
(use-package puni
  :ensure t
  :defer t
  :bind
  (("C-S-o" . puni-split)
   ("M-+" . puni-splice)
   ("M-R" . puni-raise)
   ([remap transpose-sexps] . puni-transpose)
   ([remap kill-word] . puni-forward-kill-word)
   ([remap backward-kill-word] . puni-backward-kill-word)
   ([remap insert-parentheses] . puni-syntactic-backward-punct)
   ([remap move-past-close-and-reindent] . puni-syntactic-forward-punct))
  :custom
  (puni-confirm-when-delete-unbalanced-active-region nil))

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

;;;; Abbrev
;; Automatically correct typed strings (e.g. words).  Most useful for
;; correcting spelling mistakes as they are made.
(use-package abbrev
  :ensure nil
  :custom
  (save-abbrevs 'silently)
  (abbrev-suggest t)
  (abbrev-suggest-hint-threshold 2)
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'abbrev-mode)
  ;; Enable the mode globally
  (setq-default abbrev-mode t))

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
  :custom
  ;; Headlines
  ;; TODO 2025-05-22: Document:
  ;; - `org-hide-leading-stars'
  ;; - `org-n-level-faces'
  ;; - `org-cycle-separator-lines'
  ;; - `org-cycle-level-faces'
  ;; - `org-insert-heading-respect-content'
  ;; - `org-fontify-todo-headline’
  ;; (org-ellipsis " ⮷")                        ; TODO 2025-05-22: Revisit this
  (org-blank-before-new-entry
   '((heading . auto)
     (plain-list-item . nil)))
  (org-M-RET-may-split-line
   '((table . nil)
     (default . t)))
  (org-startup-folded 'nofold)
  (org-fontify-done-headline nil)

  ;; Org blocks
  (org-structure-template-alist
   '(("s" . "src")
     ("S" . "src emacs-lisp")
     ("q" . "quote")
     ("c" . "comment")
     ("C" . "center")
     ("e" . "export")
     ("E" . "example")
     ("v" . "verse")))
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line nil)

  ;; Timestamps
  (org-edit-timestamp-down-means-later t)
  (org-extend-today-until 4)
  (org-use-effective-time t)

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

  ;; Tags
  (org-tags-column 0)
  (org-tags-exclude-from-inheritance '("__journal" "__top_of_mind"))

  ;; Properties
  (org-use-property-inheritance '("CATEGORY" "ARCHIVE"))

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
      (add-hook hook #'pulsar-reveal-entry)))
  
  ;; Standardize creation of IDs and targets
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
           (target (concat "<<" id ">>")))
      (if (called-interactively-p 'interactive)
          (insert target)
        target)
      (let ((org-link-context-for-files t))
        (org-link--add-to-stored-links (org-store-link '(16)) id))
      target)))

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

;; Collection of org packages
(use-package org-contrib
  :ensure t
  :after org)

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

;;;; Org-expiry
(use-package org-expiry
  :ensure nil
  :after org-contrib
  :custom
  (org-expiry-inactive-timestamps t))

;;;; Olivetti
(use-package olivetti
  :ensure t
  :hook
  ((org-mode-hook Info-mode-hook emacs-news-view-mode-hook org-msg-edit-mode-hook markdown-mode-hook)
   . olivetti-mode)
  ;; (olivetti-mode-hook . krisb-olivetti-set-bookmark-face)
  :bind
  ( :map olivetti-mode-map
    ("C-c |" . nil))
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

;;;; Visual-wrap
;; TODO 2025-05-27: Document history with `visual-wrap’
(use-package visual-wrap
  :ensure nil
  :hook
  (on-first-buffer-hook . global-visual-wrap-prefix-mode))

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
  (jinx-delay 0.7)
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'jinx-mode)

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
  ( :map org-mode-map
    ("C-c u" . org-web-tools-insert-link-for-url))
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
  ;; (dictionary-server "localhost")
  (dictionary-server nil)
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

;;;; Display-line-numbers
;; Show line numbers on the left fringe
(use-package display-line-numbers
  :ensure nil
  :bind
  ( :map krisb-toggle-keymap
    ("l" . display-line-numbers-mode))
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width-start t)) ; Use same width throughout

;;;; Citar
(use-package citar
  :ensure t
  ;; NOTE 2025-05-27: We might have to :demand this package if I need
  ;; it before I call any of its commands.  However, I would ideally
  ;; avoid that because other packages like citar-org-node are lazily
  ;; loaded, waiting for citar to load before themselves.  Thus, if
  ;; citar is immediately loaded at startup, so too will those other
  ;; packages (which may also load other packages).  Maybe we should
  ;; be satisfied with e.g. :defer 10.
  :defer t
  :hook
  (org-mode-hook . citar-capf-setup)
  :bind
  (("C-c b b" . citar-insert-citation)
   ("C-c b o" . citar-open)
   ("C-c b f" . citar-open-files)
   ("C-c b n" . citar-open-notes)
   :map org-mode-map
   ([remap org-cite-insert] . citar-insert-citation))
  :custom
  (citar-bibliography krisb-bibliography-files)
  (citar-notes-paths (list krisb-notes-directory))
  (citar-open-entry-function #'citar-open-entry-in-file)
  (citar-default-action #'citar-open-files)
  ;; See also citar-format.el for more information on what happens with the
  ;; templates.
  (citar-templates
   '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}") ; Candidate
     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}") ; Candidate annotation
     (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n") ; Formatted reference
     (note . "${title} by ${author}"))) ; New note title

  ;; Instruct org-cite to use citar
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-org-styles-format 'long)
  :config
  ;; Fancy UI
  (with-eval-after-load 'all-the-icons
    ;; Taken from https://github.com/emacs-citar/citar/wiki/Indicators
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon
                "file-o"
                :face 'all-the-icons-green
                :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  " ; Need this because the default padding is too low for these icons
       :tag "has:files"))
    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (all-the-icons-octicon
                "link"
                :face 'all-the-icons-orange
                :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (all-the-icons-material
                "speaker_notes"
                :face 'all-the-icons-blue
                :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "  "
       :tag "has:notes"))
    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon
                "circle-o"
                :face 'all-the-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))
    (setq citar-indicators
          (list citar-indicator-files-icons
                citar-indicator-links-icons
                citar-indicator-notes-icons
                citar-indicator-cited-icons))))

;; ;; Use `citar' with `org-cite'
;; (use-package citar-org
;;   :after oc
;;   :ensure nil
;;   :custom
;;   (org-cite-insert-processor 'citar)    ;
;;   (org-cite-follow-processor 'citar)
;;   (org-cite-activate-processor 'citar)
;;   (citar-org-styles-format 'long))

;;;; Org-roam
(use-package org-roam
  :ensure t
  :defer t
  ;; TODO 2025-05-27: Revisit this
  ;; :bind
  ;; ( :map krisb-note-keymap
  ;;   ("f" . org-roam-node-find)
  ;;   ("i" . org-roam-node-insert)
  ;;   ("c" . org-roam-capture)
  ;;   ("l" . org-roam-buffer-toggle)
  ;;   ("ta" . org-roam-tag-add)
  ;;   ("tr" . org-roam-tag-remove)
  ;;   ("g" . org-roam-graph))
  :custom
  (org-roam-directory krisb-notes-directory)
  (org-roam-db-node-include-function
   (lambda () (not (member "ATTACH" (org-get-tags)))))
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-mode-sections
   '((org-roam-backlinks-section :unique t)
     org-roam-reflinks-section))
  :config
  ;; Fold headings by default
  (add-to-list 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)

  ;; 2025-03-24: Using org-node/org-mem.el to replace this.  I dont enable it at
  ;; all because enabling causes a `org-roam-db-sync' on startup since it
  ;; detects that org-mem's db isn't its own...
  ;; (org-roam-db-autosync-mode 1)

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

;;;; Org-roam-folgezettel
(use-package org-roam-folgezettel
  :ensure ( :repo "https://github.com/krisbalintona/org-roam-folgezettel.git"
            :branch "vtable-unstable")
  :defer t
  :hook
  (org-roam-folgezettel-mode-hook . hl-line-mode)
  (org-roam-folgezettel-mode-hook . (lambda () (setq-local line-spacing 0.2)))
  :bind
  ( :map krisb-note-keymap
    ("m" . org-roam-folgezettel-list)
    ("s" . org-roam-folgezettel-show-node-in-list))
  :custom
  (org-roam-folgezettel-default-filter-query '(box "main"))
  :config
  ;; FIXME 2025-06-30: Eventually upstream contents of
  ;; krisb-org-roam-ext once I figure out a generalizable zettelkasten
  ;; workflow for most/all users.
  (require 'krisb-org-roam-ext)

  ;; Load embark integration
  (with-eval-after-load 'embark
    (require 'org-roam-folgezettel-embark))

  ;; We must add these after their default values are set by org
  (with-eval-after-load 'org
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
    (add-to-list 'org-use-property-inheritance "ROAM_BOX")))

;;;; Citar-org-node
(use-package citar-org-node
  :ensure (:repo "https://github.com/krisbalintona/citar-org-node.git")
  :after citar
  :demand t
  :bind
  ( :map krisb-note-keymap
    ("b a" . citar-org-node-add-refs)
    ("b o" . citar-org-node-open-resource))
  :config
  (citar-org-node-mode 1)
  (add-to-list 'mode-line-collapse-minor-modes 'citar-org-node-mode))

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

;;;; Word-wrap-mode
;; Instead of wrapping lines on whitespace, as is the default, also
;; wrap lines on the characters denoted by
;; `word-wrap-whitespace-characters’, e.g., em-dashes and en-dashes.
(use-package word-wrap-mode
  :ensure nil
  :demand t
  :config
  (global-word-wrap-whitespace-mode 1))

;;;; Astute.el
;; Display punctuation typographically (e.g., em-dashes as "—" and
;; en-dashes as "–")
(use-package astute
  :ensure t
  :defer t
  :hook
  (text-mode-hook . astute-mode)
  :custom
  (astute-lighter "")
  (astute-prefix-single-quote-exceptions
   '("bout"
     "em"
     "n'"
     "cause"
     "round"
     "twas"
     "tis")))

;;;; Flymake-vale
(use-package flymake-vale
  :ensure (:repo "https://github.com/tpeacock19/flymake-vale.git")
  :defer t
  :hook (text-mode-hook . flymake-vale-load))

;;;; Org-hide-drawers
;; Make org drawers less visually obtrusive.
(use-package org-hide-drawers
  :ensure ( :repo "https://github.com/krisbalintona/org-hide-drawers.git"
            :branch "devel")
  :defer t
  :hook
  (org-mode-hook . org-hide-drawers-mode)
  :bind
  ( :map krisb-toggle-keymap
    ("h" . org-hide-drawers-transient))
  :custom
  (org-hide-drawers-display-strings
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
         (list 'property-drawer-regexp (propertize " #" 'face 'shadow) (rx (0+ anychar)))))
  :config
  (add-to-list 'mode-line-collapse-minor-modes 'org-hide-drawers-mode)

  (require 'transient)
  (transient-define-prefix org-hide-drawers-transient ()
    "Transient map for useful org-hide-drawers commands."
    [("h" "Hide drawers" org-hide-drawers-make-overlays)
     ("u" "Unhide drawers" org-hide-drawers-delete-overlays)
     ("t" "Toggle hiding" org-hide-drawers-toggle)]))

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

;;;; Wombag
(use-package wombag
  :ensure ( :repo "https://github.com/krisbalintona/wombag.git"
            :branch "merge")
  :defer t
  :hook
  ((wombag-show-mode-hook . org-remark-mode)
   (wombag-show-mode-hook . krisb-wombag-entry-setup))
  :bind
  ( :map krisb-open-keymap
    ("w" . wombag))
  :custom
  (wombag-dir (no-littering-expand-var-file-name "wombag"))
  (wombag-db-file (no-littering-expand-var-file-name "wombag/wombag.sqlite"))
  (wombag-username "krisbalintona")
  (wombag-host "https://app.wallabag.it")
  (wombag-password (auth-source-pick-first-password :host "app.wallabag.it"))
  (wombag-client-id "23882_1jzdzdd09ikgw4k8o0cog4wggk48cgc0gwk8oos0gsc44gcsco")
  (wombag-client-secret (auth-source-pick-first-password :host "emacs-wombag.el"))
  (wombag-search-filter "")
  :config
  (defun krisb-wombag-entry-setup ()
    "Set up the visual for wombag-entry buffers."
    (setq-local line-spacing 0.08)
    (face-remap-add-relative 'default :height 1.1)
    (when (require 'olivetti nil t)
      (olivetti-mode 1)
      (olivetti-set-width 120))
    (when (require 'mixed-pitch nil t)
      (mixed-pitch-mode 1))
    (visual-line-mode 1)))

;;;; Org-keyterm-index
(use-package org-keyterm-index
  :ensure (:repo "https://github.com/krisbalintona/org-keyterm-index.git")
  :after org
  :defer t)

;;; Blogging
(require 'krisb-org-publish)

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
  (message-send-hook . krisb-message-check-subject)
  (message-send-hook . krisb-message-check-from)
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

;;;; Footnote
;; TODO 2025-05-24: Document:
;; - `footnote-section-tag’
;; Footnotes for `message-mode'
(use-package footnote
  :ensure nil
  :hook
  (message-mode-hook . footnote-mode)
  :custom
  (footnote-mode-line-string "")
  (footnote-spaced-footnotes nil)
  (footnote-prompt-before-deletion nil))

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
  (notmuch-show-hook . visual-wrap-prefix-mode)
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
  (notmuch-draft-folder     ; Relative to root of the notmuch database
   (file-relative-name krisb-email-drafts-directory krisb-email-directory))
  (notmuch-draft-save-plaintext 'ask)
  (notmuch-tagging-keys
   `(("a" notmuch-archive-tags "Archive")
     ("r" notmuch-show-mark-read-tags "Mark read")
     ("f" ("+flagged") "Flag")
     ("s" ("+spam" "-inbox") "Mark as spam")
     ("t" ("+trash" "-inbox") "Trash")))
  (notmuch-tag-formats
   '(("unread" (propertize tag 'face 'notmuch-tag-unread))
     ("flagged" (propertize tag 'face 'notmuch-tag-flagged))
     ("watch" (propertize tag 'face 'font-lock-warning-face))))
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
  ;; The order of headers in this list seems to be the order in which
  ;; they will appear in `notmuch-show’ buffers.  See also the user
  ;; option `notmuch-show-message-visible'.  Additionally, to add
  ;; headers to this list that are beyond the default, you must add to
  ;; the “extra_headers” setting in the “show” section of your notmuch
  ;; config.  Finally, to have these extra headers be query-able via
  ;; notmuch search queries, be sure to define a search term prefix
  ;; for it.  (See (info "(notmuch-config) DESCRIPTION") for how to
  ;; achieve such a set up.)
  (notmuch-message-headers '("To" "Cc" "List-Id" ; Show mailing list ID
                             "Date" "Subject"))
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
  (sendmail-program (if (executable-find "gmi") "gmi" "sendmail"))
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
  (advice-add 'notmuch-show-message-adjust :override #'ignore)

  ;; Add to `display-buffer-alist'
  (add-to-list 'display-buffer-alist
               '("\\*notmuch-hello\\*"
                 (display-buffer-in-tab display-buffer-full-frame)
                 (tab-group . "media"))))

;;;; Ol-notmuch
;; Org-links for search queries (i.e. notmuch-search-mode,
;; notmuch-tree-mode) and messages (i.e. notmuch-show-mode).
(use-package ol-notmuch
  :ensure t
  ;; :after (:any ol org-capture)
  ;; :demand t
  :defer t
  :init
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
                   "* TODO [#E] Review subscription/newsletter email: [[%L][\"%:subject\"]] %? :email:inbox:%^g

From %:from
To: %:to\n"
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
  :config
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

;;;; Notmuch-addr
;; Better address completion for notmuch; replaces the built-in
;; `notmuch-address' completion system. Read
;; https://nmbug.notmuchmail.org/nmweb/show/20201108231150.5419-1-jonas%40bernoul.li
;; for more information
(use-package notmuch-addr
  :ensure t
  :after notmuch-address
  :demand t
  :config
  (notmuch-addr-setup))

;;;; Notmuch-bookmarks
(use-package notmuch-bookmarks
  :ensure t
  :after notmuch
  :demand t
  :config
  (notmuch-bookmarks-mode))

;;;; Mode line indicator for notmuch emails
;; Try using display-time's built-in email indicator --- less
;; informative but more visually subtle than `notmuch-indicator'.
;; Obviously the below applies only when `display-time-mode' is
;; non-nil.
;; Modify `display-time-mail-string’ such that it displays a neat
;; unicode icon for mail.  Then modify `display-time-mail-function’
;; such that it returns non-nil when there is more than one notmuch
;; email available that is worth notifying my about. (Note: we use
;; `display-time-mail-string’ instead of `display-time-use-mail-icon’
;; because the latter is for xpm and pbm files; see the final form in
;; `display-time-string-forms’.)
(with-eval-after-load 'time
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
  ;; seem to succeed as the command run when clicking the mode line
  ;; mail string?
  (setopt display-time-mail-string krisb-display-time-mail-string
          display-time-mail-function 'krisb-display-time-mail-notmuch-function)
  (with-eval-after-load 'notmuch
    (setopt display-time-mail-face 'notmuch-search-flagged-face))

  ;; FIXME 2025-05-26: This assumes that we always leave notmuch via
  ;; the notmuch-hello buffer.  This is a workaround because I know of
  ;; no other reliable indiaction of when I’m done checking mail.  Is
  ;; there something better?
  ;; Advise `notmuch-bury-or-kill-this-buffer’ such that it updates
  ;; after leaving the notmuch-hello buffer.  This prevents the mail
  ;; string from being visible right after we’ve just checked mail in
  ;; notmuch
  (advice-add 'notmuch-bury-or-kill-this-buffer :around
              (lambda (&rest args)
                (when (equal major-mode 'notmuch-hello-mode)
                  (display-time-update))
                (apply args))))

;;;; Notmuch-transient
(use-package notmuch-transient
  :disabled t                     ; 2025-09-07: Cool but not essential
  :ensure t
  :after notmuch
  :demand t
  :custom
  (notmuch-transient-prefix "C-d")
  (notmuch-transient-add-bindings t))

;;;; MUA (mail transfer agent)

;;;;; Sendmail
;; Use `sendmail' program to send emails?  If yes, set the value of
;; `send-mail-function' to `sendmail-send-it'
(use-package sendmail
  :ensure nil
  :after message
  :demand t
  :custom
  (mail-default-directory krisb-email-drafts-directory)
  ;; The following options make sure that emails are sent from the
  ;; email address specified in the "from" header field!  Taken from
  ;; https://jonathanchu.is/posts/emacs-notmuch-isync-msmtp-setup/
  (mail-specify-envelope-from t)
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header))

;;;;; Smtpmail
;; Use `msmtp' program to send emails?  If yes, set the value of
;; `send-mail-function' to `smtpmail-send-it'
(use-package smtpmail
  :ensure nil
  ;; For AUR:
  ;; :ensure-system-package msmtp
  :after message
  :demand t
  :custom
  (smtpmail-queue-mail nil)
  ;; Below are settings for Gmail.  See
  ;; https://support.google.com/mail/answer/7126229?hl=en#zippy=%2Cstep-change-smtp-other-settings-in-your-email-client
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (smtpmail-servers-requiring-authorization "gmail") ; NOTE 2024-08-25: Fixes Gmail's 530 error on sending
  ;; Make sure email details that are used are not the current (when
  ;; flushing) variables, but the variables used when writing the
  ;; email
  (smtpmail-store-queue-variables t)
  (smtpmail-queue-dir (expand-file-name ".smtp-queue" krisb-email-drafts-directory)))

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
      :empty-lines 1)))
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

;;;; Org-agenda
(use-package org-agenda
  :custom
  ;; Org-agenda buffer
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-sticky t)
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

;;;; Org-clock
;; TODO 2025-05-24: Document:
;; - `org-clock-out-when-done’
(use-package org-clock
  :custom
  (org-clock-in-resume t)
  (org-clock-out-remove-zero-time-clocks t))

;;;; Org-depend
;; Add blocking and triggering actions when an org-todo state is
;; changed.
(use-package org-depend
  :ensure nil
  :after org-contrib
  :demand t)

;;;; Org-review
(use-package org-review
  :ensure t
  :defer t
  :bind
  ( :map org-mode-map
    ("C-c r s" . org-review-insert-next-review)
    ("C-c r l" . org-review-insert-last-review)
    ("C-c r u" . krisb-org-review-unreview)
    :map org-agenda-mode-map
    ("C-c r s" . org-review-insert-next-review)
    ("C-c r l" . org-review-insert-last-review)
    ("C-c r u" . krisb-org-review-unreview))
  :custom
  (org-review-delay "+8d")
  (org-review-last-timestamp-format 'inactive)
  (org-review-next-timestamp-format 'inactive)
  (org-review-sets-next-date t)
  :config
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

;;;; Org-refile
;; TODO 2025-06-25: Document:
;; - `org-refile-use-cache'
(use-package org-refile
  :ensure nil
  :after org
  :custom
  (org-refile-targets
   '((org-agenda-files . (:tag . "project"))
     (org-agenda-files . (:level . 0))
     (nil . (:maxlevel . 4))))
  ;; TODO 2024-10-07: Think about whether I actually want this.  What
  ;; if I want to refile to a non-todo heading in the current file?
  ;; (org-refile-target-verify-function ; Only let not done todos be refile targets
  ;;  (lambda () (if (org-entry-is-todo-p) (not (org-entry-is-done-p)))))
  (org-refile-target-verify-function nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  :config
  ;; Workaround for vertico issue with `org-refile'.  See
  ;; https://github.com/minad/vertico#org-refile
  (setopt org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil)
  (when (bound-and-true-p vertico-mode)
    (advice-add #'org-olpath-completing-read :around
                (lambda (&rest args)
                  (minibuffer-with-setup-hook
                      (lambda () (setq-local completion-styles '(basic)))
                    (apply args))))))

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

;; Enable `delete-selection-mode'.  When selecting text, if typing new
;; text, replace the selected text with the new text
(delete-selection-mode t)

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

;; Set `sentence-end-double-space' conditionally
(defun krisb-sentence-end-double-space-setup ()
  "Set up the value for `sentence-end-double-space'."
  (setq-local sentence-end-double-space
              (cond ((derived-mode-p '(prog-mode conf-mode log-edit-mode)) t)
                    ((derived-mode-p '(text-mode wombag-show-mode)) nil))))

(dolist (mode '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode #'krisb-sentence-end-double-space-setup))

;; `duplicate-dwim' binding
;; TODO 2025-05-22: Document the `duplicate-line-final-position'and
;; `duplicate-region-final-position' user options
(bind-key "C-x ;" #'duplicate-dwim)

;; Don't prompt user to confirm killing running sub-processes when
;; quitting Emacs
(setopt confirm-kill-processes nil)
