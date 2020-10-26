(setq user-full-name "Kristoffer Balintona"
      user-mail-address "krisbalintona@gmail.com")

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst python-p
  (or (executable-find "python3")
      (and (executable-find "python")
           (> (length (shell-command-to-string "python --version | grep 'Python 3'")) 0)))
  "Do we have python3?")

(defconst pip-p
  (or (executable-find "pip3")
      (and (executable-find "pip")
           (> (length (shell-command-to-string "pip --version | grep 'python 3'")) 0)))
  "Do we have pip3?")

(defconst clangd-p
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
  "Do we have clangd?")

(defconst eaf-env-p
  (and *sys/linux* (display-graphic-p) python-p pip-p
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  "Do we have EAF environment setup?")

(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory) ; Select folder to store packages
      package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ;; ;; Chinese servers
        ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup t)          ; To prevent initializing twice
  (package-initialize))

;; Set use-package-verbose to t for interpreted .emacs, and to nil for
;; byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

(unless (package-installed-p 'use-package) ; Install use-package if not installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(setq quelpa-upgrade-interval 2) ; Update quelpa packages every X days after init
(add-hook #'after-init-hook #'quelpa-upgrade-all-maybe)

(setq quelpa-checkout-melpa-p t) ; Update (or not) local MELPA git repo since I'll be using quelpa for non-MELPA packages
(setq quelpa-upgrade-p nil) ; Only applies to MELPA packages?: Don't upgrade packages automatically

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(quelpa-use-package-activate-advice) ; Necessary for use-package-always-ensure to take from ELPA but all quelpa packages to take from quelpa

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 1) ;; in days
  (auto-package-update-prompt-before-update nil)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results nil)
  :config
  (auto-package-update-maybe)
  )

(use-package diminish)

(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq confirm-kill-emacs 'y-or-n-p) ; Confirm before killing emacs

(custom-set-variables '(confirm-kill-processes nil))

(add-hook 'before-save-hook 'time-stamp) ; or (add-hook 'write-file-functions

(setq completion-ignore-case t)
(custom-set-variables
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t))

(setq make-backup-files nil)
(setq create-lockfiles nil) ; don't create .# files (crashes 'npm start')

;; 2 Spaces
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

;; Spaces instead of indentations
(setq-default indent-tabs-mode nil)

(setq ad-redefinition-action 'accept)

(setq large-file-warning-threshold nil)

(setq auto-save-default nil) ; Prevents transitory files from being saved

(setq x-stretch-cursor t) ; Stretch cursor to the glyph width

(setq window-combination-resize t) ; take new window space from all

(global-subword-mode t) ; Iterate through CamelCase words

(setq truncate-string-ellipsis "…") ; For all elliipsis

(setq max-mini-window-height 0.15)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq kb/default-font "RobotoMono Nerd Font")

(add-to-list 'default-frame-alist `(font . ,kb/default-font))
;; (add-to-list 'default-frame-alist '(font . "Iosevka Fixed Slab"))

(defun kb/default-fonts-setup ()
  "Set Emacs-wide fonts"

  ;; (set-face-attribute 'default nil :font "Iosevka Fixed Slab" :height 135)
  (set-face-attribute 'default nil :font kb/default-font :height 135)
  (set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 140)
  (set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 148)

  (set-fontset-font ; Emoji support
   t
   '(#x1f300 . #x1fad0)
   (cond
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola")
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
    ;; GNU Emacs Removes Color Emoji Support on the Mac
    ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
    ))
  )

(if (daemonp) ; Set fonts based on if daemon or not
    (progn (add-hook 'server-after-make-frame-hook 'kb/default-fonts-setup)
           (add-hook 'window-setup-hook 'kb/default-fonts-setup)
           (add-hook 'server-after-make-frame-hook 'org-mode-restart)) ; Make sure fonts are properly loaded in org files
  (add-hook 'window-setup-hook 'kb/default-fonts-setup))
;; Regular fonts

(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode)
  :custom
  (mixed-pitch-fixed-pitch-faces
   '(diff-added diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face line-number line-number-current-line line-number-major-tick line-number-minor-tick markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim

                highlight-indent-guides-character-face
                ))
  )

;; (use-package doom-themes
;;   :ensure t
;;   :config (load-theme 'doom-dracula t))
;; (use-package doom-themes
;;   :ensure t
;;   :config (load-theme 'doom-palenight t))
(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))
;; (use-package mood-one-theme
;;   :ensure t
;;   :config (load-theme 'mood-one t))
;; (use-package spacemacs-theme
;;   :ensure t
;;   :config (load-theme 'spacemacs-dark t))

(column-number-mode)

(setq display-line-numbers-type 'relative)

;; Enabled for these
(dolist (mode '(prog-mode-hook
                ))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Disabled for these
(dolist (mode '(org-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-frame-parameter (selected-frame) 'alpha '(98 . 85))
(add-to-list 'default-frame-alist '(alpha . (98 . 85)))

(setq line-spacing 0) ; This is default

(use-package doom-modeline
  :custom
  (doom-modeline-window-width-limit fill-column) ; The limit of the window width.
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  ;; (doom-modeline-icon (display-graphic-p)) ; Show icons if in Emacs GUI
  (doom-modeline-icon t) ; In order to work with emacsclient
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-number-limit 99)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-persp-name t)
  (doom-modeline-display-default-persp-name nil)
  (doom-modeline-persp-icon t)
  (doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (doom-modeline-github nil)
  (doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (doom-modeline-modal-icon t)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (doom-modeline-mu4e nil) ; This variable is flipped?

  ;; Whether display the gnus notifications.
  (doom-modeline-gnus t)
  (doom-modeline-gnus-timer 2)
  ;; (doom-modeline-gnus-excluded-groups '("dummy.group"))

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (doom-modeline-irc t)
  (doom-modeline-irc-stylize 'identity)

  ;; Whether display the environment version.
  (doom-modeline-env-version t)
  ;; Or for individual languages
  (doom-modeline-env-enable-python t)
  (doom-modeline-env-enable-ruby t)
  (doom-modeline-env-enable-perl t)
  (doom-modeline-env-enable-go t)
  (doom-modeline-env-enable-elixir t)
  (doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (doom-modeline-env-ruby-executable "ruby")
  (doom-modeline-env-perl-executable "perl")
  (doom-modeline-env-go-executable "go")
  (doom-modeline-env-elixir-executable "iex")
  (doom-modeline-env-rust-executable "rustc")

  ;; What to dispaly as the version while a new one is being loaded
  (doom-modeline-env-load-string "...")

  (doom-modeline-height 26)
  (doom-modeline-bar-width 6)
  :config
  (if (daemonp) ; Hooks depending on daemon or not
      (progn (add-hook 'server-after-make-frame-hook 'doom-modeline-mode 100)
             (add-hook 'window-setup-hook 'doom-modeline-mode))
    (add-hook 'window-setup-hook 'doom-modeline-mode)) ; Use this hook to prevent right side from being clipped

  (set-face-attribute 'mode-line nil :family "Noto Sans" :height 0.75)
  (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 0.68)

  ;; Don't show encoding on modeline if it is UTF-8
  (defun doom-modeline-conditional-buffer-encoding ()
    (setq-local doom-modeline-buffer-encoding
                (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                            (eq buffer-file-coding-system 'utf-8)))))
  (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
  (add-hook 'doom-modeline-mode-hook #'doom-modeline-conditional-buffer-encoding) ; Necessary so it takes affect imediately, not before I change major modes

  ;; Show actual path of file in symlinks
  (setq find-file-visit-truename t)

  ;; Enable time in the mode-line
  (display-time-mode t)
  ;; Show battery
  (setq battery-load-critical 15)
  (setq battery-load-low 25)
  (unless (equal "Battery status not available"
                 (battery))
    (display-battery-mode 1))                       ; On laptops it's nice to know how much power you have

  ;; Show file-size
  (size-indication-mode t)

  (doom-modeline-def-modeline 'main
    '(bar " " matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info " " battery " " minor-modes checker input-method buffer-encoding major-mode process vcs))
  )

(use-package general
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys)

  (general-create-definer kb/leader-keys
    :keymaps '(normal insert visual emacs eshell helpful)
    :prefix "SPC"
    :global-prefix "M-SPC")

  (kb/leader-keys
    "t"  '(:ignore t :which-key "Toggles")
    "tl" '(display-line-numbers-mode :which-key "Line numbers")
    "o"  '(:ignore t :which-key "Open")
    "b"  '(:ignore t :which-key "Buffers")
    "bp" '(previous-buffer :which-key "Prev buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "f"  '(:ignore t :which-key "Files")
    "fs" '(save-buffer :which-key "Save buffer")
    "h"  '(:ignore t :which-key "Help")
    "q"  '(:ignore t :which-key "Quit")
    "l"  '(:ignore t :which-key "Langtool")
    "n" '(:ignore t :which-key "Org-roam")
    "i" '(:ignore t :which-key "Copying and pasting")
    "w" '(:ignore t :which-key "Manage windows")
    "g"   '(:ignore t :which-key "git")
    "e"   '(:ignore t :which-key "Eval stuff")
    "eb"  '(eval-buffer :which-key "Eval buffer")

    "u" 'universal-argument
    )

  (kb/leader-keys
    :keymaps '(visual)
    "er" '(eval-region :which-key "Eval region")
    "ee" '(eval-last-sexp :which-key "Eval last sexp")
    )
  )

(setq org-directory "~/Documents/org-database/")
(defvar kb/agenda-dir (concat org-directory "agenda/"))
(defvar kb/roam-dir (concat org-directory "roam/"))
(defvar kb/library-dir (concat org-directory "library/"))

(defvar kb/all-org-dir-files (cl-remove-if
                              (lambda (it)
                                (string-match-p (rx "archive.org") it))
                              (directory-files-recursively org-directory ".org$")))
(defvar kb/all-agenda-dir-files (cl-remove-if
                                 (lambda (it)
                                   (string-match-p (rx "archive.org") it))
                                 (directory-files-recursively kb/agenda-dir ".org$")))
(defvar kb/all-agenda-dir-files-minus-inbox (cl-remove-if
                                             (lambda (it)
                                               (string-match-p (rx "archive.org") it)
                                               (string-match-p (rx "inbox.org") it))
                                             (directory-files-recursively kb/agenda-dir ".org$")))

(defvar kb/library-dir (concat org-directory "library"))
(defvar kb/emacs-config-file (concat user-emacs-directory "configs/base-config.org"))

(defvar kb/dot-config-dir "~/.config/")
(defvar kb/wm-config-file (concat kb/dot-config-dir "i3/config"))

(defun kb/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x K") 'kb/kill-current-buffer)
(kb/leader-keys
  "bK" '(kb/kill-current-buffer :which-key "Kill current buffer")
  )

(defun kb/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(kb/leader-keys
  "qQ" '(kb/kill-all-buffers :which-key "Kill all buffers"))

(defun +default/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(kb/leader-keys
  "fy" '(+default/yank-buffer-filename :which-key "Yank filename")
  )

(defun kb/delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.

    If PATH is not specified, default to the current buffer's file.

    If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          ;; ;; Ensures that windows displaying this buffer will be switched to
          ;; ;; real buffers (`doom-real-buffer-p')
          ;; (doom/kill-this-buffer-in-all-windows buf t)
          ;; (doom--update-files path)
          (kb/kill-current-buffer)
          (message "Deleted %S" short-path))))))

(kb/leader-keys
  "fD" '(kb/delete-this-file :which-key "Delete current file")
  )

(defun kb/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

    If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    ;; (doom--update-files old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

(kb/leader-keys
  "fR" '(kb/move-this-file :which-key "Rename current file")
  )

(defun kb/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (doom--sudo-file-path file)))

(defun kb--sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(kb/leader-keys
  "fu" '(kb/sudo-find-file :which-key "Find file as sudo")
  )

(defun kb/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-file
   (kb--sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

(kb/leader-keys
  "fU" '(kb/sudo-this-file :which-key "Sudo save this file")
  )

(defun kb/org-file-jump-to-heading (org-file heading-title)
  (interactive)
  (find-file (expand-file-name org-file))
  (goto-char (point-min))
  (search-forward (concat "* " heading-title))
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun kb/org-file-show-headings (org-file)
  (interactive)
  (find-file (expand-file-name org-file))
  (counsel-org-goto)
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(kb/leader-keys
  "fc"  '(:ignore t :which-key "Custom file searches")

  "fo" '(counsel-outline :which-key "Goto heading in current file")

  "fcL" '((lambda () (interactive) (counsel-find-file kb/library-dir)) :which-key "Library files")

  "fcw" '((lambda () (interactive) (find-file kb/wm-config-file)) :which-key "WM config file")
  "fce" '((lambda () (interactive) (find-file kb/emacs-config-file)) :which-key "Emacs base config file")

  "fcE" '((lambda () (interactive) (kb/org-file-show-headings kb/emacs-config-file)) :which-key "Edit Emacs base config")

  ;; ;; Section for Jumping directly to headings
  ;; "fds" '((lambda () (interactive) (kb/org-file-jump-to-heading "~/.dotfiles/Systems.org" "Base Configuration")) :which-key "base system")
  ;; "fdS" '((lambda () (interactive) (kb/org-file-jump-to-heading "~/.dotfiles/Systems.org" system-name)) :which-key "this system")
  ;; "fdp" '((lambda () (interactive) (kb/org-file-jump-to-heading "~/.dotfiles/Desktop.org" "Panel via Polybar")) :which-key "polybar")
  )

(kb/leader-keys
  "qs" '(org-save-all-org-buffers :which-key "Save all org buffers")
  )

(kb/leader-keys
  "mg" 'org-mark-ring-goto
  "mc" 'counsel-mark-ring
  "ma" 'org-mark-ring-push
  )

(use-package helpful
  :after (evil evil-collection)
  :demand t ; Don't wait for the binded keys
  :bind
  ;; Counsel commands are already remapped in the counsel section
  ;; ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ;; ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)

  :config
  (evil-define-key '(visual normal) 'global (kbd "K") 'helpful-at-point)

  (kb/leader-keys
    "hk" '(helpful-key :which-key "Desc key"))
  )

(use-package beacon
  :custom
  (beacon-blink-when-focused t) ; Blink when Emacs comes into focus
  (beacon-blink-delay 0.1)
  (beacon-blink-duration 0.5)
  :config
  (beacon-mode)
  )

(use-package smooth-scrolling
  :custom
  (smooth-scroll-margin 9)
  :config
  (smooth-scrolling-mode t)

  (add-hook 'eshell-mode-hook (lambda () (smooth-scrolling-mode 0))) ; Turn off smooth scrolling in eshell
  )

(use-package saveplace
  :config
  (save-place-mode)
  )

(use-package which-key
  :custom
  (which-key-idle-delay 0.85)
  (which-key-show-early-on-C-h t) ; Show which-key immediately for C-h
  (which-key-add-column-padding 0)
  (which-key-max-display-columns nil)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right) ; Show which-key on the ritght
 )

(use-package all-the-icons)

(use-package hide-mode-line)

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t) ; Save buffer if Emacs is idle
  (super-save-idle-duration 10) ; Wait 10 seconds for idle trigger
  (super-save-remote-files t) ; Turn on saving of remote files (those pulled from git repo?)
  (super-save-exclude nil) ; Don't exclude anything from being saved
  :config
  (add-to-list 'super-save-triggers 'evil-window-next)

  (super-save-mode t)
  )

(use-package autorevert
  :custom
  (auto-revert-interval 7)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose t)
  :config
  (global-auto-revert-mode t)
  )

(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package unicode-fonts
  :preface
  (defun dw/replace-unicode-font-mapping (block-name old-font new-font)
    "Taken from https://github.com/daviwil/dotfiles/blob/master/Emacs.org#startup-performance"
    (let* ((block-idx (cl-position-if
                       (lambda (i) (string-equal (car i) block-name))
                       unicode-fonts-block-font-mapping))
           (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
           (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
      (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
            `(,updated-block))))
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :preface
  (defun kb/fix-unicode-fonts ()
    "Fix the font mappings to use the right emoji font"
    (mapcar
     (lambda (block-name)
       (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
     '("Dingbats"
       "Emoticons"
       "Miscellaneous Symbols and Pictographs"
       "Transport and Map Symbols"))

    (unicode-fonts-setup)
    )
  :config
  (if (daemonp) ; Hooks depending on daemon or not
      (add-hook 'server-after-make-frame-hook 'kb/fix-unicode-fonts)
    (add-hook 'window-setup-hook 'kb/fix-unicode-fonts))
  )

(use-package emojify
  :custom
  (global-emojify-mode t)
  )

(setq epg-pinentry-mode 'loopback)
;; (pinentry-start) ; This function doesn't exist?

(use-package sublimity
  :disabled
  :init
  (require 'sublimity-scroll)
  ;; (require 'sublimity-map) ;; experimental
  ;; (require 'sublimity-attractive)
  :custom
  (sublimity-scroll-weight 5)
  (sublimity-scroll-drift-length 10)
  :config
  (sublimity-mode)
  )

(use-package company
  :hook ((after-init . global-company-mode)
         (company-mode . company-tng-mode)
         (company-mode . evil-normalize-keymaps))
  :custom
  (company-idle-delay 0.8) ; Trigger completion in a second
  (company-tooltip-delay 1) ; Wait a little until the tooltip shows
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-global-modes '(not erc-mode message-mode help-mode gud-mode))

  ;; Only search the current buffer for `company-dabbrev' (a backend that
  ;; suggests text your open buffers). This prevents Company from causing
  ;; lag once you have a lot of buffers open.
  (company-dabbrev-other-buffers nil)

  ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
  ;; domain-specific words with particular casing.
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)

  (company-show-numbers t) ; Number the candidates (use M-1, M-2 etc to select completions).

  ;; ;; ;; These are the backends that Doom has active
  ;; (company-backends '(company-dabbrev company-yasnippet company-ispell))
  :config
  (add-to-list 'company-backends 'company-capf) ; For org-roam

  ;; Company-tng to use tab to cycle through suggestions
  (add-to-list 'company-frontends 'company-tng-frontend)
  (define-key company-active-map (kbd "RET") 'nil)
  (define-key company-active-map [return] 'nil)
  (define-key company-active-map (kbd "TAB") #'company-select-next)
  (define-key company-active-map [tab] #'company-select-next)
  (define-key company-active-map [backtab] #'company-select-previous)
  )

(use-package company-box
  :after (company)
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-show-single-candidate 'always)
  (company-box-backends-colors nil)
  (company-box-max-candidates 50)
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-icons-all-the-icons
   (let ((all-the-icons-scale-factor 0.8))
     `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
       (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
       (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
       (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
       (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
       (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
       (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
       (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
       (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
       (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
       (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
       (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
       (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
       (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
       (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
       (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
       (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
       (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
       (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
       (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
       (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
       (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
       (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
       (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
       (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
       (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))
  )

(use-package company-dict
  :custom
  (company-dict-dir (concat user-emacs-directory "dict/")) ; Where to look for dictionary files
  :config
  (add-to-list 'company-backends 'company-dict)
;; Make company-dict aware of other minor modes' dictionaries by
;; adding minor mode symbols to company-dict-minor-mode-list
  )

(use-package ivy
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              :map ivy-switch-buffer-map
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-extra-directories nil) ; Remove ./ and ../
  (ivy-use-virtual-buffers nil) ; Bookmarks and recent files in completion buffer
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t) ; Make prompt line selectable (as a candidate)
  (enable-recursive-minibuffers t) ; Allow minibuffer commands in minibuffer
  (ivy-height 17)
  (ivy-wrap t)
  (ivy-fixed-height-minibuffer t)
  (ivy-magic-slash-non-match-action nil) ; disable magic slash on non-match
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-on-del-error-function #'ignore) ; don't quit minibuffer on delete-error
  (ivy-sort-max-size 7500) ; Default is wayy too high - slows down in big projects
  (ivy-re-builders-alist
   '((counsel-esh-history . ivy--regex-fuzzy) ; Fuzzy findings for counsel-esh-history
     (t . ivy--regex-ignore-order))) ; Default
  :config
  (ivy-mode t)

  ;; Faces
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil :height 136)

  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.
  (require 'counsel nil t)
  )

(use-package swiper
  :bind ("C-s" . counsel-grep-or-swiper) ; Depending on length of file
  )

(use-package counsel
  :bind (("M-x" . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file)
         ("C-x b" . 'counsel-switch-buffer)
         ("C-x b" . 'counsel-switch-buffer)
         ("C-h t" . 'counsel-load-theme) ; Replace help-with-tutorial
         ("C-c g" . 'counsel-git)
         ("C-c j" . 'counsel-git-grep)
         ("C-c k" . 'counsel-ag)
         ("C-x l" . 'counsel-locate)
         ("C-h f" . 'counsel-describe-function)
         ("C-h v" . 'counsel-describe-variable)
         ("C-h o" . 'counsel-describe-symbol)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         )
  :custom
  ;; Remove "^" when entering a ivy completion buffer
  (ivy-initial-inputs-alist nil)
  :config

  (kb/leader-keys
    "ff" '(counsel-find-file :which-key "Find file")
    "fF" '(counsel-file-jump :which-key "Fuzzy find file")
    "fr" '(counsel-recentf :which-key "Recent files")

    "hf" '(counsel-describe-function :which-key "Desc func")
    "hv" '(counsel-describe-variable :which-key "Desc var")
    "ho" '(counsel-describe-symbol :which-key "Desc sym")
    "ht" '(counsel-load-theme :which-key "Change theme")

    "bc" '(counsel-switch-buffer :which-key "Counsel switch")

    "mm" '(counsel-bookmark :which-key "Go to bookmark")
    "ms" '(bookmark-set :which-key "Set bookmark")

    "iy" '(counsel-yank-pop :which-key "Paste")

    "eh" '(counsel-esh-history :which-key "Eshell history") ; I need to figure out how to do mode maps
    )
  )

(use-package counsel-projectile
  :after projectile)

(use-package flx
  :defer t  ; Ivy loads this when needed
  :custom
  (ivy-flx-limit 10000)
  )

(use-package ivy-rich
  :custom
  (ivy-rich-path-style 'abbrev) ; Abbreviate file names
  :config
  (ivy-rich-mode t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )

(use-package amx
  :config
  (amx-mode)
  )

(use-package flx
  :disabled
  :defer t  ; is loaded by ivy
  :custom
  (ivy-flx-limit 10000)
  )

(use-package all-the-icons-ivy-rich
  :after (ivy-rich)
  :custom
  (all-the-icons-ivy-rich-icon-size 0.9) ; The icon size
  ;; Slow Rendering
  ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
  ;; you can try setting the following variable
  (inhibit-compacting-font-caches t)
  :config
  (all-the-icons-ivy-rich-mode t)
  )

(use-package desktop
  :custom
  (desktop-save t) ; Always save when quitting Emacs or changing desktop
  (desktop-path `(,user-emacs-directory))
  (desktop-dirname user-emacs-directory)
  (desktop-base-file-name "emacs-save-desktop")
  (desktop-restore-eager t) ; Lazily restore buffers to reduce startup time
  ;; (desktop-restore-eager 12) ; Lazily restore buffers to reduce startup time
  ;; (desktop-lazy-verbose nil) ; Don't be verbose when lazy loading buffers
  ;; (desktop-lazy-idle-delay 10) ; Wait 10 seconds until lazy loading buffers
  (desktop-auto-save-timeout 20) ; Idle time until save
  (desktop-restore-frames t) ; Restore frames too
  (desktop-load-locked-desktop t) ; Always load so that it is compatible with daemon
  ;; (desktop-restore-reuses-frames 'keep) ; Keep current frames when restoring session
  (history-length 1000) ; This is what Doom uses
  :config
  (desktop-save-mode)

  (setq desktop-buffers-not-to-save ;; Don't save these buffers
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$"))
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

  ;; Remove desktop after it's been read
  (add-hook 'desktop-after-read-hook
            '(lambda ()
               ;; desktop-remove clears desktop-dirname
               (setq desktop-dirname-tmp desktop-dirname)
               (desktop-remove)
               (setq desktop-dirname desktop-dirname-tmp)
               (org-mode-restart)
               ))

  (defun saved-session ()
    (file-exists-p (concat desktop-dirname desktop-base-file-name)))

  ;; Use session-restore to restore the desktop manually
  (defun session-restore ()
    "Restore a saved emacs session."
    (interactive)
    (if (saved-session)
        (desktop-read)
      (message "No desktop found.")))

  ;; Use session-save to save the desktop manually
  (defun kb/prompt-session-save ()
    "Save an emacs session."
    (interactive)
    (if (saved-session)
        (if (y-or-n-p "Overwrite existing desktop? ")
            (desktop-save-in-desktop-dir)
          (message "Session not saved."))
      (desktop-save-in-desktop-dir)
      ))

  (defun kb/auto-session-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save-in-desktop-dir)))
  ;; (add-hook 'after-save-hook 'kb/auto-session-save) ; Save constantly
  (run-with-timer 0 480 'kb/auto-session-save) ; Save every 10 minutes

  ;; Restore when I want it to
  (add-hook 'after-init-hook
            '(lambda ()
               (if (saved-session)
                   ;; (if (y-or-n-p "Restore desktop? ")    )
                   (session-restore)
                 (progn (session-restore)
                        (if (daemonp) (org-mode-restart))) ; Restart org-mode to properly set faces for org files after loading with daemon
                 )))


  ;; Prevent desktop file from being locked on system and
  ;; Emacs crashes
  (defun emacs-process-p (pid)
    "If pid is the process ID of an emacs process, return t, else nil.
            Also returns nil if pid is nil."
    (when pid
      (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
        (when (file-exists-p cmdline-file)
          (with-temp-buffer
            (insert-file-contents-literally cmdline-file)
            (goto-char (point-min))
            (search-forward "emacs" nil t)
            pid)))))

  (defadvice desktop-owner (after pry-from-cold-dead-hands activate)
    "Don't allow dead emacsen to own the desktop file."
    (when (not (emacs-process-p ad-return-value))
      (setq ad-return-value nil)))
  (defun kb/desktop-owner-advice (original &rest args)
    (let ((owner (apply original args)))
      (if (and owner (/= owner (emacs-pid)))
          (and (car (member owner (list-system-processes)))
               (let (cmd (attrlist (process-attributes owner)))
                 (if (not attrlist) owner
                   (dolist (attr attrlist)
                     (and (string= "comm" (car attr))
                          (setq cmd (car attr))))
                   (and cmd (string-match-p "[Ee]macs" cmd) owner))))
        owner)))

  ;; Ensure that dead system processes don't own it.
  (advice-add #'desktop-owner :around #'kb/desktop-owner-advice)
  )

(use-package bufler
  :after (evil evil-collection)
  :config
  (bufler-mode)

  ;; Default bindings are shadowed by Evil mode so remap
  (add-hook 'bufler-list-mode-hook
            (lambda ()
              (define-key evil-motion-state-local-map (kbd "?") 'hydra:bufler/body)
              (define-key evil-motion-state-local-map (kbd "RET") 'bufler-list-buffer-switch)
              (define-key evil-normal-state-local-map (kbd "SPC") 'bufler-list-buffer-peek)
              (define-key evil-motion-state-local-map (kbd "N") 'bufler-list-buffer-name-workspace)
              (define-key evil-normal-state-local-map (kbd "m") 'bufler-mode)
              (define-key evil-normal-state-local-map (kbd "r") 'bufler)
              (define-key evil-normal-state-local-map (kbd "s") 'bufler-list-buffer-save)
              (define-key evil-normal-state-local-map (kbd "K") 'bufler-list-buffer-kill)
              (define-key evil-normal-state-local-map (kbd "f") 'bufler-list-group-frame)
              (define-key evil-normal-state-local-map (kbd "F") 'bufler-list-group-make-frame)
              (define-key evil-normal-state-local-map (kbd "q") 'quit-window)
              ))

  (pretty-hydra-define hydra:bufler
    (:hint t :foreign-keys run :quit-key "?" :exit t)
    ("Bufler"
     (("r" #'bufler "Refresh")
      ("m" #'bufler-mode "Toggle bufler-mode")
      ("q" #'quit-window "Quit"))
     "Buffer"
     (("SPC" #'bufler-list-buffer-peek "Peek at")
      ("RET" #'bufler-list-buffer-switch "Switch to buffer")
      ("K" #'bufler-list-buffer-kill "Kill buffer or group of buffers")
      ("s" #'bufler-list-buffer-save "Save buffer or group of buffers")
      ("N" #'bufler-list-buffer-name-workspace "Add buffers to named workspace (prefix removes)"))
     "Workspace"
     (("f" #'bufler-list-group-frame "Current frame focus")
      ("F" #'bufler-list-group-make-frame "Make frame and set focus"))))

  (kb/leader-keys
    "bb" '(bufler-switch-buffer :which-key "Bufler switch")
    "bs" '(bufler-workspace-frame-set :which-key "Set bufler workspace for this frame")
    "bl" '(bufler-list :which-key "Bufler buffer list")
    )
  )

(use-package eyebrowse
  :after (evil evil-collection)
  :custom
  (eyebrowse-default-workspace-slot 0) ; Start at 0
  (eyebrowse-keymap-prefix (kbd "C-c C-w"))
  (eyebrowse-mode-line-left-delimiter " | ")
  (eyebrowse-mode-line-right-delimiter " | ")
  (eyebrowse-mode-line-separator " ")
  (eyebrowse-tagged-slot-format "%t") ; Only show workspace name (tag) if avail
  (eyebrowse-wrap-around t) ; Not sure waht this does
  :config
  (eyebrowse-mode)

  (evil-define-key '(visual normal motion) 'global (kbd "gt") 'eyebrowse-next-window-config)
  (evil-define-key '(visual normal motion) 'global (kbd "ga") 'eyebrowse-prev-window-config)
  (evil-define-key '(visual normal motion) 'global (kbd "gz") 'eyebrowse-last-window-config)
  (evil-define-key nil eyebrowse-mode-map (kbd "C-c C-w r") 'eyebrowse-rename-window-config)
  (evil-define-key nil eyebrowse-mode-map (kbd "C-c C-w c") 'eyebrowse-close-window-config)
  (global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (global-set-key (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
  (global-set-key (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
  (global-set-key (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
  (global-set-key (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
  (global-set-key (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
  (global-set-key (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
  )

(use-package winner
  :custom
  (winner-dont-bind-my-keys t) ; Don't bind keys because I bind them myself
  :config
  (winner-mode)

  (general-define-key
   "C-<left>" 'winner-undo
   "C-<right>" 'winner-redo
   )
  )

(use-package evil
  :init
  ;; A lot of the settings need to be set before evil inisializes
  (setq evil-want-integration t
        evil-want-keybinding nil ; Add more keybinds for other modes I don't want
        evil-want-C-u-scroll t ; Rebind C-u from universal argument to evil scroll up
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t ; Don't skip lines in visual-line-mode
        evil-want-Y-yank-to-eol t
        evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-undo-system 'undo-fu)
  :config
  (evil-mode t)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'insert)

  (evil-define-key 'insert 'global (kbd "C-g") 'evil-escape)
  (evil-define-key '(normal insert visual) 'global (kbd "C-:") 'evil-jump-forward)
  (evil-define-key '(normal visual) 'global (kbd "zi") 'org-toggle-inline-images)

  (kb/leader-keys
    "ww" 'evil-window-next
    "wc" 'evil-window-delete
    "wo" 'delete-other-windows

    "wL" 'evil-window-move-far-right
    "wH" 'evil-window-move-far-left
    "wJ" 'evil-window-move-very-bottom
    "wK" 'evil-window-move-very-top

    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up

    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split
    )
  )

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init)
  )

(use-package evil-goggles
  :disabled ; Doesn't add anything for me and makes editing feel slower because of the delay
  :after (evil)
  :custom
  (evil-goggles-duration 0.1)
  ;; :init
  ;; (custom-set-faces
  ;;  '(evil-goggles-delete-face ((t (:inherit 'shadow))))
  ;;  '(evil-goggles-paste-face ((t (:inherit 'lazy-highlight))))
  ;;  '(evil-goggles-yank-face ((t (:inherit 'isearch-fail)))))
  :config
  (evil-goggles-mode)

  ;; ;; optionally use diff-mode's faces; as a result, deleted text
  ;; ;; will be highlighed with `diff-removed` face which is typically
  ;; ;; some red color (as defined by the color theme)
  ;; ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces)
  )

(use-package evil-commentary
  :config
  (evil-commentary-mode +1)
  )

(use-package evil-org
  :after (evil evil-collection org)
  :custom
  (org-special-ctrl-a/e t) ; Make ^ and $ ignore tags and leading stars
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar))))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode t)
  )

(use-package evil-visualstar
  :custom
  (evil-visualstar/persistent t) ; Allow visual-mode to remain in affect to allow repeating searches
  :config
  (global-evil-visualstar-mode)
  )

(use-package evil-magit
  :after (evil evil-collection magit)
  )

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'org-mu4e)

;; General settings
(setq mu4e-root-maildir "~/Documents/Emails/Personal")
(setq mu4e-get-mail-command "/usr/bin/mbsync -a") ; Use mbsync to sync mail
(setq mu4e-html2text-command 'mu4e-shr2text) ; Renders emails' html with eww engine
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-change-filenames-when-moving t) ; Prevent duplicate UUIDs of messages
(setq mu4e-view-show-images t) ; Enable inline images
(when (fboundp 'imagemagick-register-types) ; use imagemagick, if available
  (imagemagick-register-types))
(setq mu4e-view-prefer-html nil) ; Don't prefer html
(setq mu4e-update-interval 300) ; Update automatically
(setq mu4e-index-cleanup t) ; Make sure nothing breaks
(setq mu4e-index-update-in-background t) ; Update in background
(setq message-kill-buffer-on-exit t) ; Delete message buffer after sent
(setq mu4e-view-show-addresses nil) ; show names and not addresses ; toggle per name with M-RET
(setq mu4e-headers-include-related nil) ; Do not show related messages by default (toggle with =W= works anyway)
(add-hook 'mu4e-view-mode-hook 'visual-line-mode) ; Automatic linebreaks when reading email
;; (add-hook 'mu4e-main-mode-hook 'display-line-numbers-mode) ; Doesn't work?
;; (setq-hook! 'mu4e-view-mode-hook truncate-lines nil) ; or this way
(setq mu4e-confirm-quit nil) ; Close mu4e without asking
(setq mu4e-completing-read-function #'ivy-completing-read) ; Use ivy for whatever this is
(add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser)) ; Html mails might be better rendered in a browser

;; Add a column to display what email account the email belongs to.
(add-to-list 'mu4e-header-info-custom
             '(:account
               :name "Account"
               :shortname "Account"
               :help "Which account this email belongs to"
               :function
               (lambda (msg)
                 (let ((maildir (mu4e-message-field msg :maildir)))
                   (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))
;; Fields
(setq mu4e-view-fields '(:from :to :cc :bcc :subject :flags :date :maildir :mailing-list :tags :attachments :signature)) ; Customize view fields
(setq  mu4e-headers-fields
       '((:from . 25)
         (:flags . 8)
         (:subject . 83)
         (:human-date . 13)
         (:account . 13)
         ))

;; Use fancy icons
(setq mu4e-use-fancy-chars t
      mu4e-headers-draft-mark '("D" . " ")
      mu4e-headers-flagged-mark '("F" . " ")
      mu4e-headers-new-mark '("N" . " ")
      mu4e-headers-passed-mark '("P" . " ")
      mu4e-headers-replied-mark '("R" . " ")
      mu4e-headers-seen-mark '("S" . " ")
      mu4e-headers-trashed-mark '("T" . " ")
      mu4e-headers-attach-mark '("a" . " ")
      mu4e-headers-encrypted-mark '("x" . " ")
      mu4e-headers-signed-mark '("s" . " ")
      mu4e-headers-unread-mark '("u" . " "))

                                        ; Add bookmarks (fast queries)
(add-to-list 'mu4e-bookmarks
             '( :name "Brown inbox"
                :query "maildir:/Brown/Inbox"
                :key ?b))
(add-to-list 'mu4e-bookmarks
             '( :name "Personal inbox"
                :query "maildir:/Personal/Inbox"
                :key ?h))

                                        ; Shortcuts
(setq mu4e-maildir-shortcuts
      '((:maildir "/Personal/All-Mail" :key ?A)
        (:maildir "/Personal/Sent"   :key ?S)
        (:maildir "/Personal/Trash"    :key ?T)
        (:maildir "/Personal/Inbox"    :key ?I)
        (:maildir "/Personal/Drafts"    :key ?D)
        (:maildir "/Personal/Starred"    :key ?L)
        (:maildir "/Brown/All-Mail" :key ?a)
        (:maildir "/Brown/Sent"   :key ?s)
        (:maildir "/Brown/Trash"    :key ?t)
        (:maildir "/Brown/Inbox"    :key ?i)
        (:maildir "/Brown/Drafts"    :key ?d)
        (:maildir "/Brown/Starred"    :key ?l)
        ))

(setq
 mu4e-sent-folder   "/Personal/Sent"       ;; folder for sent messages
 mu4e-drafts-folder "/Personal/Drafts"     ;; unfinished messages
 mu4e-trash-folder  "/Personal/Trash"      ;; trashed messages
 mu4e-refile-folder "/Personal/Archive")   ;; saved messages

(setq mu4e-context-policy 'pick-first) ; Choose default/first context (Brown)
(setq mu4e-compose-context-policy 'ask-if-none) ; Choose context you're already in for composing messages
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Brown"
           :enter-func (lambda () (mu4e-message "Entering Brown context"))
           ;; :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
           ;; we match based on the maildir of the message
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/Brown" (mu4e-message-field msg :maildir))))
           :vars '( ( user-mail-address . "kristoffer_balintona@brown.edu"  )
                    ( smtpmail-smtp-user . "kristoffer_balintona@brown.edu")
                    ( smtpmail-smtp-server . "smtp.gmail.com" )
                    ( user-full-name . "Kristoffer Balintona" )
                    ( mu4e-trash-folder . "/Brown/Trash/" )
                    ( mu4e-refile-folder . "/Brown/All-Mail/" )
                    ( mu4e-drafts-folder . "/Brown/Drafts/" )
                    ( mu4e-attachment-dir . "~/Documents/Emails/Brown/Attachments/" )
                    ( mu4e-compose-signature .
                      (concat
                       "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\n"
                       "Kind regards, \n"
                       "Kristoffer \n"))))
         ,(make-mu4e-context
           :name "Personal"
           :enter-func (lambda () (mu4e-message "Entering the personal context"))
           ;; :leave-func (lambda () (mu4e-message "Leaving Outlook context"))
           ;; we match based on the maildir of the message
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/Personal" (mu4e-message-field msg :maildir))))
           :vars '( ( user-mail-address . "krisbalintona@gmail.com" )
                    ( smtpmail-smtp-user . "krisbalintona@gmail.com" )
                    ( smtpmail-smtp-server . "smtp.gmail.com" )
                    ( user-full-name . "Kristoffer Balintona" )
                    ( mu4e-trash-folder . "/Personal/Trash/" )
                    ( mu4e-refile-folder . "/Personal/All-Mail/" )
                    ( mu4e-drafts-folder . "/Personal/Drafts/" )
                    ( mu4e-attachment-dir . "~/Documents/Emails/Personal/Attachments/" )
                    ( mu4e-compose-signature  .
                      (concat
                       "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\n"
                       "Kind regards, \n"
                       "Kristoffer \n"))))))

                                        ; Msmtp for sending emails
(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-stream-type  'starttls)
(setq user-full-name "Joe")
(setq smtpmail-default-smtp-server "smtp.gmail.com")
(setq smtpmail-local-domain "gmail.com")
(setq smtpmail-smtp-service '587)
(setq sendmail-program "/usr/bin/msmtp")
(setq mu4e-hide-index-messages t) ; Hide indexing message in minibuffer
(setq mu4e-compose-dont-reply-to-self t) ; Don't reply to myself

(setq mu4e-compose-format-flowed t) ; Something about linu continuation?
;; Some email clients ignore format=flowed (i.e. Outlook). Therefore,
;; we send very long lines, so that they auto-flow. 998 chars are the
;; actual maximum from the relevant RFC:
;; https://www.ietf.org/rfc/rfc2822.txt
(setq fill-flowed-encode-column 998)

(setq message-sendmail-extra-arguments '("--read-envelope-from")) ; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-f-is-evil 't)
(setq mu4e-sent-messages-behavior 'delete) ; don't save messages to Sent Messages, Gmail/IMAP takes care of this

;; I don't think I need this since I set contexts already when I go into mu4e
;; ;; Whenever a new mail is to be composed, change all relevant
;; ;; configuration variables to the respective account. This method is
;; ;; taken from the MU4E documentation:
;; ;; http://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html#Multiple-accounts
;; (defun my-mu4e-set-account ()
;;   "Set the account for composing a message."
;;   (let* ((account
;;           (if mu4e-compose-parent-message
;;               (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;                 (string-match "/\\(.*?\\)/" maildir)
;;                 (match-string 1 maildir))
;;             (completing-read (format "Compose with account: (%s) "
;;                                      (mapconcat #'(lambda (var) (car var))
;;                                                 my-mu4e-account-alist "/"))
;;                              (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
;;                              nil t nil nil (caar my-mu4e-account-alist))))
;;          (account-vars (cdr (assoc account my-mu4e-account-alist))))
;;     (if account-vars
;;         (mapc #'(lambda (var)
;;                   (set (car var) (cadr var)))
;;               account-vars)
;;       (error "No email account found"))))
;; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
(add-hook 'mu4e-compose-mode-hook 'visual-line-mode)

                                        ; Queuing mail
(setq smtpmail-queue-mail  't
      smtpmail-queue-dir  "~/Documents/Emails/Queue/cur")

;; This hook correctly modifies gmail flags on emails when they are marked.
;; Without it, refiling (archiving), trashing, and flagging (starring) email
;; won't properly result in the corresponding gmail action, since the marks
;; are ineffectual otherwise.
(add-hook 'mu4e-mark-execute-pre-hook
          (defun +mu4e-gmail-fix-flags-h (mark msg)
            (pcase mark
              (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
              (`refile (mu4e-action-retag-message msg "-\\Inbox"))
              (`flag   (mu4e-action-retag-message msg "+\\Starred"))
              (`unflag (mu4e-action-retag-message msg "-\\Starred")))))

(kb/leader-keys
  "om" '(mu4e :which-key "Email"))

(set-face-attribute 'mu4e-header-face nil :height 140 :font "FiraCode Nerd Font") ; Non-colored header items
(set-face-attribute 'mu4e-header-highlight-face nil :height 140 :font "FiraCode Nerd Font") ; Current item
(set-face-attribute 'mu4e-flagged-face nil :height 140 :font "FiraCode Nerd Font")
(set-face-attribute 'mu4e-unread-face nil :height 140 :font "FiraCode Nerd Font") ; Unread (pink) items

(use-package mu4e-alert
  :after doom-modeline
  :custom
  (mu4e-alert-interesting-mail-query (concat
                                      "flag:unread"
                                      " AND NOT flag:trashed"
                                      " AND NOT maildir:\"/Personal/All-Mail\""
                                      " AND NOT maildir:\"/Brown/All-Mail\""
                                      " AND NOT maildir:\"/Queue/\""))
  (mu4e-alert-email-notification-types '(subjects))

  (display-time-use-mail-icon t)
  (display-time-mail-icon "📬") ; Non-customizable

  ;; (display-time-use-mail-icon t) ; Not sure what this does
  ;; (display-time-mail-icon (image :type png :file "~/Pictures/gmail.png" :ascent center))
  :config
  ;; Format how it appears in the modeline
  (defun mu4e-alert-default-mode-line-formatter (mail-count)
    "Default formatter used to get the string to be displayed in the mode-line.
       MAIL-COUNT is the count of mails for which the string is to displayed"
    (when (not (zerop mail-count))
      (concat "  "
              (if (zerop mail-count)
                  " "
                (format "  %d " mail-count))
              (propertize
               "Mail"
               'display (when (display-graphic-p)
                          display-time-mail-icon)
               'face display-time-mail-face
               'help-echo (concat (if (= mail-count 1)
                                      "You have an unread email"
                                    (format "You have %s unread emails" mail-count))
                                  "\nClick here to view "
                                  (if (= mail-count 1) "it" "them"))
               'mouse-face 'mode-line-highlight
               'keymap '(mode-line keymap
                                   (mouse-1 . mu4e-alert-view-unread-mails)
                                   (mouse-2 . mu4e-alert-view-unread-mails)
                                   (mouse-3 . mu4e-alert-view-unread-mails)))
              " " ; Padding
              )))

  ;; Unread emails in modeline
  (mu4e-alert-set-default-style 'libnotify)

  (add-hook 'doom-modeline-mode-hook #'mu4e-alert-enable-mode-line-display)
  (add-hook 'doom-modeline-mode-hook #'mu4e-alert-enable-notifications)
  ;; For combatibility with emacsclient
  (add-hook 'server-after-make-frame-hook #'mu4e-alert-enable-mode-line-display)
  (add-hook 'server-after-make-frame-hook #'mu4e-alert-enable-notifications)
  )

(use-package org-msg
  :custom
  (org-msg-startup "inlineimages")
  (org-msg-greeting-name-limit 3)
  (org-msg-text-plain-alternative t)
  )

(use-package calfw) ; Requisite framework

(use-package calfw-org
  :custom
  (cfw:org-overwrite-default-keybinding t)
  ;; Month
  (calendar-month-name-array
   ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

  ;; Week days
  (calendar-day-name-array
   ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])

  ;; First day of the week
  (calendar-week-start-day 1) ; 0:Sunday, 1:Monday

  :config
  ;; Calendar views
  (defun kb/calendar-school ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")  ; orgmode source
      ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
      )))

  ;; Grid frame
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)

  ;; Calendar faces
  (custom-set-faces
   '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
   '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
   '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
   '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
   '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
   '(cfw:face-grid ((t :foreground "DarkGrey")))
   '(cfw:face-default-content ((t :foreground "#bfebbf")))
   '(cfw:face-periods ((t :foreground "cyan")))
   '(cfw:face-day-title ((t :background "grey10")))
   '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
   '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
   '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
   '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
   '(cfw:face-today ((t :background: "grey10" :weight bold)))
   '(cfw:face-select ((t :background "#2f2f2f")))
   '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
   '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
   '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold))))
  )

(use-package org-gcal
  :disabled
  :custom
  (org-gcal-client-id "your-id-foo.apps.googleusercontent.com")
  (org-gcal-client-secret "your-secret")
  (org-gcal-file-alist '(("your-mail@gmail.com" .  "~/schedule.org")
                         ("another-mail@gmail.com" .  "~/task.org")))
  )

;; (load-file "~/testing-emacs/elpa/org-plus-contrib-20201019/org.el") ;; (require 'org-plus-contrib-20201019)
;; (use-package org-plus-contrib
;;   :ensure t
;;   :pin org)

;; a kludge to solve multiple org versions conundrum:
(assq-delete-all 'org package--builtins)

(use-package org
  :hook (org-mode . kb/org-visual-setup)
  :custom
  ;; (org-ellipsis " ⛛") ; Specifically for org-mode headlines ; Doesn't work?
  (org-startup-indented t)
  (org-startup-folded 'nofold)
  (org-pretty-entities t) ; Show as UTF-8 characters (useful for math)
  (org-hide-emphasis-markers t) ; Remove org-mode markup characters
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t) ; Have these blocks look pretty
  (org-pretty-entities t) ; Prettify things
  (org-pretty-entities-include-sub-superscripts nil) ; Don't show super- and sunbscripts

  (org-src-window-setup 'current-window) ; Open src block window on current buffer
  :preface
  (defun kb/org-face-setup ()
    "Taken from https://github.com/daviwil/emacs-from-scratch/blob/1a13fcf0dd6afb41fce71bf93c5571931999fed8/init.el"

    ;; Ensure that everything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch :background "#232635" :extend t) ; Set background of org-blocks and extend to the end of the line
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-quote nil :height 143)
    (set-face-attribute 'org-tag nil :height 153 :bold t :italic t)
    (set-face-attribute 'org-link nil :foreground "MediumPurple3" :bold nil :italic t :font "Noto Sans" :height 145 :underline nil)
    (set-face-attribute 'org-document-title nil :inherit 'org-level-4 :height 1.7 :foreground "goldenrod")

    (set-face-attribute 'org-level-1 nil :height 210 :font "Noto Sans")
    (set-face-attribute 'org-level-2 nil :height 198 :font "Noto Sans")
    (set-face-attribute 'org-level-3 nil :height 185 :font "Noto Sans")
    (set-face-attribute 'org-level-4 nil :height 170 :foreground "medium aquamarine" :font "Noto Sans")
    (set-face-attribute 'org-level-5 nil :height 165 :foreground "light sea green" :font "Noto Sans")
    )

  (defun kb/org-visual-setup () ; Modes only for org-mode
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1)
    )

  :config
  (if (daemonp) ; Add hook based on if daemon or not
      (add-hook 'server-after-make-frame-hook 'kb/org-face-setup)
    (add-hook 'after-init-hook 'kb/org-face-setup))

  (add-hook 'desktop-after-read-hook 'kb/org-face-setup)

  (kb/leader-keys
    "nn" '(org-capture :which-key "Org-capture")
    )
  )

(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       ;; Position determines where org-insert-todo-heading and org-insert-item
       ;; insert the new list item.
       (if (eq direction 'above)
           (org-beginning-of-item)
         (org-end-of-item)
         (backward-char))
       (org-insert-item (org-element-property :checkbox context))
       ;; Handle edge case where current item is empty and bottom of list is
       ;; flush against a new heading.
       (when (and (eq direction 'below)
                  (eq (org-element-property :contents-begin context)
                      (org-element-property :contents-end context)))
         (org-end-of-item)
         (org-end-of-line)))

      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))


(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

(define-key org-mode-map (kbd "<C-return>") '+org/insert-item-below)
(evil-define-key '(normal visual) 'global (kbd "<C-return>") '+org/insert-item-below)

(defun +org--toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree."
  (let ((beg (or beg
                 (if (org-before-first-heading-p)
                     (line-beginning-position)
                   (save-excursion (org-back-to-heading) (point)))))
        (end (or end
                 (if (org-before-first-heading-p)
                     (line-end-position)
                   (save-excursion (org-end-of-subtree) (point)))))
        (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                    (ignore-errors (overlays-in beg end)))))
    (dolist (ov overlays nil)
      (delete-overlay ov)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))

  (defun +org/dwim-at-point (&optional arg)
    "Do-what-I-mean at point.

  If on a:
  - checkbox list item or todo heading: toggle it.
  - clock: update its time.
  - headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
    subtree; update statistics cookies/checkboxes and ToCs.
  - footnote reference: jump to the footnote's definition
  - footnote definition: jump to the first reference of this footnote
  - table-row or a TBLFM: recalculate the table's formulas
  - table-cell: clear it and go into insert mode. If this is a formula cell,
    recaluclate it instead.
  - babel-call: execute the source block
  - statistics-cookie: update it.
  - latex fragment: toggle it.
  - link: follow it
  - otherwise, refresh all inline images in current tree."
    (interactive "P")
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context)))))))


  (define-key evil-motion-state-map (kbd "RET") '+org/dwim-at-point)

(use-package org-superstar  ;; Improved version of org-bullets
  :hook ((prog-mode . (lambda () (visual-line-mode 0)))
         (org-mode . (lambda () (org-superstar-mode t))))
  :custom
  (org-cycle-level-faces nil)
  (org-n-level-faces 5)

  (org-superstar-leading-bullet ?\s) ;; Render leading stars as spaces!
  (org-superstar-leading-fallback ?\s) ; Hide away leading stars on terminal.
  (org-hide-leading-stars t) ;; t means no character is there at all
  (org-superstar-cycle-headline-bullets nil) ; Don't repeat bullets in heirarchy
  (org-superstar-todo-bullet-alist
   '(("TODO" . 9744)
     ("[ ]"  . 9744)
     ("DONE" . 9745)
     ("[X]"  . 9745)))
  (org-superstar-headline-bullets-list
   '("⚝" "●" "◉" "○"))
  (org-superstar-first-inlinetask-bullet ?▶)
  ;; org-superstar-item-bullet-alist ; For plainlist bullets

  (org-hidden-keywords '(title)) ; hide #+TITLE:
  (inhibit-compacting-font-caches t) ; Stop slowdown
  )

(use-package org-roam
  :quelpa (org-roam :fetcher git :url "https://github.com/org-roam/org-roam" :branch "master")
  :after company ; Necessary for some reason
  :custom
  (org-roam-directory kb/roam-dir)
  (org-roam-verbose nil) ; Don't echo messages that aren't errors
  (org-roam-completion-system 'ivy)
  (org-roam-completion-everywhere t) ; Org-roam completion everywhere
  (org-roam-link-auto-replace t) ; Replace roam link type with file link type when possible
  (org-roam-buffer-window-parameters '((no-other-window . t)
                                       (no-delete-other-windows . t)
                                       ))
  (org-roam-db-gc-threshold most-positive-fixnum) ; Temporarily increase GC threshold during intensive org-roam operations
  (org-roam-index-file "index.org") ; My Index
  (org-roam-tag-separator " ")
  (org-roam-link-use-custom-faces 'everywhere) ; Use org-roam-link face everywhere (including org-roam-buffer)
  ;; Roam-buffer specific changes
  (org-roam-graph-extra-config '(("rankdir" . "LR"))) ; Extra options passed to graphviz
  (org-roam-buffer-prepare-hook
   '(org-roam-buffer--insert-title
     org-roam-buffer--insert-backlinks
     org-roam-buffer--insert-ref-links))
  (org-roam-buffer-width 0.20)
  :config
  (org-roam-mode)

  (add-to-list 'org-open-link-functions 'org-roam--open-fuzzy-link)
  (add-to-list 'org-open-at-point-functions 'org-roam-open-id-at-point)

  ;; Overall faces
  (set-face-attribute 'org-roam-link nil :inherit 'org-link :italic nil :foreground "goldenrod3")

  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode) ; Hide modeline in org-roam buffer
  (add-hook 'org-roam-buffer-prepare-hook
            (lambda ()
              (face-remap-add-relative 'default :height 109)
              (face-remap-add-relative 'org-document-title :height 145 :foreground "DarkOrange3")
              (face-remap-add-relative 'org-roam-link :height 112 :slant 'normal)
              (face-remap-add-relative 'org-level-1 :height 140)
              (face-remap-add-relative 'org-level-2 :height 117)
              (face-remap-add-relative 'org-level-3 :height 114)
              ))

  (kb/leader-keys
    "nb" '(org-roam-switch-to-buffer :which-key "Switch to buffer")
    "nf" '(org-roam-find-file :which-key "Find file")
    "ng" '(org-roam-graph :which-key "Show graph")
    "ni" '(org-roam-insert :which-key "Insert note")
    "nI" '(org-roam-jump-to-index :which-key "Go to index")
    ;; "nI" '(org-roam-insert-immediate :which-key "Insert now") ; Calls org-roam-capture-immediate-template
    "nl" '(org-roam :which-key "Toggle Roam buffer")
    "nL" '(org-roam-db-build-cache :which-key "Rebuild cache")
    "nc" '(org-roam-capture :which-key "Roam capture")

    "nD" '(:ignore t :which-key "Call the doctor")
    "nDt" '(org-roam-doctor :which-key "Doctor this file")
    "nDa" '((lambda () ; Call org-roam-doctor with universal argument (C-u)
              (interactive)
              (let ((current-prefix-arg 4))
                (call-interactively 'org-roam-doctor)
                ))
            :which-key "Doctor all files")

    ;; "nd" '(:ignore t :which-key "Roam dailies")
    ;; "ndd" '(org-roam-dailies-date :which-key "Choose date")
    ;; "ndt" '(org-roam-dailies-today :which-key "Today")
    ;; "ndm" '(org-roam-dailies-tomorrow :which-key "Tomorrow")
    ;; "ndy" '(org-roam-dailies-yesterday :which-key "Yesterday")
    )
  )

;; (defun org-roam--replace-link (old-path new-path &optional old-desc new-desc)
;;   "Replace Org-roam file links with path OLD-PATH to path NEW-PATH.
;;     If OLD-DESC is passed, and is not the same as the link
;;     description, it is assumed that the user has modified the
;;     description, and the description will not be updated. Else,
;;     update with NEW-DESC."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward org-link-any-re nil t)
;;       (when-let ((link (org-element-lineage (org-element-context) '(link) t)))
;;         (let ((type (org-element-property :type link))
;;               (path (org-element-property :path link)))
;;           (when (and (string-equal (expand-file-name path) old-path)
;;                      (org-in-regexp org-link-bracket-re 1))
;;             (let* ((label (if (match-end 2)
;;                               (match-string-no-properties 2)
;;                             (org-link-unescape (match-string-no-properties 1))))
;;                    (new-label (if (string-equal label old-desc)
;;                                   new-desc
;;                                 label)))
;;               (replace-match
;;                (save-match-data (org-roam-format-link new-path new-label type))))))))))

(use-package org-roam-server
  :quelpa (org-roam-server :fetcher git :url "https://github.com/org-roam/org-roam-server")
  :after org-roam
  :custom
  (org-roam-server-host "127.0.0.1")
  (org-roam-server-port 8080)
  (org-roam-server-authenticate nil)
  (org-roam-server-export-inline-images t)
  (org-roam-server-serve-files nil)
  (org-roam-server-served-file-extensions '("pdf" "mp4" "ogv"))
  (org-roam-server-network-poll t)
  (org-roam-server-network-label-truncate t)
  (org-roam-server-network-label-truncate-length 60)
  (org-roam-server-network-label-wrap-length 30)
  (org-roam-graph-viewer "/usr/bin/brave")
  (org-roam-graph-executable "/usr/bin/dot") ; Graphviz executable
  (org-roam-server-network-arrows nil) ; Arrows?
  ;; (org-roam-server-default-exclude-filters [{ "id": "moc", "parent" : "tags"  }]) ; Default exclusion
  (org-roam-server-default-include-filters "null") ; Default inclusion
  :config
  (if (daemonp)
      (add-hook 'org-roam-mode-hook 'org-roam-server-mode)) ; Don't start server if not in daemo mode, otherwise will break things
  )

(setq org-roam-capture-templates
      '(("l" "New literature note" plain (function org-roam--capture-get-point)
         "\n\n* %?"
         :file-name "%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil)"
         :head "#+TITLE: ${title}\n#+ROAM_TAGS: lit\n- CREATED :: %T\n- Time-stamp: <>\n- SOURCE :: \n- TAGS :: \n- LINKS :: \n\n---\n\n* TODO Process ${title} :WAITING:\n:PROPERTIES:\n:CATEGORY: lit\n:ARCHIVE: %(concat kb/agenda-dir \"archive.org::datetree/* Lit Notes\")\n:END:\n"
         :jump-to-captured t
         :immediate-finish t
         :unnarrowed t)
        ("p" "New permanent note" plain (function org-roam--capture-get-point)
         "\n\n* Insight%?"
         :file-name "%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil)"
         :head "#+TITLE: ${title}\n#+ROAM_TAGS: zett\n- CREATED :: %T\n- Time-stamp: <>\n- SOURCE :: \n- MOC :: \n- TAGS :: \n- LINKS :: \n\n---\n\n* TODO Process ${title} :NASCENT:\n:PROPERTIES:\n:CATEGORY: zett\n:ARCHIVE: %(concat kb/agenda-dir \"archive.org::datetree/* Zetts\")\n:END:"
         :unnarrowed t
         :immediate-finish t
         :jump-to-captured t)
        ("m" "New map of content" plain (function org-roam--capture-get-point)
         "\n\n* %?"
         :file-name "%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil)"
         :head "#+TITLE: ${title}\n#+ROAM_TAGS: moc\n- TAGS :: \n- BREADCRUMBS :: \n\n---"
         :immediate-finish t
         :unnarrowed t)
        ("i" "New quote" plain (function org-roam--capture-get-point)
         "\n\n* %?"
         :file-name "%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil)"
         :head "#+TITLE: ${title}\n#+ROAM_TAGS: quote\n- CREATED :: %T\n- Time-stamp: <>\n- SOURCE :: \n- TAGS :: \n- LINKS :: \n\n---"
         :unnarrowed t
         :jump-to-captured t)
        ("e" "New entity" plain (function org-roam--capture-get-point)
         "\n\n* %?"
         :file-name "%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil)"
         :head "#+TITLE: ${title}\n#+ROAM_TAGS: %^{What type of entity is this?|person|system|website|platform|organization}\n- CREATED :: %T\n- Time-stamp: <>\n- TAGS :: \n- LINKS :: \n\n---"
         :immediate-finish t)
        ("t" "New tag" plain (function org-roam--capture-get-point)
         "\n\n* %?"
         :file-name "%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil)"
         :head "#+TITLE: ${title}\n#+ROAM_TAGS: tag\n- CREATED :: %T\n- Time-stamp: <>\n\n---"
         :immediate-finish t)
        ("w" "Produce a work of writing" plain (function org-roam--capture-get-point)
         "\n\n* %?"
         :file-name "%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil)"
         :head "#+TITLE: ${title}\n#+ROAM_TAGS: product\n- CREATED :: %T\n- Time-stamp: <>\n- MOC :: \n\n---"
         :immediate-finish t
         :jump-to-captured t)
        ))

(use-package deft
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory kb/roam-dir)
  (deft-use-filename-as-title nil) ; Use title not file name
  (deft-auto-save-interval -1.0) ; disable auto-save
  ;; converts the filter string into a readable file-name using kebab-case:
  (deft-file-naming-rules
    '((noslash . "-")
      (nospace . "-")
      (case-fn . downcase)))
  :config

  (kb/leader-keys
    "nd" '(deft :which-key "Deft")
    )
  )

(use-package ivy-bibtex
  :after org-roam
  :custom
  (bibtex-completion-notes-path kb/roam-dir) ; Irrelevant since I use org-roam-bibtex instead
  (bibtex-completion-library-path (concat kb/roam-dir "bibliographic/bib-pdfs")) ; Where bibtex searches for pdfs
  (bibtex-completion-bibliography (concat kb/roam-dir "bibliographic/master-lib.bib"))
  (bibtex-completion-pdf-field "file") ; Zotero stores pdfs in a field called file - this settings allows bibtex to find the pdf
  (bibtex-completion-pdf-open-function ; Use okular to open a pdf
   (lambda (fpath)
     (call-process "okular" nil 0 nil fpath)))
  (bibtex-completion-browser-function 'browse-url-default-browser) ; Use default browser to open
  (ivy-bibtex-default-action 'ivy-bibtex-edit-notes) ; Edit notes on defualt selection

  ;; Template for new note (but I use orb for this)
  (bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* TODO Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n")
   )

  ;; Symbols used for indicating the availability of notes and PDF files
  (bibtex-completion-pdf-symbol "🖇")
  (bibtex-completion-notes-symbol "🖋")
  :config
  ;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` which I already
  ;; have set in ivy-re-builders-alist
  (autoload 'ivy-bibtex "ivy-bibtex" "" t)

  (ivy-set-actions ; Actions shown after M-o
   'ivy-bibtex
   '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI")
     ("e" ivy-bibtex-edit-notes "Edit notes")
     ("c" ivy-bibtex-insert-citation "Insert citation")
     ("r" ivy-bibtex-insert-reference "Insert reference")
     ("P" ivy-bibtex-open-annotated-pdf "Open annotated PDF (if present)") ; This last function doesn't have an associated action yet (for annotated pdfs)
     ("a" bibtex-completion-add-pdf-to-library "Add pdf to library")
     ))

  (kb/leader-keys
    "fa" '(ivy-bibtex :which-key "Ivy-bibtex")
    "fA" '(ivy-bibtex-with-notes :which-key "Ivy-bibtex only notes")
    )
  )

(use-package org-roam-bibtex
  :after (org-roam ivy-bibtex)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map ; Within files that have #+ROAM_KEY
              (("C-c n a" . orb-note-actions)))
  :custom
  (orb-preformat-keywords
   '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))
  (orb-templates
   '(("n" "Reference paired with notes" plain (function org-roam-capture--get-point)
      ""
      :file-name "%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil)-${slug}"
      :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: bib_notes\n- CREATED :: %T\n- Time-stamp: <>\n- KEYWORDS :: ${keywords}\n- TAGS :: \n- LINKS :: \n\n---\n\n* TODO Process ${title} :WAITING:\n:PROPERTIES:\n:CATEGORY: bib_notes\n:ARCHIVE: %(concat kb/agenda-dir \"archive.org::datetree/* Bib Notes\")\n:END:\n\n* ${title} Notes\n:PROPERTIES:\n:Custom_ID: ${citekey}\n:URL: ${url}\n:AUTHOR: ${author-or-editor}\n:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")\n:NOTER_PAGE:\n:END:\n\n"
      :unnarrowed t)

     ;; ("r" "Plain reference" plain (function org-roam-capture--get-point)
     ;;  ""
     ;;  :file-name "%(format-time-string \"%b%d%Y-%H%M\" (current-time) nil)-${slug}"
     ;;  :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n- CREATED :: %T\n- Time-stamp: <>\n- KEYWORDS :: ${keywords}\n- TAGS :: \n- LINKS :: \n\n---\n\n* ${title} Note\n:PROPERTIES\n:Custom_ID: ${citekey\n:URL: ${url\n:AUTHOR: ${author-or-editor\n:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\"\n:NOTER_PAGE\n:END:\n\n"
     ;;  :unnarrowed t)
     ))
  :config
  (kb/leader-keys
    "nBs" '(orb-find-non-ref-file :which-key "Search non-bibliographic Roam notes")
    "nBi" '(orb-insert-non-ref :which-key "Insert non-bibliographic Roam note")
    "nBa" '(orb-note-actions :which-key "Orb actions")
    )
  )

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-width)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  :config
  ;; (evil-define-key 'normal 'pdf-view-mode-map (kbd "q") #'kill-current-buffer)
  (define-key pdf-view-mode-map (kbd "q") #'kill-current-buffer)
  )

(use-package org-noter
  :demand t ; Demand so it doesn't defer to noter insert call
  :custom
  (org-noter-notes-search-path kb/roam-dir)
  (org-noter-separate-notes-from-heading t) ; Add blank line betwwen note heading and content
  (org-noter-notes-window-location 'horizontal-split) ; Horizontal split between notes and pdf
  (org-noter-always-create-frame nil) ; Don't open frame
  (org-noter-hide-other nil) ; Show notes that aren't synced with (you're on)
  (org-noter-auto-save-last-location t) ; Go to last location
  :config
  (define-key org-noter-doc-mode-map (kbd "M-o") 'org-noter-insert-note)

  (kb/leader-keys
    "on" '(org-noter :which-key "Org-noter"))
  )

(use-package org-ref
  :custom
  (org-ref-notes-directory kb/roam-dir) ; Same directory as org-roam
  (org-ref-bibliography-notes (concat kb/roam-dir "bibliographic/bib-notes.org")) ; Irrelevant for me - I have it here just in case
  (org-ref-pdf-directory (concat kb/roam-dir "bibliographic/bib-pdfs/"))
  (org-ref-default-bibliography (concat kb/roam-dir "bibliographic/master-lib.bib"))
  (org-ref-completion-library 'org-ref-ivy-cite) ; Use ivy
  (org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n")
  (org-ref-notes-function 'orb-edit-notes)
  )

(require 'org-protocol)

(require 'org-agenda)

(setq org-agenda-files kb/all-org-dir-files)

(setq org-agenda-start-with-log-mode t ; Show progression of done and clocked tasks in grid view
      org-agenda-log-mode-items '(closed clock) ; Things which should be added to grid view in log mode (turned on above)
      org-log-done 'time ; When done add CLOSED line with inactive timestamp
      org-log-into-drawer t) ; But everything into a drawer as opposed to appending it

(setq org-refile-targets (quote (;; (nil :maxlevel . 9)
                                        ; Consider headlines in the current buffer
                                 (kb/all-agenda-dir-files-minus-inbox :maxlevel . 1)))) ; Only first-level headlines
(setq org-refile-use-outline-path 'file) ; Show file name while refiling
(setq org-outline-path-complete-in-steps nil) ; Don't have consequtive promps for paths and headings
(setq org-refile-allow-creating-parent-nodes "confirm")

;; ; Exclude DONE state tasks from refile targets
;; (defun kb/verify-refile-target ()
;;   "Remove todos in done states from possible refile target"
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))
;; (setq org-refile-target-verify-function 'kb/verify-refile-target)

;; Custom org-reverse-datetree refile function
(defun kb/org-refile-to-reverse-datetree-archive (); (arg)
  "Refile a todo into my archive file, with today as the day"
  (interactive "P")
  (org-reverse-datetree-refile-to-file
   kb/agenda-dir "archive.org" (current-time)))
;; :ask-always arg :prefer '("CREATED_TIME" "CREATED_AT" "CLOSED")))

;; Save all buffers after refiling to avoid losing progress
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Causes freezing on refile and changing priority for parent todos
(setq org-enforce-todo-dependencies nil)
(setq org-enforce-todo-checkbox-dependencies nil)
(setq org-agenda-dim-blocked-tasks t)

                                        ; Classic org-agenda archiving
(setq org-archive-location (concat kb/agenda-dir "archive.org::"))

;; Change todo states with S-left and S-right skipping all of the
;; normal processing when entering or leaving a todo state. This
;; cycles through the todo states but skips setting timestamps and
;; entering notes which is very convenient when all you want to do is
;; fix up the status of an entry.
(setq org-use-fast-todo-selection 'auto)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d!/@)")
        (sequence "HOLD(h!)" "SOMEDAY(s)" "INACTIVE(i@)" "|" "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "orange" :underline t)
        ("NEXT" :foreground "orchid" :weight bold)
        ("PROG" :foreground "turquoise" :underline t)
        ("DONE" :foreground "chartreuse" :weight normal)
        ("HOLD" :foreground "LightGoldenrod1" :weight normal :underline t)
        ("SOMEDAY" :foreground aquamarine :weight normal)
        ("CANCELLED" :foreground "deep pink" :weight normal)
        ("INACTIVE" :foreground "light slate blue" :weight normal)
        ))

                                        ; Priorities
;; (setq org-priority-faces ; Faces set by org-fancy-priorities
;;       '((65 :foreground "#e45649")
;;         (66 :foreground "#da8548")
;;         (67 :foreground "#0098dd")))
(setq org-priority-highest ?A
      org-priority-lowest ?F
      org-priority-default ?D) ; This needs to be defined due to a bug which uses the old variable names (these) instead of the new ones (the following)
(setq org-highest-priority ?A
      org-lowest-priority ?F
      org-default-priority ?D)

                                        ; Add or remove tags as you change the checkbox state
;; (setq org-todo-state-tags-triggers
;;       '(("TODO" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("NEXT" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("DONE" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("HOLD" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("CANCELLED" ("TODO") ("NEXT") ("DONE") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))
;;         ("INACTIVE" ("NEXT") ("HOLD") ("SOMEDAY") ("CANCELLED") ("INACTIVE"))))

;; ; Automatically change todo keyword to DONE when all children are complete
;; (defun kb/org-summary-todo (n-done n-not-done)
;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;   (let (org-log-done org-log-states)   ; turn off logging
;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
;; (add-hook 'org-after-todo-statistics-hook 'kb/org-summary-todo)

;; Remove empty LOGBOOK drawers on clock out
(defun kb/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'kb/remove-empty-drawer-on-clock-out 'append)

;; Window setup
(setq org-agenda-window-setup 'current-window) ; Open agenda in current window
(setq org-indirect-buffer-display 'current-window) ; Put indirect buffers right on top of the current window

(add-hook 'org-agenda-finalize-hook 'evil-goto-first-line) ; Start at first line in org-agenda

;; Custom bulk mark functions
(setq org-agenda-bulk-custom-functions
      '((?P org-agenda-priority)
        (?R kb/org-agenda-process-inbox-item)
        ))

;; Columns
(setq org-tags-column -180)
;; (setq org-agenda-tags-column -208)
;; (add-hook 'after-focus-change-function
;;           (lambda () (progn
;;                        (setq org-tags-column -80)
;;                        (org-align-tags t))))

;; Schedule and item format settings
(setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      ;; org-agenda-include-deadlines t
      org-agenda-block-separator 61
      org-agenda-tags-column 'auto
      org-cycle-separator-lines 0) ; Remove blank lines when folding
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo . " %i %-13:c   %-6e %?s %t |%l")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")
        ))
;; org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%a, %b %e - %H:%M>") ; For org-agenda timestamps, default is '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")

;; Stuck projects
(setq org-stuck-projects
      '("*/!-HOLD-SOMEDAY" ; Tags/todo/property matcher that identifies which tasks are projects
        ("PROG" "NEXT") ; Todo keywords for non-stuck projects
        nil ; Tags for non-stuck projects
        "" ; Any regexp for non-stuck projects
        ))

;; Function to quicky set effort, priority, tags, and refile.
;; From https://blog.jethro.dev/posts/processing_inbox/
(defun kb/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (interactive)
  (org-with-wide-buffer
   ;; (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-todo)
   ;; (call-interactively 'jethro/my-org-agenda-set-effort)
   (call-interactively 'org-agenda-set-effort)
   (org-agenda-refile nil nil t)
   ))

(require 'dash)

(defun my-org-keep-quiet (orig-fun &rest args)
  (let ((buffers-pre (-filter #'get-file-buffer (org-agenda-files))))
    (apply orig-fun args)
    (let* ((buffers-post (-filter #'get-file-buffer (org-agenda-files)))
           (buffers-new  (-difference buffers-post buffers-pre)))
      (mapcar (lambda (file) (kill-buffer (get-file-buffer file))) buffers-new))))

(advice-add 'org-agenda-list :around #'my-org-keep-quiet)
(advice-add 'org-search-view :around #'my-org-keep-quiet)
(advice-add 'org-tags-view   :around #'my-org-keep-quiet)

(add-to-list 'org-modules 'org-habit)
(setq org-habit-show-habits-only-for-today t
      org-habit-preceding-days 3
      org-habit-following-days 5)
;; org-habit-graph-column 110)
(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t))) ; Force showing of habits in agenda every day at 6AM

(setq org-habit-graph-column 100)

(kb/leader-keys
  "oa" '(org-agenda :which-key "Org-agenda")
  )

(general-define-key ; Eyebrowse keybindings overwrite this so I reset it
  :keymaps 'org-capture-mode-map
  "C-c C-w" 'org-capture-refile
  )

(define-key org-agenda-mode-map (kbd "s") #'kb/org-refile-to-reverse-datetree-archive) ; Archiving tasks with org-reverse-datetree
(define-key org-agenda-mode-map (kbd "r") #'kb/org-agenda-process-inbox-item) ; Archiving tasks with org-reverse-datetree

;; (evil-define-key 'motion 'org-agenda-mode-map (kbd "q") #'org-agenda-exit) ; Bury org-agenda and close all related buffers
;; (evil-define-key 'motion 'org-agenda-mode-map (kbd "z q") #'org-agenda-quit) ; Bury org-agenda buffer only. Note that org-agenda-quit is distinct from org-agenda-Quit

(use-package org-ql
  :after org-roam ; Necessary for one or more of the functions
  :config
  (setq org-ql-views
   `(("Current projects"
      :buffers-files org-agenda-files
      :query (and (parent (todo))
                  (todo)
                  (not (done)))
      :super-groups ((:auto-parent t))
      :title "Current projects"
      :sort (deadline scheduled todo priority)
      )
     ("In-progress tasks not in a project"
      :buffers-files org-agenda-files
      :query (and (not (done))
                  (not (parent (todo)))
                  (not (children (todo)))
                  (todo "PROG"))
      :super-groups ((:discard (:file-path "roam"))
                     (:name none
                            :anything t))
      :title "In-progress tasks"
      :sort (priority deadline scheduled)
      )
     ("In-progress and upcoming Zettelkasten notes"
      :buffers-files ,(org-roam--list-all-files)
      :query (and (not (done))
                  (todo "PROG" "NEXT"))
      :super-groups ((:name none
                            :anything t))
      :title "Zettelkasten notes I'm working on"
      :sort (todo deadline scheduled priority)
      )
     ))
  ;; (setq org-ql-view-buffer nil) ; No clue how to set this variable

  (kb/leader-keys
    "oq" '(org-ql-view :which-key "Org-ql views") ; Currently can't find a way to close all org-agenda bufers after opening the org-ql-view
    )
  )

(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode)

  ;; Remove Evil bindings on group headings
  (define-key org-agenda-mode-map (kbd "h") nil)
  (define-key org-super-agenda-header-map (kbd "h") nil)
  (define-key org-agenda-mode-map (kbd "j") nil)
  (define-key org-super-agenda-header-map (kbd "j") nil)
  (define-key org-agenda-mode-map (kbd "k") nil)
  (define-key org-super-agenda-header-map (kbd "k") nil)
  (define-key org-agenda-mode-map (kbd "l") nil)
  (define-key org-super-agenda-header-map (kbd "l") nil)

  (setq org-agenda-custom-commands nil) ; Start from scratch
  )

(add-to-list 'org-agenda-custom-commands
             '("p" "Current tasks"
               ((org-ql-block '(and (parent (todo "PROG"))
                                    (todo)
                                    (not (done)))
                              ((org-ql-block-header "Current projects")
                               (org-agenda-files kb/all-agenda-dir-files-minus-inbox)
                               (org-super-agenda-groups
                                '((:auto-parent t))
                                )))
                (agenda ""
                        ((org-agenda-overriding-header "The near future")
                         (org-habit-show-habits-only-for-today t)
                         (org-agenda-start-day "+0") ; Start the agenda view with yesterday
                         (org-agenda-span 4)
                         (org-super-agenda-groups
                          '((:name "Due"
                                   :time-grid t
                                   :scheduled today
                                   :deadline today)
                            (:name "Planned in the future"
                                   :scheduled future)
                            (:name "Due in the future"
                                   :deadline future)
                            (:name "Missed Items!"
                                   :scheduled past
                                   :deadline past)
                            (:name "Uncategorized"
                                   :anything t)
                            ))
                         ))
                (org-ql-block '(and (todo "PROG")
                                    (not (parent (todo)))
                                    (not (children (todo)))
                                    (not (done)))
                              ((org-ql-block-header "Tasks by priority")
                               (org-agenda-files kb/all-agenda-dir-files-minus-inbox)
                               (org-super-agenda-groups
                                '((:auto-priority t))
                                )))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("n" "What's next?"
               ((org-ql-block '(or (and (parent (todo "NEXT"))
                                        (not (done)))
                                   (and (children (todo))
                                        (todo "NEXT")
                                        (not (done))
                                        ))
                              ((org-ql-block-header "Next projects")
                               (org-super-agenda-groups
                                '((:name "Projects" :children todo)
                                  (:auto-parent t)
                                  ))
                               ))
                (org-ql-block '(and (not (parent (todo)))
                                    (not (children (todo)))
                                    (todo "NEXT")
                                    (not (done)))
                              ((org-ql-block-header "Next Non-project Tasks")
                               (org-super-agenda-groups
                                '((:name "No effort or effort less than 5 minutes"
                                         :effort< "5")
                                  (:name "10 minutes or less"
                                         :effort< "11")
                                  (:name "30 minutes or less"
                                         :effort< "31")
                                  (:name "1 hour or less"
                                         :effort< "61")
                                  (:name "More than an hour but less than 3"
                                         :effort< "180")
                                  (:name "3 hours or more"
                                         :effort> "179")
                                  (:name "Next tasks without an effort rating"
                                         :anything t)
                                  ))
                               ))
                ;; (alltodo ""
                ;;          ((org-agenda-overriding-header "What else is on my to-do list?")
                ;;           (org-agenda-prefix-format
                ;;            '((agenda . " %i %-12:c%?-12t% s")
                ;;              (todo . " %i %-13:c   %-6e %?s %t | ")
                ;;              (tags . " %i %-12:c")
                ;;              (search . " %i %-12:c")
                ;;              ))
                ;;         (org-agenda-files kb/all-agenda-dir-files-minus-inbox)
                ;;           (org-super-agenda-groups
                ;;            '((:discard (:not (:todo "NEXT")))
                ;;              (:name "No effort or effort less than 5 minutes"
                ;;                     :effort< "5")
                ;;              (:name "10 minutes or less"
                ;;                     :effort< "11")
                ;;              (:name "30 minutes or less"
                ;;                     :effort< "31")
                ;;              (:name "1 hour or less"
                ;;                     :effort< "61")
                ;;              (:name "More than an hour but less than 3"
                ;;                     :effort< "180")
                ;;              (:name "3 hours or more"
                ;;                     :effort> "179")
                ;;              (:name "Next tasks without an effort rating"
                ;;                     :anything t)
                ;;              ))
                ;;           ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("z" "Current Zettelkasten notes"
               ((alltodo ""
                         ((org-agenda-overriding-header "Current and upcoming Zettelkasten notes")
                          (org-agenda-files (org-roam--list-all-files))
                          (org-super-agenda-groups
                           '((:discard (:not (:todo ("PROG" "NEXT"))))
                             (:name none
                                    :auto-parent t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("Zz" "Fresh Zettelkasten notes"
               ((alltodo ""
                         ((org-agenda-overriding-header "Zettelkasten maintanence overview")
                          (org-agenda-files (org-roam--list-all-files))
                          (org-super-agenda-groups
                           '((:name "Tags that are done but not marked as done"
                                    :tag ("MATURE" "COMPLETE"))
                             (:name "Fermenting notes"
                                    :tag "ephemeral")
                             (:name "Unprocessed ephemeral notes" ; Remove once I've finished processing all the notes with the ephemeral tag. I use my seedbox for this now
                                    :tag "ephemeral")
                             (:name "Fresh notes"
                                    :tag ("WAITING" "NASCENT"))
                             (:name "Intermediate notes"
                                    :tag ("PROGRESS" "GROWING"))
                             (:name "Irregular notes"
                                    :anything t)
                             ))
                          ))
                (alltodo ""
                         ((org-agenda-overriding-header "By category")
                          (org-agenda-files (org-roam--list-all-files))
                          (org-super-agenda-groups
                           '((:name "Tags that are done but not marked as done"
                                    :tag ("MATURE" "COMPLETE"))
                             (:name none
                                    :auto-category t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("Zg" "Zettelkasten growth and done"
               ((alltodo ""
                         ((org-agenda-overriding-header "All notes organized by note-type")
                          (org-agenda-files (org-roam--list-all-files))
                          (org-super-agenda-groups
                           '((:name none
                                    :auto-category t)
                             ))
                          ))
                (todo "DONE|CANCELLED"
                      ((org-agenda-overriding-header "Finished notes")
                       (org-agenda-files (org-roam--list-all-files))
                       (org-super-agenda-groups
                        '((:discard (:not (:category ("lit" "bib_notes" "quote" "zett" "ephemeral"))))
                          (:name none
                                 :auto-ts t)
                          (:name "Irregular notes"
                                 :anything t)
                          ))
                       ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("xu" "Projects potentially in limbo (via stuck projects)"
               ((stuck ""
                       ((org-agenda-overriding-header "School")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "school.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Computer stuff")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "computers.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Inputs")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "inputs.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Miscellaneous")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "misc.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Habits")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "habits.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                (stuck ""
                       ((org-agenda-overriding-header "Entertainment")
                        (org-super-agenda-groups
                         '((:discard (:tag "REFILE"))
                           (:discard (:not (:file-path "media.org")))
                           (:discard (:todo "INACTIVE"))
                           (:auto-category t)
                           ))
                        ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("xb" "Stuff in the backburner"
               ((alltodo ""
                         ((org-agenda-overriding-header "Did I forget about these?")
                          (org-super-agenda-groups
                           '((:discard (:not (:todo ("SOMEDAY" "HOLD" "INACTIVE"))))
                             (:name none
                                    :auto-category t)
                             (:name "You shouldn't be here..."
                                    :anything t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("xd" "Todos in a DONE state"
               ((todo "DONE|CANCELLED"
                      ((org-agenda-overriding-header "Regular candidates for archival")
                       (org-agenda-files (directory-files-recursively kb/agenda-dir "[^hive].org$"))))
                (todo "DONE|CANCELLED"
                      ((org-agenda-overriding-header "Done Zettelkasten notes")
                       (org-agenda-files (org-roam--list-all-files))))
                )
               )
             t)

(add-to-list 'org-agenda-custom-commands
             '("xr" "All trivial and to-refile tasks"
               ((alltodo ""
                         ((org-agenda-overriding-header "Tasks to refile")
                          (org-super-agenda-groups
                           '((:discard (:not (:tag "REFILE")))
                             (:name none
                                    :auto-tags t)
                             (:discard (:anything t))
                             ))
                          ))
                (alltodo ""
                         ((org-agenda-overriding-header "High-priority items without next todo keyword")
                          (org-super-agenda-groups
                           '((:name none
                                    :and (:priority>= "B" :not (:todo ("NEXT" "PROG"))))
                             (:discard (:anything t))
                             ))
                          ))
                (alltodo ""
                         ((org-agenda-overriding-header "Trivial Tasks")
                          (org-super-agenda-groups
                           '((:name none
                                    :and (:priority<= "E" :not (:todo ("HOLD" "SOMEDAY" "INACTIVE" "CANCELLED"))))
                             (:discard (:anything t))
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("fw" "Schoolwork"
               ((agenda ""
                        ((org-agenda-overriding-header "My school calendar")
                         (org-agenda-span 'week)
                         (org-agenda-start-day "-1") ; Start the agenda view with yestersy
                         (org-agenda-span 7)
                         (org-super-agenda-groups
                          '((:discard (:not (:file-path "school")))
                            (:name "Due"
                                   :time-grid t
                                   :deadline today)
                            (:name "Planned"
                                   :time-grid t
                                   :scheduled today)
                            (:name "Due in the future"
                                   :time-grid t
                                   :deadline future)
                            (:name "Planned in the future"
                                   :time-grid t
                                   :scheduled future)
                            (:name "Missed Items!"
                                   :scheduled past
                                   :deadline past)
                            (:name "Uncategorized"
                                   :anything t)
                            ))
                         ))
                (alltodo ""
                         ((org-agenda-overriding-header "Assignments on my plate...")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "school.org")))
                             (:name "Related to coursework"
                                    :tag ("ASSIGNMENT" "EMAIL"))
                             (:name "Me involved with the community"
                                    :tag ("CLUB" "EVENT" "SOCIAL" "ORGANIZATION"))
                             (:name "Consumption"
                                    :tag ("LEARN" "PARSE"))
                             (:name "Finances"
                                    :tag ("PAYING"))
                             (:name "Overflow (uncategorized)"
                                    :anything t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("fe" "Entertainment time?"
               ((alltodo ""
                         ((org-agenda-overriding-header "What's on my \"to-comsume\" list?")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "media.org")))
                             (:name "Movies"
                                    :tag "MOVIE")
                             (:name "Shows"
                                    :tag "SHOW")
                             (:name "Books"
                                    :tag "BOOK")
                             (:name "Videos"
                                    :tag "VIDEOS")
                             (:name "Overflow (uncategorized)"
                                    :anything t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("fc" "Computer-related tasks"
               ((alltodo ""
                         ((org-agenda-overriding-header "Computer stuff I have to get to")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "computers.org")))
                             (:name "Projects"
                                    :tag ("PROJECT"))
                             (:name "Things that involve thinking"
                                    :tag ("DWELL" "WORKFLOW"))
                             (:name "Going through information"
                                    :tag ("LEARN" "PACKAGE" "DOCS"))
                             (:name "Actions for the better"
                                    :tag ("CONFIG" "TROUBLESHOOTING"))
                             (:name "Actions for QoL"
                                    :tag ("RICE"))
                             (:name "Overflow (uncategorized)"
                                    :anything t)
                             ))
                          ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("fi" "My input tasks"
               ((alltodo ""
                         ((org-agenda-overriding-header "All my inputs")
                          (org-super-agenda-groups
                           '((:discard (:not (:file-path "inputs.org")))
                             (:name none
                                    :auto-tags t)
                             ))
                          ))
                ))
             t)

(with-eval-after-load 'org-super-agenda
  (set-face-attribute 'org-super-agenda-header nil :height 148 :font "Noto Sans" :foreground "DarkGoldenrod2" :underline nil)
  (set-face-attribute 'org-agenda-date nil :height 157 :font "Noto Sans" :foreground "dodger blue" :underline nil)
  (set-face-attribute 'org-agenda-structure nil :height 180 :font "Noto Sans" :bold t :italic t :foreground "DarkOliveGreen3" :underline t)
  )

(setq org-default-notes-file (concat kb/agenda-dir "inbox.org"))
(setq org-capture-templates ; Used for org-agenda task management
      '(("s" "School related task")
        ("ss" "New assignment" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] %? [/] :ASSIGNMENT:\nSCHEDULED: %^T\nDEADLINE: %^T\n%U")
        ;; ("si" "Go through information" entry (file org-default-notes-file)
        ;;  "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] %? [/] :PARSE:\nSCHEDULED: %^T\n%U")
        ;; ("st" "Think about something" entry (file org-default-notes-file)
        ;;  "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] %? [/] %^G\nSCHEDULED: %^T\n%U")
        ("se" "Email" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Revisit %:fromname ( %:fromaddress ) -- /%:subject/ [/] :EMAIL:\n- RECEIVED :: %:date-timestamp-inactive\nSCHEDULED: %^t\nDEADLINE: %^T\n%A\n%U")
        ;; ("sz" "Zoom call" entry (file org-default-notes-file)
        ;;  "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] %? [/] %^G\nDEADLINE: %^T\n%U")
        ;; ("sw" "Watch or listen to something" entry (file org-default-notes-file)
        ;;  "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] %? [/] %^G\n%U")

        ("i" "New input")
        ("iv" "Video" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Watch %(org-cliplink-capture) :VIDEO:\n%U\n"
         :immediate-finish t)
        ("ia" "Article" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Read %(org-cliplink-capture) :ARTICLE:\n%U\n"
         :immediate-finish t)
        ("ip" "Podcast" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Listen to %(org-cliplink-capture) :PODCAST:\n%U\n")
        ("iw" "Profound quote" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p%? :QUOTE:\nby \n%U\n\n")
        ("ib" "Book" entry (file org-default-notes-file)
         "* TODO [#%^{Priority?|A|B|C|D|E|F}] Read /%?/ :BOOK:\nby \n%U\n")
        ("il" "Lecture" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Watch and study %(org-cliplink-capture) :LECTURE:\n%U\n"
         :immediate-finish t)
        ("ij" "Academic paper" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] Read and analyze %(org-cliplink-capture) :PAPER:\n%U\n"
         :immediate-finish t)

        ("m" "New entertainment to gobble" entry (file org-default-notes-file)
         "* TODO Consume %? %^{What type of entertainment?|MOVIE|BOOK|SHOW|VIDEO}\n%U\n")
        ;; ("m" "New entertainment to gobble")
        ;; ("mm" "Movie" entry (file org-default-notes-file)
        ;;  "* TODO Watch %? :MOVIE:\n%U\n")
        ;; ("mb" "Book" entry (file org-default-notes-file)
        ;;  "* TODO Read %? :BOOK:\n%U\n")
        ;; ("ms" "Show" entry (file org-default-notes-file)
        ;;  "* TODO Watch %? :SHOW:\n%U\n")
        ;; ("mv" "Video" entry (file org-default-notes-file)
        ;;  "* TODO Watch %? :VIDEO:\n%U\n")

        ("c" "Computer thing")
        ("cc" "Check something" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] %? [/] %^G\n%U")
        ;; ("ct" "Something related to though or learning" entry (file org-default-notes-file)
        ;;  "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] %? [/] %^G\n%U")
        ("ca" "Do something technical" entry (file org-default-notes-file)
         "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] %? [/] %^G\n%U")

        ("a" "Agnostic todo" entry (file org-default-notes-file)
         "* TODO %? [/] %^G\n%U")
        ;; ("a" "Agnostic todo" entry (file org-default-notes-file)
        ;;  "* TODO %^{EFFORT}p[#%^{Priority?|A|B|C|D|E|F}] %? [/] %^G\n%U")
        ("f" "This is an idea I should ferment" entry (file+headline "~/Documents/org-database/roam/seedbox.org" "Fermenting Items")
         "* %? \n%U"
         :jump-to-captured t)
        ("h" "Habit/recurring task" entry (file org-default-notes-file)
         "* NEXT %? [/] :HABIT:\nSCHEDULED: %^t\n:PROPERTY:\n:LOGGING: DONE(!)\n:STYLE: habit\n:END:\n%U\n"
         :immediate-finish t
         :jump-to-captured t)
        ))

(use-package org-wild-notifier
  :custom
  (alert-default-style 'libnotify) ; Set default alert (global) style
  (org-wild-notifier-alert-time '(10 45 120))
  (org-wild-notifier-notification-title "Org-agenda")
  (org-wild-notifier-keyword-whitelist nil)
  (org-wild-notifier-keyword-blacklist nil)
  (org-wild-notifier-tags-whitelist nil)
  (org-wild-notifier-tags-blacklist nil)
  (org-wild-notifier-alert-times-property "wild_notifier_notify_before")
  :config
  (org-wild-notifier-mode)
  )

(setq org-attach-id-dir "attachments/")
(setq org-attach-dir-relative t) ; Use relative file paths
(setq org-attach-method 'cp) ; Attach copies of files
(setq org-attach-archive-delete 'query) ; If subtree is deleted or archived, ask user

(use-package org-download
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "scrot -s %s") ; Use scrot
  (org-download-link-format "[[download:%s]]\n")
  (org-download-annotate-function (lambda (_link) ""))
  :config
  (setq-default org-download-image-dir (concat org-directory "resources/")
                org-download-heading-lvl nil
                org-download-timestamp "%Y-%m-%d_%H-%M-%S_") ; Default

  (kb/leader-keys
    "ii" '(org-download-clipboard :which-key "Paste image from clipboard")
    )
  )

(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '((?A . "💀")
                               (?B . "🔥")
                               (?C . "🌟")
                               (?D . "🏃")
                               (?E . "👍")
                               (?F . "🧋")))
  )

(use-package org-cliplink
  :config
  (kb/leader-keys
    "ib" '(org-cliplink :which-key "Paste https"))
  )

(use-package visual-fill-column
  :ensure t
  :hook ((org-mode . visual-fill-column-mode)
         (mu4e-view-mode . visual-fill-column-mode))
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t)
  :config
  (setq-default split-window-preferred-function 'visual-fill-column-split-window-sensibly) ; Be able to vertically split windos that have wide margins
  )

(use-package helm-org-rifle
  :config
  (kb/leader-keys
    "fcl" '((lambda () (interactive) (helm-org-rifle-directories kb/library-dir nil)) :which-key "Library search")
    "fco" '(helm-org-rifle-org-directory :which-key "Org directory search")

    "or" '(helm-org-rifle-current-buffer :which-key "Helm-org-rifle current buffer")
    "oR" '(helm-org-rifle-occur-current-buffer :which-key "Helm-org-rifle-occur current buffer")
    )
  )

(with-eval-after-load 'org
  (setq org-src-tab-acts-natively t) ; Treat tabs in src blocks the same as if it were in the language's major mode
  )

(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :config
  (kb/leader-keys
    "ld" 'xref-find-definitions
    "lr" 'xref-find-references
    "ln" 'lsp-ui-find-next-reference
    "lp" 'lsp-ui-find-prev-reference
    "ls" 'counsel-imenu
    "le" 'lsp-ui-flycheck-list
    "lS" 'lsp-ui-sideline-mode
    "lX" 'lsp-execute-code-action)
  )

(use-package doom-snippets
  :demand t
  :quelpa (doom-snippets :fetcher git :url "https://github.com/hlissner/doom-snippets")
  )

(require' org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;; (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
;; (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
;; (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
;; (add-to-list 'org-structure-template-alist '("json" . "src json"))

(use-package yasnippet
  :hook ((text-mode . yas-minor-mode-on)
         (prog-mode . yas-minor-mode-on)
         (snippet-mode . yas-minor-mode-on))
  :config
  (defvar kb/personal-snippets-dir
    (concat user-emacs-directory "snippets/"))
  (add-to-list 'yas-snippet-dirs 'kb/personal-snippets-dir) ; Only accepts symbols or strings

  (yas-reload-all)
  )

(use-package lisp-extra-font-lock
  :hook (emacs-lisp-mode . (lambda () (lisp-extra-font-lock-mode t)))
  :config
  ;; (lisp-extra-font-lock-global-mode 1)
  )

(use-package highlight-function-calls
  :hook (emacs-lisp-mode . (lambda () (highlight-function-calls-mode t)))
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (auto-revert-check-vc-info nil) ; Fixes VC info on a timer in order to take into account changes made outside of Emacs - causes micro-stutters when too many version controlled buffers
  :config
  ;; (add-hook 'magit-post-stage-hook 'magit-refresh) ; Immedieatly refresh buffer after staging something - causes freezing?

  (kb/leader-keys
    "gg"  '(magit-status :which-key "Status")
    "gs"  '(magit-status :which-key "Status")
    "gd"  'magit-diff-unstaged
    "gc"  'magit-branch-or-checkout
    "gl"   '(:ignore t :which-key "log")
    "glc" 'magit-log-current
    "glf" 'magit-log-buffer-file
    "gb"  'magit-branch
    "gP"  'magit-push-current
    "gp"  'magit-pull-branch
    "gf"  'magit-fetch
    "gF"  'magit-fetch-all
    "gr"  'magit-rebase)
  )

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-character ?⏐)
  :config
  ;; (set-face-attribute 'highlight-indent-guides-character-face nil :inherit 'org-block) ;:(background "#232635") ; Same as org-block background
  (highlight-indent-guides-auto-set-faces) ; Set faces based on theme
  )

(use-package flycheck
  :hook (lsp-mode . flycheck-mode) ; Sthart alongside lsp-mode
  :custom
  (flycheck-emacs-lisp-load-path 'inherit) ; Use load-path for Emacs session

  ;; Check syntax only when saving file and switching to other buffer
  (flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))
  (flycheck-idle-buffer-switch-delay 2) ; Wait 2 secons after buffer switch

  ;; Run flycheck even if visiting buffer quickly (reliant on idle-buffer-switch)
  (flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Time to show an error on point
  (flycheck-display-errors-delay 0.25)
  )

(use-package rainbow-mode
  :config
  (rainbow-mode) ; Enable everywhere
  )

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode)
  )

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p user-emacs-directory)
    (setq projectile-project-search-path `(,user-emacs-directory)))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode)

  (kb/leader-keys
    "Pf"  'counsel-projectile-find-file
    "Ps"  'counsel-projectile-switch-project
    "PF"  'counsel-projectile-rg
    "Pp"  'counsel-projectile
    "Pc"  'projectile-compile-project
    "Pd"  'projectile-dired)
  )

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode t)
  )

(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  )

(with-eval-after-load 'eshell
  (setq eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input))) ; don't record command in history if prefixed with whitespace
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  ;; UI Enhancements
  (add-hook 'eshell-mode-hook
            (defun +eshell-remove-fringes-h ()
              (set-window-fringes nil 0 0)
              (set-window-margins nil 1 nil)))

  ;; Enable text wrapping
  (add-hook 'eshell-mode-hook
            (defun +eshell-enable-text-wrapping-h ()
              (visual-line-mode +1)
              (set-display-table-slot standard-display-table 0 ?\ )))
  )

;; (define-key eshell-mode-map (kbd "<up>") 'eshell-previous-matching-input-from-input) ; Mode map doesn't fking exist
;; (define-key eshell-mode-map (kbd "<down>") 'eshell-next-matching-input-from-input)

(kb/leader-keys
  "oE" '(eshell :which-key "Open eshell"))

;; Go to https://github.com/daviwil/dotfiles/blob/master/Emacs.org#better-colors

(require 'dash)
(require 's)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                      (-> ,ICON
                          (concat esh-section-delim ,FORM)
                          (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          "\n"
          eshell-prompt-string))

;; Separator between esh-sections
(setq esh-sep " | ")  ; or " | "

;; Separator between an esh-section icon and form
(setq esh-section-delim " ")

;; Eshell prompt header
(setq esh-header "\n┌─")  ; or "\n┌─"

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(setq eshell-prompt-regexp "└─> λ ")   ; or "└─> "
(setq eshell-prompt-string "└─> λ ")   ; or "└─> "

(esh-section esh-dir
             " \xf07c "  ;  (faicon folder)
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "gold" :weight 'bold))

(esh-section esh-git
             "ᛦ"  ;  (git icon)
             (magit-get-current-branch)
             '(:foreground "pink"))

;; (esh-section esh-python
;;              "\xe928"  ;  (python icon)
;;              pyvenv-virtual-env-name)

(esh-section esh-clock
             "\xf017 "  ;  (clock icon)
             (format-time-string "%H:%M" (current-time))
             '(:foreground "forest green"))

;; Below I implement a "prompt number" section
(setq esh-prompt-num 0)
(add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

(esh-section esh-num
             "\xf0c9 "  ;  (list icon)
             (number-to-string esh-prompt-num)
             '(:foreground "brown"))

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

;; Enable the new eshell prompt
(setq eshell-prompt-function 'esh-prompt-func)

(require 'dash)
(require 's)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                      (-> ,ICON
                          (concat esh-section-delim ,FORM)
                          (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          "\n"
          eshell-prompt-string))

;; Separator between esh-sections
(setq esh-sep " | ")  ; or " | "

;; Separator between an esh-section icon and form
(setq esh-section-delim " ")

;; Eshell prompt header
(setq esh-header "\n┌─")  ; or "\n┌─"

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(setq eshell-prompt-regexp "└─> λ ")   ; or "└─> "
(setq eshell-prompt-string "└─> λ ")   ; or "└─> "

(esh-section esh-dir
             " \xf07c "  ;  (faicon folder)
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "gold" :weight bold))

(esh-section esh-git
             "ᛦ"  ;  (git icon)
             (magit-get-current-branch)
             '(:foreground "pink"))

;; (esh-section esh-python
;;              "\xe928"  ;  (python icon)
;;              pyvenv-virtual-env-name)

(esh-section esh-clock
             "\xf017 "  ;  (clock icon)
             (format-time-string "%H:%M" (current-time))
             '(:foreground "forest green"))

;; Below I implement a "prompt number" section
(setq esh-prompt-num 0)
(add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

(esh-section esh-num
             "\xf0c9 "  ;  (list icon)
             (number-to-string esh-prompt-num)
             '(:foreground "brown"))

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

;; Enable the new eshell prompt
(setq eshell-prompt-function 'esh-prompt-func)

(use-package smartparens
  :hook (eshell-mode . smartparens-mode)
  )

(use-package eshell-up)

(use-package eshell-z)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3) ; How big is the window?
  ;; (eshell-toggle-use-projectile-root t)
  (eshell-toggle-use-git-root t)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell) ; Terminal emulator to use
  (eshell-toggle-run-command nil) ; Command to run in new buffer
  :config

  (kb/leader-keys
    "oe" '(eshell-toggle :which-key "Toggle Eshell")
    )
  )

(use-package esh-help
  :config
  (setup-esh-help-eldoc)
  )

(use-package shrink-path)

(use-package esh-autosuggest
  :disabled ; Fish does this better?
  :hook (eshell-mode . esh-autosuggest-mode)
  :custom
  (esh-autosuggest-delay 0.5)
  :config
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil)
  )

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode)
  :config
  (when (and (executable-find "fish")
             (require 'fish-completion nil t))
    (global-fish-completion-mode))
  )

(use-package eshell-syntax-highlighting
  :config
  (eshell-syntax-highlighting-global-mode t)
  )

(use-package undo-fu
  :demand t
  :hook (after-init . undo-fu-mode)
  :custom
  ;; Store more undo history to prevent loss of data
  (undo-limit (* 100 1024))
  (undo-strong-limit 3000000)
  (undo-outer-limit 3000000)
  :config

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t)
  )

(use-package undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :preface
  (setq undo-fu-session-directory (concat user-emacs-directory "undo-fu-session/")
        undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  ;; (global-undo-tree-mode -1) ; Incompatible - make sure it's off
  )

(use-package spell-fu
  :custom
  (spell-fu-idle-delay 0.6)
  (spell-fu-directory (concat user-emacs-directory "spell-fu"))

  ;; Ispell settings
  (ispell-program-name "/usr/bin/aspell")
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary (concat user-emacs-directory "ispell-personal-dict-en"))
  :config
  (add-hook 'text-mode-hook 'spell-fu-mode)

  (kb/leader-keys
    "ts" '(spell-fu-mode :which-key "Toggle spell-fu")
    "ta" '(spell-fu-word-add :which-key "Spell-fu-word-add"))
  )

(use-package define-word
  :config
  (global-set-key (kbd "C-c d") 'define-word-at-point)
  (global-set-key (kbd "C-c D") 'define-word)
  )

(use-package powerthesaurus
  :config
(kb/leader-keys
    "Ll" 'powerthesaurus-lookup-word
    "Lp" 'powerthesaurus-lookup-word-at-point

    ;; combines these two functions into one. It tries to infer whatever
    ;; user wants to look up. If there is an active selection that will be
    ;; the choice. Otherwise, it checks if there any word at point and
    ;; fetches that word. And if there is nothing appropriate, it asks the
    ;; user to provide a word.
    "Lb" 'powerthesaurus-lookup-word-dwim)
  )
