;;; Package.el
;; Initialize package resources
(setopt package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                           ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-archive-priorities '(("gnu-elpa" . 4)
                                     ("melpa" . 3)
                                     ("nongnu" . 2)
                                     ("gnu-elpa-devel" . 1))
        package-install-upgrade-built-in t
        package-pinned-packages nil

        load-prefer-newer t)            ; Do not load outdated byte code files

;; As recommended in https://elpa.gnu.org/
(unless (fboundp 'package-activate-all) (package-initialize))

;; Although `use-package' is built-in starting Emacs 29.1, I should make sure
;; it's installed just in case I test/use an earlier Emacs version
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setopt use-package-always-ensure t
        use-package-expand-minimally t  ; Verbosity of use-package macro
        use-package-always-defer nil)

;; Only be verbose when interpreted, otherwise errors are caught at compile time
(setopt use-package-verbose (not (bound-and-true-p byte-compile-current-file)))

;; Compile statistics to be shown in `use-package-report'
(setopt use-package-compute-statistics t)

;; Set better default package paths
(use-package no-littering
  :init
  ;; According to the package instructions, these variables must be set prior to
  ;; loading the feature
  (eval-and-compile                 ; Ensure values don't differ at compile time
    (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory) ; Config files
          no-littering-var-directory (expand-file-name "var/" user-emacs-directory))) ; Persistent files
  :config
  ;; Ensure the directories exist
  (mkdir no-littering-etc-directory t)
  (mkdir no-littering-var-directory t)

  ;; Read docstring.  Sets more secure values for
  ;; `auto-save-file-name-transforms', `backup-directory-alist', and
  ;; `undo-tree-history-directory-alist'.
  (no-littering-theme-backups))

;;; On
;; Package exposes a number of utility hooks and functions ported from Doom
;; Emacs.  The hooks make it easier to speed up Emacs startup by providing
;; finer-grained control of the timing at which packages are loaded.  Provides
;; the following hooks:
;; - on-first-input-hook
;; - on-init-ui-hook
;; - on-first-file-hook
;; - on-switch-frame-hook
;; - on-first-buffer-hook
;; - on-switch-buffer-hook
;; - on-switch-window-hook
(use-package on)

;;; System-packages
;; Install system packages via Emacs. Necessary for use-package's
;; `:ensure-system-package' flag
(use-package system-packages
  :custom
  (system-packages-noconfirm nil)
  :config
  (when (executable-find "paru")
    (add-to-list 'system-packages-supported-package-managers
                 '(paru . ((default-sudo . nil)
                           (install . "paru -S")
                           (search . "paru -Ss")
                           (uninstall . "paru -Rns")
                           (update . "paru -Syu")
                           (clean-cache . "paru -Sc")
                           (change-log . "paru -Qc")
                           (log . "cat /var/log/paru.log")
                           (get-info . "paru -Qi")
                           (get-info-remote . "paru -Si")
                           (list-files-provided-by . "paru -qQl")
                           (owning-file . "paru -Qo")
                           (owning-file-remote . "paru -F")
                           (verify-all-packages . "paru -Qkk")
                           (verify-all-dependencies . "paru -Dk")
                           (remove-orphaned . "paru -Rns $(paru -Qtdq)")
                           (list-installed-packages . "paru -Qe")
                           (list-installed-packages-all . "paru -Q")
                           (list-dependencies-of . "paru -Qi")
                           (noconfirm . "--noconfirm"))))
    (setq system-packages-package-manager 'paru
          system-packages-use-sudo nil)))

;;; El-patch
(use-package el-patch)

;;; Async.el
;; Async library and a few small but useful implementations
(use-package async
  :custom
  (async-bytecomp-allowed-packages 'all)
  :config
  (async-bytecomp-package-mode 1))

;;; Enlight
;; Easily create simple startup screens
(use-package enlight
  :hook (window-setup . (lambda ()
                          (unless (or desktop-save-mode
                                      (bound-and-true-p easysession-save-mode)
                                      (bound-and-true-p psession-mode))
                            (enlight-open))))
  :custom
  (enlight-content
   (concat
    (grid-get-box `( :align center
                     :width 80
                     :content
                     ;; Art generated by
                     ;; https://www.patorjk.com/software/taag/#p=display&f=Isometric1&t=emacs
                     ,(propertize
                       "      ___           ___           ___           ___           ___
     /\\  \\         /\\__\\         /\\  \\         /\\  \\         /\\  \\
    /::\\  \\       /::|  |       /::\\  \\       /::\\  \\       /::\\  \\
   /:/\\:\\  \\     /:|:|  |      /:/\\:\\  \\     /:/\\:\\  \\     /:/\\ \\  \\
  /::\\~\\:\\  \\   /:/|:|__|__   /::\\~\\:\\  \\   /:/  \\:\\  \\   _\\:\\~\\ \\  \\
 /:/\\:\\ \\:\\__\\ /:/ |::::\\__\\ /:/\\:\\ \\:\\__\\ /:/__/ \\:\\__\\ /\\ \\:\\ \\ \\__\\
 \\:\\~\\:\\ \\/__/ \\/__/~~/:/  / \\/__\\:\\/:/  / \\:\\  \\  \\/__/ \\:\\ \\:\\ \\/__/
  \\:\\ \\:\\__\\         /:/  /       \\::/  /   \\:\\  \\        \\:\\ \\:\\__\\
   \\:\\ \\/__/        /:/  /        /:/  /     \\:\\  \\        \\:\\/:/  /
    \\:\\__\\         /:/  /        /:/  /       \\:\\__\\        \\::/  /
     \\/__/         \\/__/         \\/__/         \\/__/         \\/__/    "
                       'face 'modus-themes-fg-yellow-intense)))
    "\n\n"
    (grid-get-box
     `( :align center
        :width 80
        :content ,(enlight-menu
                   `(("Configs"
                      ;; 2025-03-12 FIXME: This is a workaround.  Using
                      ;; `command-remapping' does not work for some reason.
                      ("Recent files" ,(if (functionp 'recentf-open-files)
                                           'consult-recent-file
                                         'recentf-open-files)
                       "r")
                      ("Emacs" (project-switch-project user-emacs-directory) "e")
                      ("Dotfiles" (project-switch-project "~/dotfiles/") "d"))
                     ("Other"
                      ("Projects" project-switch-project "p")
                      ("Email" notmuch "n"))))))))
  :init
  ;; For more complex layouts, as recommended by the README
  (use-package grid
    :autoload (grid-get-box grid-get-row grid-get-column)
    :vc (:url "https://github.com/ichernyshovvv/grid.el")))

;;; Provide
(provide 'krisb-core)
