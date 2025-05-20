;; -*- lexical-binding: t; -*-

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

;;; Async.el
;; Async library and a few small but useful implementations
(use-package async
  :custom
  (async-bytecomp-allowed-packages 'all)
  :config
  (async-bytecomp-package-mode 1))

;;; Provide
(provide 'krisb-core)
