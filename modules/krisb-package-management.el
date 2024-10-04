;; Initialize package resources
(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu-elpa" . 4)
                                   ("melpa" . 3)
                                   ("nongnu" . 2)
                                   ("gnu-elpa-devel" . 1))
      package-install-upgrade-built-in t
      package-pinned-packages nil)

;; We have to manually initialize package because in `early-init.el' we set
;; `package-enable-at-startup' to nil to speed up startup, like Doom Emacs does.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
