;;; repo-configure-rcp.el --- Configure and load repositories

;;; Code:
;; LoadRepos
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
  (setq package-enable-at-startup nil) ; Nil if I have it t in early-init.el (?)
  (package-initialize))
;; LoadRepos

;; Set use-package-verbose to t for interpreted .emacs, and to nil for
;; byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;; InstallOrgContrib
(unless (package-installed-p 'org-plus-contrib)
  (progn
    (package-refresh-contents)
    (package-install 'org-plus-contrib)))
;; InstallOrgContrib

(provide 'repo-configure-rcp)
;;; Commentary:
;;
;;; repo-configure-rcp.el ends here
