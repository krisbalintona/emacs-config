;;; repo-configure-rcp.el --- Configure and load repositories

;;; Code:
;; LoadRepos
;; All of this is package.el and it conflicts with
;; straight.el. Package-archives can be used if loaded after
;; straight.el. Useful if you want to still peruse packages with M-x
;; package-list-packages
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ("org" . "http://orgmode.org/elpa/")
        ;; ("gnu"   . "https://elpa.gnu.org/packages/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ;; ;; Chinese servers
        ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ))
;; (setq package-user-dir (expand-file-name "elpa" user-emacs-directory)) ; Select folder to store packages

;; (unless (bound-and-true-p package--initialized)
;;   (setq package-enable-at-startup nil) ; Don't package initialize at Emacs startup (?)
;;   (package-initialize))
;; LoadRepos

(provide 'repo-configure-rcp)
;;; Commentary:
;;
;;; repo-configure-rcp.el ends here
