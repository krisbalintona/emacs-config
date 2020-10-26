;;; doom-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "doom-snippets" "doom-snippets.el" (0 0 0 0))
;;; Generated autoloads from doom-snippets.el

(autoload 'doom-snippets-remove-compiled-snippets "doom-snippets" "\
Delete all .yas-compiled-snippets.el files." t nil)

(autoload 'doom-snippets-initialize "doom-snippets" "\
Add `doom-snippets-dir' to `yas-snippet-dirs', replacing the default
yasnippet directory." nil nil)

(eval-after-load 'yasnippet (lambda nil (doom-snippets-initialize)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-snippets" '("doom-snippets-")))

;;;***

;;;### (autoloads nil "doom-snippets-lib" "doom-snippets-lib.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-snippets-lib.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-snippets-lib" '("doom-snippets-")))

;;;***

;;;### (autoloads nil nil ("doom-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doom-snippets-autoloads.el ends here
