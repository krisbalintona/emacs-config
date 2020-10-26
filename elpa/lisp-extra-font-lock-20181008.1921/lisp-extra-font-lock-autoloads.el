;;; lisp-extra-font-lock-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lisp-extra-font-lock" "lisp-extra-font-lock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lisp-extra-font-lock.el

(defvar lisp-extra-font-lock-modes '(emacs-lisp-mode lisp-mode) "\
List of modes where Lisp Extra Font Lock Global mode should be enabled.")

(custom-autoload 'lisp-extra-font-lock-modes "lisp-extra-font-lock" t)

(autoload 'lisp-extra-font-lock-mode "lisp-extra-font-lock" "\
Minor mode that highlights bound variables and quoted expressions in lisp.

If called interactively, enable Lisp-Extra-Font-Lock mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'lisp-extra-font-lock-global-mode 'globalized-minor-mode t)

(defvar lisp-extra-font-lock-global-mode nil "\
Non-nil if Lisp-Extra-Font-Lock-Global mode is enabled.
See the `lisp-extra-font-lock-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lisp-extra-font-lock-global-mode'.")

(custom-autoload 'lisp-extra-font-lock-global-mode "lisp-extra-font-lock" nil)

(autoload 'lisp-extra-font-lock-global-mode "lisp-extra-font-lock" "\
Toggle Lisp-Extra-Font-Lock mode in all buffers.
With prefix ARG, enable Lisp-Extra-Font-Lock-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Lisp-Extra-Font-Lock mode is enabled in all buffers where
`(lambda nil (when (apply 'derived-mode-p lisp-extra-font-lock-modes) (lisp-extra-font-lock-mode 1)))' would do it.
See `lisp-extra-font-lock-mode' for more information on Lisp-Extra-Font-Lock mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "lisp-extra-font-lock" '("lisp-extra-font-lock-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lisp-extra-font-lock-autoloads.el ends here
