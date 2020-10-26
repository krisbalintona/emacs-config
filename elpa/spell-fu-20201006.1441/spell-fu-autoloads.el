;;; spell-fu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "spell-fu" "spell-fu.el" (0 0 0 0))
;;; Generated autoloads from spell-fu.el

(autoload 'spell-fu-mode "spell-fu" "\
Toggle `spell-fu-mode' in the current buffer.

If called interactively, enable Spell-Fu mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-spell-fu-mode 'globalized-minor-mode t)

(defvar global-spell-fu-mode nil "\
Non-nil if Global Spell-Fu mode is enabled.
See the `global-spell-fu-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-spell-fu-mode'.")

(custom-autoload 'global-spell-fu-mode "spell-fu" nil)

(autoload 'global-spell-fu-mode "spell-fu" "\
Toggle Spell-Fu mode in all buffers.
With prefix ARG, enable Global Spell-Fu mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Spell-Fu mode is enabled in all buffers where
`spell-fu-mode-turn-on' would do it.
See `spell-fu-mode' for more information on Spell-Fu mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "spell-fu" '("spell-fu-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; spell-fu-autoloads.el ends here
