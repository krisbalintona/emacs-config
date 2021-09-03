;;; shell-vterm-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Vterm packages and their configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Vterm
;; Full-fledged terminal emulator
(use-package vterm
  :demand t
  :ensure-system-package (("/usr/lib64/libvterm.so.0" . libvterm) ; Specifically for Fedora
                          (libtool)
                          ("/usr/include/vterm.h" . "libvterm-devel")
                          (cmake))
  :gfhook '(lambda ()
             (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
             (buffer-face-mode t)
             (face-remap-add-relative 'default :height 119)) ; Change default face size
  :custom
  (vterm-kill-buffer-on-exit nil)
  (vterm-copy-exclude-prompt t)
  (vterm-min-window-width 50)
  )

;;; shell-vterm-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-vterm-rcp)
