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
  :ensure-system-package (("/usr/lib64/libvterm.so.0" . libvterm) ; Specifically for Fedora
                          (libtool)
                          ("/usr/include/vterm.h" . "libvterm-devel")
                          (cmake))
  :gfhook
  '(lambda ()
     (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
     (buffer-face-mode t)
     (face-remap-add-relative 'default :height 107)) ; Change default face size
  'hide-mode-line-mode
  :general (kb/leader-keys "ot" '(vterm :which-key "Vterm"))
  :custom
  (vterm-kill-buffer-on-exit nil)
  (vterm-copy-exclude-prompt t)
  (vterm-timer-delay 0.01)              ; Make vterm appear less "slow"
  )

;;; shell-vterm-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-vterm-rcp)
