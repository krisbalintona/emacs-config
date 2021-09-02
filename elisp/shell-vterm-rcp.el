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
  :gfhook '(lambda ()
             (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
             (buffer-face-mode t)
             (face-remap-add-relative 'default :height 119)) ; Change default face size
  :custom
  (vterm-kill-buffer-on-exit nil)
  (vterm-copy-exclude-prompt t)
  (vterm-min-window-width 50)
  )

;;;; Vterm-toggle
;; Eshell-toggle but for vterm
(use-package vterm-toggle
  :requires vterm
  :general
  (kb/leader-keys
    "ot" '(vterm-toggle :which-key "Vterm-toggle")
    "oT" '(vterm :which-key "Vterm in current window")
    )
  (kb/leader-keys
    :keymaps 'vterm-mode-map
    :states '(normal motion visual)
    "vp" '(vterm-toggle-backward :which-key "Prev vterm buffer")
    "vn" '(vterm-toggle-forward :which-key "Prev vterm buffer")
    "vd" '(vterm-toggle-insert-cd :which-key "Cd to current buffer dir")
    )
  )

;;; shell-vterm-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-vterm-rcp)
