;;; shell-vterm-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Vterm packages and their configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Shell-mode
(use-package sh-script
  :straight nil
  :config
  (general-unbind ; Interferes with eyebrowse
    :keymaps 'sh-mode-map
    :states 'normal
    "gz"
    )
  )

;;;; Vterm
;; Full-fledged terminal emulator
(use-package vterm
  :ensure-system-package (("/usr/lib64/libvterm.so.0" . libvterm) ; Specifically for Fedora
                          (libtool)
                          ("/usr/include/vterm.h" . "libvterm-devel")
                          (cmake))
  :hook (vterm-mode . (lambda ()
                        (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
                        (buffer-face-mode t)
                        (face-remap-add-relative 'default :height 119)) ; Change default face size
                    )
  :custom
  (vterm-kill-buffer-on-exit nil)
  (vterm-copy-exclude-prompt t)
  (vterm-min-window-width 50)
  )

;;;; Vterm-toggle
;; Eshell-toggle but for vterm
(use-package vterm-toggle
  :config
  (kb/leader-keys
    :keymaps 'vterm-mode-map
    :states '(normal motion visual)
    "vp" '(vterm-toggle-backward :which-key "Prev vterm buffer")
    "vn" '(vterm-toggle-forward :which-key "Prev vterm buffer")
    "vd" '(vterm-toggle-insert-cd :which-key "Cd to current buffer dir")
    )

  (kb/leader-keys
    "ot" '(vterm-toggle :which-key "Vterm-toggle")
    "oT" '(vterm :which-key "Vterm in current window")
    )
  )

;;; shell-vterm-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-vterm-rcp)
