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

;;; Vterm
;; Full-fledged terminal emulator
(use-package vterm
  :ensure-system-package (("/usr/lib64/libvterm01/" . libvterm) ; Specifically for Fedora
                          (libtool)
                          ("/usr/include/libvterm01/" . "libvterm-devel")
                          (cmake))
  :gfhook
  '(lambda ()
     (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
     (buffer-face-mode t)
     (face-remap-add-relative 'default :height 1.1))
  :general
  (:keymaps 'vterm-mode-map
            :states 'insert
            "<tab>" 'vterm-send-tab)
  (kb/open-keys
    "v" '((lambda ()
            (interactive)
            (vterm (concat "*vterm* "
                           (file-name-nondirectory (directory-file-name (file-name-directory default-directory))))))
          :wk "Vterm"))
  :custom
  (vterm-kill-buffer-on-exit nil)
  (vterm-copy-exclude-prompt t)
  (vterm-timer-delay nil)      ; Make vterm appear less "slow" by removing delay
  :config
  (defun kb/kill-vterm-process-maybe (&optional frame)
    "If the current buffer has a vterm process running, kill both the
process and its buffer without confirmation."
    (let ((kill-buffer-query-functions  ; Suppress asking for confirmation
           (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
      (when (string= major-mode 'vterm-mode)
        (kill-buffer))))
  (add-to-list 'delete-frame-functions #'kb/kill-vterm-process-maybe))

;;; shell-vterm-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-vterm-rcp)
