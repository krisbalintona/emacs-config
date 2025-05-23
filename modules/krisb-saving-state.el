;; -*- lexical-binding: t; -*-

;;; Register
(use-package register
  :ensure nil
  :custom
  (register-preview-delay 0)
  (register-separator "  ")
  (register-use-preview 'traditional)
  (register-preview-display-buffer-alist
   '(display-buffer-at-bottom
     (window-height . fit-window-to-buffer)
     (preserve-size . (nil . t))
     (window-parameters . ((mode-line-format . none)
                           (no-other-window . t)))))
  :config
  (with-eval-after-load 'consult
    ;; Better than `consult-register'
    (setq register-preview-function #'consult-register-format)
    ;; Adds thin lines, sorting and hides the mode line of the register preview
    ;; window. Copied from https://github.com/minad/consult#use-package-example
    (advice-add #'register-preview :override #'consult-register-window)))

;;; Provide
(provide 'krisb-saving-state)
