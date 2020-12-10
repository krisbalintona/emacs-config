;;; themes-rcp.el --- Summary
;;
;; Here are all the themes that interest me. One is enabled and all the others
;; are disabled. I've also added my doom-modeline configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Emacs themes
(use-package doom-themes
  :disabled t
  :config (load-theme 'doom-dracula t))

(use-package doom-themes
  :disabled t
  :config (load-theme 'doom-palenight t))

(use-package atom-one-dark-theme
  :config (load-theme 'atom-one-dark t))

(use-package mood-one-theme
  :disabled t
  :config (load-theme 'mood-one t))

(use-package spacemacs-theme
  :disabled t
  :config (load-theme 'spacemacs-dark t))

;;;; Doom-modeline
;; Sleek modeline from Doom Emacs
(use-package doom-modeline
  :after faces-rcp
  :custom
  ;; Modeline settings
  (doom-modeline-window-width-limit fill-column) ; The limit of the window width.
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  ;; (doom-modeline-icon (display-graphic-p)) ; Show icons if in Emacs GUI
  (doom-modeline-icon t) ; In order to work with Emacsclient
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(LaTeX-mode markdown-mode gfm-mode org-mode))
  (doom-modeline-number-limit 99)
  (doom-modeline-vcs-max-length 28)
  (doom-modeline-lsp t)
  (doom-modeline-height 33)
  (doom-modeline-bar-width 0)
  :config
  (if (daemonp) ; Hooks depending on daemon or not
      (progn (add-hook 'server-after-make-frame-hook 'doom-modeline-mode 100)
             (add-hook 'window-setup-hook 'doom-modeline-mode))
    (add-hook 'window-setup-hook 'doom-modeline-mode)) ; Use this hook to prevent right side from being clipped

  (set-face-attribute 'mode-line nil :family kb/modeline-font :height 0.75)
  (set-face-attribute 'mode-line-inactive nil :family kb/modeline-font :height 0.68)
  (doom-modeline-def-modeline 'main
    '(bar " " matches vcs " " buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info " " battery " " input-method buffer-encoding major-mode checker minor-modes process))
  )

;;;;; Modeline additions
;; Minor modeline additions/settings
(with-eval-after-load 'doom-modeline
  (setq find-file-visit-truename t) ; Show actual path of file in symlinks
  (display-time-mode t) ; Enable time in the mode-line
  (size-indication-mode t) ; Show file-size

  ;; Show battery
  (use-package battery
    :straight nil
    :custom
    (battery-load-critical 15)
    (battery-load-low 25)
    :config
    (unless (equal "Battery status not available"
                   (battery))
      (display-battery-mode t)) ; Show battery in modeline
    )
  )

;; Don't show encoding on modeline if it is UTF-8
(defun doom-modeline-conditional-buffer-encoding ()
  "Don't show encoding on modeline if it is UTF-8."
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-conditional-buffer-encoding) ; Necessary so it takes affect imediately, not before I change major modes for the first time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'themes-rcp)
;;; Commentary:
;;
;;; themes-rcp.el ends here
