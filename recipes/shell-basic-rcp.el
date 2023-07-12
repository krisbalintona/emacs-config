;;; shell-basic-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Configuration and packages related to the basic `shell-mode'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Shell
;; Built-in shell
(use-package shell
  :elpaca nil
  :custom
  (async-shell-command-buffer 'new-buffer)) ; Don't ask, just do

;;; Topsy
;; Stick a shell prompt, defun line, etc to the header line
(use-package topsy
  :hook ((eshell-mode magit-section-mode comint-mode term-mode shell-mode) . topsy-mode)
  :custom
  (topsy-mode-functions
   '((emacs-lisp-mode . topsy--beginning-of-defun)
     (magit-section-mode . topsy--magit-section)
     (org-mode . (lambda ()
                   "topsy: Please use package `org-sticky-header' for Org mode"))
     ;; My functions
     (comint-mode . (lambda ()
                      (when (> (window-start) 1)
                        (save-excursion
                          (goto-char (window-start))
                          (goto-char (comint-previous-prompt 1))
                          (font-lock-ensure (point) (point-at-eol))
                          (buffer-substring (point) (point-at-eol))))))
     (shell-mode . (lambda ()
                     (when (> (window-start) 1)
                       (save-excursion
                         (goto-char (window-start))
                         (goto-char (comint-previous-prompt 1))
                         (font-lock-ensure (pos-bol) (point-at-eol))
                         (buffer-substring (pos-bol) (point-at-eol))))))
     (eshell-mode . (lambda ()
                     (when (> (window-start) 1)
                       (save-excursion
                         (goto-char (window-start))
                         (goto-char (eshell-previous-prompt 1))
                         (font-lock-ensure (pos-bol) (point-at-eol))
                         (buffer-substring (pos-bol) (point-at-eol))))))
     (nil . topsy--beginning-of-defun))))

;;; shell-basic-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-basic-rcp)
