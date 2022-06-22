;;; programming-lsp-bridge-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All configuration related to lsp-bridge.
;;
;;;;;;;;p;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Lsp-bridge
;; Minimal LSP client whose benefit is asynchrony
(use-package lsp-bridge
  :straight (lsp-bridge :type git
                        :host github
                        :repo "manateelazycat/lsp-bridge"
                        :files (:defaults "*.py" "langserver" "acm"))
  :gfhook '(lambda ()                        ; For Xref support
             (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t))
  :init
  (defun kb/global-lsp-bridge-mode ()
    "My own, non-scuffed version of the command."
    (interactive)
    (dolist (hook lsp-bridge-default-mode-hooks)
      (add-hook hook (lambda () (lsp-bridge-mode 1))))
    (setq lsp-bridge-diagnostics-timer
          (run-with-idle-timer lsp-bridge-diagnostics-fetch-idle t #'lsp-bridge-diagnostics-fetch)))
  :config
  (kb/global-lsp-bridge-mode)

  ;; Enable extension
  (require 'lsp-bridge-jdtls) ; Provide Java third-party library jump and -data directory support, optional
  (require 'lsp-bridge-icon)  ; Show icons for completion items

  ;; For corfu users with HiDPI screen
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3)))))))

;;; programming-lsp-bridge-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-lsp-bridge-rcp)
