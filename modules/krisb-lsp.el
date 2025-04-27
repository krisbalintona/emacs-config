;; -*- lexical-binding: t; -*-

;;; Eglot
(use-package eglot
  :custom
  (eglot-code-action-indications '(eldoc-hint margin))
  (eglot-code-action-indicator "  α ")
  (eglot-sync-connect t)                ; Use `eglot-connect-timeout' seconds
  (eglot-connect-timeout 30)
  (eglot-autoreconnect 10)
  (eglot-events-buffer-config
   (list :size 2000000
         ;; :size 0           ; To boost performance, set size to 0 to stop logging
         :format 'full))
  (eglot-extend-to-xref t))

;;;; Eglot-booster
;; Boosts Eglot's communication with the server. There's also a version for LSP.
(use-package eglot-booster
  ;; NOTE 2024-01-10: Must install the `emacs-lsp-booster' binary from
  ;; https://github.com/blahgeek/emacs-lsp-booster/releases
  :vc (:url "https://github.com/jdtsmith/eglot-booster.git"
            :rev :newest)
  :after eglot
  :config
  (eglot-booster-mode 1))

;;;; Eglot-signature-eldoc-talkative
;; Show documentation of symbols alongside their signature. (By default, only
;; the signature is only shown via `eglot-signature-eldoc-function'.)
(use-package eglot-signature-eldoc-talkative
  :demand t
  :after eglot
  :config
  (advice-add #'eglot-signature-eldoc-function :override #'eglot-signature-eldoc-talkative))

;;; Lsp-bridge
;; Asynchronous alternative LSP integration.  The asynchronism is at a cost: its
;; UI is bespoke.  To use, make sure to follow the install instructions in the
;; package README.

;; To install the required python packages system-wide try something like:
;;     paru -S python-epc python-orjson python-sexpdata python-six \
;;     python-setuptools python-paramiko python-rapidfuzz python-watchdog \
;;     python-packaging
(use-package lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge.git")
  :defer t
  :init
  ;; 2025-04-22: Manually install dependencies because lsp-bridge doesn't have
  ;; proper package headings...
  (dolist (package '(yasnippet markdown-mode))
    (unless (package-installed-p package)
      (package-install package))))

;;; Provide
(provide 'krisb-lsp)
