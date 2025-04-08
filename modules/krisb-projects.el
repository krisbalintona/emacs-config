;;; Project.el
(use-package project
  :bind ( :map project-prefix-map
          ("e" . project-eshell))
  :custom
  (project-vc-extra-root-markers '("Makefile"))
  (project-vc-merge-submodules nil)

  (project-file-history-behavior 'relativize)

  (project-mode-line t)
  (project-mode-line-face nil)

  ;; The commands in `project-switch-commands' must be found in
  ;; `project-prefix-map'
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-vc-dir "VC-Dir")
     (project-eshell "Eshell")
     (eat-project "EAT")
     (project-any-command "Other")))
  :config
  ;; On startup, remove non-existent directories from remembered projects list
  (project-forget-zombie-projects))

;;; Goto definition
;;;; Xref
(use-package xref
  :bind ("C-M-?". xref-find-references-and-replace) ; Emacs 29.1
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function #'xref-show-definitions-buffer)
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep)
  (xref-history-storage 'xref-window-local-history) ; Per-window history of `xref-go-*'
  :config
  ;; We remove the fallback backend, `etags--xref-backend', which prompts the
  ;; user for an etags table -- this is undesirable for me.
  (setq-default xref-backend-functions nil)
  ;; Then add `elisp--xref-backend' as the global value of
  ;; `xref-backend-functions', which means it is run when the local value ends
  ;; with `t'. See (info "(elisp) Running Hooks") for an explanation.
  (add-hook 'xref-backend-functions #'elisp--xref-backend)

  ;; Revealing headings
  (with-eval-after-load 'krisb-reveal
    (defun krisb-reveal-xref-find-information ()
      "Return information required by `krisb-reveal-fold-commands'.
See the docstring of `krisb-reveal-fold-commands'."
      (save-window-excursion
        (save-excursion
          (xref-goto-xref)
          (cons (point) (current-buffer)))))
    ;; I could also advise the following commands to call
    ;; `xref-show-location-at-point' afterwards.  Though such a solution is
    ;; applicable only to xref.  I wanted similar functionality for non-xref
    ;; buffers, so I wrote krisb-reveal, and to remain idiomatic with my usage
    ;; of it, I also do it here.
    (dolist (command '(xref-prev-line
                       xref-next-line
                       xref-quit-and-goto-xref))
      (add-to-list 'krisb-reveal-fold-commands
                   (list :command command
                         :location #'krisb-reveal-xref-find-information)))
    (add-hook 'xref-after-jump-hook #'krisb-reveal-fold)))

;;;; Consult-xref-stack
(use-package consult-xref-stack
  :vc ( :url "https://github.com/brett-lempereur/consult-xref-stack"
        :rev :newest)
  :bind (([remap xref-go-back] . krisb-consult-xref-stack-backward)
         ([remap xref-go-forward] . krisb-consult-xref-stack-forward))
  :config
  (defun krisb-consult-xref-stack-backward (arg)
    "Call `xref-go-back' or `consult-xref-stack-backward' when called with ARG."
    (interactive "p")
    (call-interactively
     (if (< 1 arg) 'consult-xref-stack-backward 'xref-go-back)))

  (defun krisb-consult-xref-stack-forward (arg)
    "Call `xref-go-forward' or `consult-xref-stack-forward' when called with ARG."
    (interactive "p")
    (call-interactively
     (if (< 1 arg) 'consult-xref-stack-forward 'xref-go-forward))))

;;;; Dumber-jump
;; A lean fork of dumb-jump.  For a list of supported languages, see
;; https://github.com/zenspider/dumber-jump?tab=readme-ov-file#supported-languages.
(use-package dumber-jump
  :ensure-system-package (rg . ripgrep)
  :custom
  (dumber-jump-default-project user-emacs-directory)
  :init
  ;; Add to global value so it is used as a fallback (when local value ends in
  ;; t)
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumber-jump-xref-activate 100))
  :config
  (setopt dumber-jump-project-denoters
          (cl-remove-duplicates
           (append dumber-jump-project-denoters project-vc-extra-root-markers))))

;;; LSP integration

;;;; Eglot
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

;;;; Lsp-bridge
;; Asynchronous alternative LSP integration.  The asynchronism is at a cost: its
;; UI is bespoke.  To use, make sure to follow the install instructions in the
;; package README.

;; To install the required python packages system-wide try something like:
;;     paru -S python-epc python-orjson python-sexpdata python-six \
;;     python-setuptools python-paramiko python-rapidfuzz python-watchdog \
;;     python-packaging
(use-package lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge.git")
  :init
  (package-install 'yasnippet)
  (package-install 'markdown-mode))

;;; Debugging
;;;; Gud
(use-package gud
  :ensure nil
  :custom
  (gud-highlight-current-line t))

;;;; Realgud
(use-package realgud
  :hook (realgud-srcbuf-mode . tool-bar-mode)
  :custom
  (realgud-window-split-orientation 'horizontal)
  (realgud-short-key-on-tracing? t))

;;;; Gdb-mi
;; A graphical interface to gdb.
(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  (gud-gdb-command-name "gdb -i=mi --quiet")
  (gdb-restore-window-configuration-after-quit 'if-gdb-many-windows)
  :config
  (defun krisb-gdb-non-stop-handler ()
    "Version of the original that avoids the GDB startup error regarding \"target-async\"."
    (goto-char (point-min))
    (if (re-search-forward "No symbol" nil t)
        (progn
          (message
           "This version of GDB doesn't support non-stop mode.  Turning it off.")
          (setq gdb-non-stop nil)
          (setq gdb-supports-non-stop nil))
      (setq gdb-supports-non-stop t)
      ;; (gdb-input "-gdb-set target-async 1" 'ignore)
      (gdb-input "-gdb-set mi-async 1" 'ignore) ; Change to this, as advised
      (gdb-input "-list-target-features" 'gdb-check-target-async)))
  (advice-add 'gdb-non-stop-handler :override #'krisb-gdb-non-stop-handler))

;;; Provide
(provide 'krisb-projects)
