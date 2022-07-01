;;; programming-eglot-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All configuration related to eglot.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Eglot
(use-package eglot
  :ensure-system-package (pyright bash-language-server)
  :hook (((python-mode lua-mode sh-mode js2-mode c-mode) . eglot-ensure)
         (eglot-managed-mode . (lambda ()
                                 "Configure `eldoc'"
                                 (setq-local eldoc-box-cleanup-interval 2
                                             eldoc-echo-area-use-multiline-p nil))))
  :general (:keymaps 'eglot-mode-map
            :prefix "<f3>"
            "r" 'eglot-rename
            "a" 'eglot-code-actions
            "=" 'eglot-format
            "s" 'eglot-shutdown
            "S" 'eglot-shutdown-all)
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.3)
  (eglot-extend-to-xref t)              ; Testing to see what this does
  (eglot-stay-out-of '("flymake"))
  :custom-face
  (eglot-highlight-symbol-face ((t (:box (:line-width -1 :style nil)))))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              "Add `eglot-flymake-backend' to `flymake-diagnostic-functions',
preserving the initial list."
              (when (eglot-managed-p)
                (add-to-list 'flymake-diagnostic-functions 'eglot-flymake-backend))))

  ;; Workaround for many hyphen characters wrapping in an ugly way in
  ;; `eldoc-box' frame
  (defun kb/eglot--format-markup (markup)
    "Format MARKUP according to LSP's spec."
    (pcase-let ((`(,string ,mode)
                 (if (stringp markup) (list markup 'gfm-view-mode)
                   (list (plist-get markup :value)
                         (pcase (plist-get markup :kind)
                           ("markdown" 'gfm-view-mode)
                           ("plaintext" 'text-mode)
                           (_ major-mode))))))
      (with-temp-buffer
        (setq-local markdown-fontify-code-blocks-natively t)

        ;; Replace the horizontal rule, which is three hyphens in the markup,
        ;; with X number of hyphens-like characters, with X being enough to
        ;; cover the width of `eldoc-box-max-pixel-width'. We can't simply
        ;; replace with more hyphens since `gfm-view-mode' renders any set of
        ;; three hyphens as a horizontal rule
        (setq string (string-replace "---"
                                     (make-string (floor (/ eldoc-box-max-pixel-width (window-font-width))) ?⎺)
                                     string))

        (insert string)
        (delete-trailing-whitespace) ; Also remove trailing whitespace while we're here
        (let ((inhibit-message t)
              (message-log-max nil))
          (ignore-errors (delay-mode-hooks (funcall mode))))
        (font-lock-ensure)
        (string-trim (buffer-string)))))
  (advice-add 'eglot--format-markup :override #'kb/eglot--format-markup))

;;; Languages
;;;; Eglot-java
(use-package eglot-java
  :ensure-system-package jdtls
  :requires eglot
  :hook ((java-mode . eglot-ensure))
  :custom
  (eglot-java-server-install-dir (no-littering-expand-var-file-name "eglot-java/"))
  (eglot-java-workspace-folder (expand-file-name eglot-java-server-install-dir "workspace/"))
  (eglot-java-eclipse-jdt-args (list "--enable-preview"))
  (eglot-java-prefix-key "C-c e")
  (eglot-java-default-bindings-enabled t) ; See `eglot-java--setup'
  :config
  (eglot-java-init))                    ; Add to hook to `java-mode-hook'

;;; Consult-eglot
;; Equivalent to `consult-lsp'; adds `consult-eglot-symbols'.
(use-package consult-eglot
  :demand t
  :requires consult
  :after eglot
  :general (:keymaps 'eglot-mode-map
            [remap xref-find-apropos] #'consult-eglot-symbols))

;;; programming-eglot-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-eglot-rcp)
