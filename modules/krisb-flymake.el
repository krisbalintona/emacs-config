;; -*- lexical-binding: t; -*-

;;; Flymake-collection
(use-package flymake-collection
  ;; For AUR:
  ;; :ensure-system-package vale
  :after flymake
  :custom
  (flymake-collection-hook-inherit-config t)
  (flymake-collection-hook-ignore-modes nil)
  (flymake-collection-vale-extension-function
   'krisb-flymake-collection-vale-extension-function)
  :init
  (defun krisb-flymake-collection-vale-extension-function (buffer)
    "My own function for `flymake-collection-vale-extension-function'.
Behaves like `flymake-collection-vale-default-extension-function' but
with the following exceptions:
- In org-src buffers, use the extension of the source buffer.
- In org buffers without an associated file, return the \"org\" extension."
    (let* ((file-name (buffer-file-name buffer))
           (extension (and file-name (file-name-extension file-name))))
      (cond
       (extension
        extension)
       ((org-src-edit-buffer-p buffer)
        (file-name-extension
         (buffer-file-name
          (org-src-source-buffer))))
       ((equal 'org-mode (buffer-local-value 'major-mode buffer))
        "org"))))
  :config
  ;; NOTE 2024-10-05: Set `flymake-collection-hook-config' immediately when the
  ;; package loads, so the first invocation of `flymake-collection-hook-setup'
  ;; uses my configured value.  NOTE 2024-10-05: I configure vale to use
  ;; proselint to my liking, so I disable the proselint checker. One reason that
  ;; motivates this decision is vale's performance compared to proselint (see
  ;; https://github.com/errata-ai/vale?tab=readme-ov-file#benchmarks).
  (setf (alist-get 'org-mode flymake-collection-hook-config)
        '((flymake-collection-vale
           :depth -20)
          (flymake-collection-proselint
           :depth -1
           :disabled t))
        (alist-get 'markdown-mode flymake-collection-hook-config)
        '((flymake-collection-markdownlint
           :depth -50)
          (flymake-collection-vale
           :depth -20)
          (flymake-collection-proselint
           :disabled t
           :depth -1))
        (alist-get 'notmuch-message-mode flymake-collection-hook-config)
        '((flymake-collection-vale
           :depth -20)
          (flymake-collection-proselint
           :depth -1
           :disabled t)))

  (flymake-collection-hook-setup))

;;; Provide
(provide 'krisb-flymake)
