;; -*- lexical-binding: t; -*-

;;; Citar-org-node
(use-package citar-org-node
  :ensure nil
  :load-path "/home/krisbalintona/emacs-repos/packages/citar-org-node/"
  :after (:any citar org-node)
  :demand t
  :diminish
  :bind ( :map krisb-note-keymap
          ("b a" . citar-org-node-add-refs)
          ("b o" . citar-org-node-open-resource))
  :config
  (citar-org-node-mode 1))

;;; Provide
(provide 'krisb-org-node)
