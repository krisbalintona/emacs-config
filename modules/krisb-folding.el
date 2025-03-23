;;; Outline-indent
(use-package outline-indent
  :hook (prog-mode . outline-indent-minor-mode)
  :bind ( :map krisb-toggle-keymap
          ("o" . krisb-outline-indent-dispatch))
  :config
  (require 'transient)
  (transient-define-prefix krisb-outline-indent-dispatch ()
    "Invoke a transient menu for `tmr'."
    [["At point"
      ("o" "Open" outline-indent-open-fold)
      ("C" "Open recursively" outline-indent-open-fold-rec)
      ("T" "Close" outline-indent-close-fold)]
     ["At point toggle"
      ("t" "Toggle" outline-indent-toggle-fold)
      ("l" "Toggle level at point" outline-indent-toggle-level-at-point)]]
    ["All folds"
     [("O" "Open" outline-indent-open-folds)
      ("C" "Close" outline-indent-close-folds)]]))

;;; Provide
(provide 'krisb-folding)
