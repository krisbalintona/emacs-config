;;; Diminish
(use-package diminish
  :config
  (with-eval-after-load 'subword
    (diminish 'subword-mode))
  (with-eval-after-load 'simple
    (diminish 'visual-line-mode))
  (with-eval-after-load 'face-remap
    (diminish 'buffer-face-mode)))

;;; Time
;; Enable time in the mode-line
(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t)
  (display-time-format "%R")
  (display-time-interval 60)
  (display-time-default-load-average nil)
  (world-clock-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Nicosia" "Nicosia (capital of Cyprus)")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Tokyo" "Tokyo")
     ("Asia/Shanghai" "Beijing")))
  :config
  (display-time-mode 1))

;;; Recursion-indicator
(use-package recursion-indicator
  :custom
  (recursion-indicator-symbols
   '((completion "C" recursion-indicator-completion)
     (prompt     "P" recursion-indicator-prompt)
     (suspend    "S" recursion-indicator-suspend)
     (t          "R" recursion-indicator-default)))
  :config
  (recursion-indicator-mode 1)
  (minibuffer-depth-indicate-mode -1))

;;; Provide
(provide 'krisb-mode-line)
