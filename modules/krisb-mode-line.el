;;; Hide-mode-line
(use-package hide-mode-line
  :bind ( :map krisb-toggle-keymap
          ("m" . hide-mode-line-mode)))

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

;;; Mode line format
(setq mode-line-defining-kbd-macro (propertize " Macro" 'face 'mode-line-emphasis))

(setopt mode-line-compact 'long         ; Emacs 28
        mode-line-right-align-edge 'window
        mode-line-percent-position '(-3 "%p") ; Don't show percentage of position in buffer
        mode-line-position-line-format '(" %l")
        mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                  mode-line-window-dedicated)
                 display (min-width (6.0)))
                mode-line-frame-identification
                mode-line-buffer-identification "   "
                mode-line-position
                mode-line-format-right-align
                (project-mode-line project-mode-line-format) "   "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; Add segments to `global-mode-string'
(add-to-list 'global-mode-string '(vc-mode (:eval (concat vc-mode " "))))

;;; Provide
(provide 'krisb-mode-line)
