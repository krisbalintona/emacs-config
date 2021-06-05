;;; themes-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Here are all the themes that interest me. One is enabled and all the others
;; are disabled. I've also added my doom-modeline configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Emacs themes
(use-package doom-themes
  :disabled t
  :config (load-theme 'doom-dracula t))

(use-package doom-themes
  :disabled t
  :config (load-theme 'doom-palenight t))

(use-package atom-one-dark-theme
  :config (load-theme 'atom-one-dark t))

(use-package mood-one-theme
  :disabled t
  :config (load-theme 'mood-one t))

(use-package spacemacs-theme
  :disabled t
  :config (load-theme 'spacemacs-dark t))

;;;; Doom-modeline
;; Sleek modeline from Doom Emacs
(use-package doom-modeline
  :after faces-rcp
  :custom
  ;; Modeline settings
  (doom-modeline-window-width-limit fill-column) ; The limit of the window width.
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  ;; (doom-modeline-icon (display-graphic-p)) ; Show icons if in Emacs GUI
  (doom-modeline-icon t) ; In order to work with Emacsclient
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(LaTeX-mode markdown-mode gfm-mode org-mode))
  (doom-modeline-mu4e nil) ; Requires `mu4e-alert' - flip this value
  (doom-modeline-percent-position nil)
  (doom-modeline-number-limit 99)
  (doom-modeline-vcs-max-length 28)
  (doom-modeline-lsp t)
  (doom-modeline-height 33)
  (doom-modeline-bar-width 2) ; Width (in number of columns) of window until information (on the right) starts to disappear
  (doom-modeline-window-width-limit 100) ; Width of the bar segment
  :config
  (if (daemonp) ; Hooks depending on daemon or not
      (add-hook 'server-after-make-frame-hook 'doom-modeline-mode 100)
    (add-hook 'window-setup-hook 'doom-modeline-mode))

  (set-face-attribute 'mode-line nil :family kb/modeline-font :height 0.77)
  (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)
  )

;;;;; Time
;; Enable time in the mode-line
(use-package time
  :straight nil
  :custom
  (display-time-format "%H:%M") ; Use 24hr format
  (display-time-default-load-average nil) ; Don't show load average along with time
  :config
  (add-hook 'after-init-hook 'display-time-mode)
  )

;;;;; Battery
;; Display batter percentage
(use-package battery
  :straight nil
  :after doom-modeline
  :custom
  (battery-load-critical 15)
  (battery-load-low 25)
  :config
  (unless (equal "Battery status not available"
                 (battery))
    (display-battery-mode t)) ; Show battery in modeline
  )

;;;;; Modeline segments
;; (Re)defining my own modeline segments
(with-eval-after-load 'doom-modeline
  (doom-modeline-def-segment kb/buffer-info
    "The standard `buffer-info' but without the 'unsaved' icon and major mode
icon."
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-name)
     ))
  (doom-modeline-def-segment kb/major-mode-icon
    "Simply the major-mode icon."
    (doom-modeline--buffer-mode-icon)
    )
  (doom-modeline-def-segment kb/matches
    "The standard `matches' segment but doesn't show buffer size (if unused)."
    (let ((meta (concat (doom-modeline--macro-recording)
                        (doom-modeline--anzu)
                        (doom-modeline--phi-search)
                        (doom-modeline--evil-substitute)
                        (doom-modeline--iedit)
                        (doom-modeline--symbol-overlay)
                        (doom-modeline--multiple-cursors))))
      (or (and (not (equal meta "")) meta)
          ""))
    )
  (doom-modeline-def-segment kb/vcs
    "Standard `vcs' but don't show branch if it's 'master'."
    (let ((active (doom-modeline--active)))
      (when-let ((icon doom-modeline--vcs-icon)
                 (text doom-modeline--vcs-text))
        (concat
         (doom-modeline-spc)
         (propertize
          (concat
           (if active
               icon
             (doom-modeline-propertize-icon icon 'mode-line-inactive))
           (doom-modeline-vspc))
          'mouse-face 'mode-line-highlight
          'help-echo (get-text-property 1 'help-echo vc-mode)
          'local-map (get-text-property 1 'local-map vc-mode))
         (if (string= text "master") ; Only print branch if not master
             ""
           (progn (if active
                      text
                    (propertize text 'face 'mode-line-inactive))
                  (doom-modeline-spc))))
        )))
  (doom-modeline-def-segment kb/eyebrowse
    "Show eyebrowse workspace information."
    (when (and (doom-modeline--active)
               (not doom-modeline--limited-width-p))
      '((eyebrowse-mode (:eval (eyebrowse-mode-line-indicator))))
      ))
  (doom-modeline-def-segment kb/time
    "Display time."
    (when (and (doom-modeline--active)
               (not doom-modeline--limited-width-p))
      '(("" display-time-string)
        " ")
      ))
  (doom-modeline-def-segment kb/mu4e
    "Display mu4e mail. Require `mu4e-alert'."
    (when (and (doom-modeline--active)
               (not doom-modeline--limited-width-p))
      '((:eval mu4e-alert-mode-line))
      ))
  (doom-modeline-def-segment kb/buffer-encoding
    "Standard `buffer-encoding' but don't show encoding on modeline if it is
UTF-8."
    (when doom-modeline-buffer-encoding
      (let ((face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))
            (mouse-face 'mode-line-highlight))
        (concat
         (doom-modeline-spc)

         ;; eol type
         (let ((eol (coding-system-eol-type buffer-file-coding-system)))
           (propertize
            (pcase eol
              (0 "LF ")
              (1 "CRLF ")
              (2 "CR ")
              (_ ""))
            'face face
            'mouse-face mouse-face
            'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
                               (pcase eol
                                 (0 "Unix-style LF")
                                 (1 "DOS-style CRLF")
                                 (2 "Mac-style CR")
                                 (_ "Undecided")))
            'local-map (let ((map (make-sparse-keymap)))
                         (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                         map)))

         ;; coding system
         (propertize
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (if (or (eq buffer-file-coding-system 'utf-8-unix) ; Check for UTF-8
                    (eq buffer-file-coding-system 'utf-8)
                    (eq buffer-file-coding-system 'prefer-utf-8-unix))
                nil
              (cond ((memq (plist-get sys :category)
                           '(coding-category-undecided coding-category-utf-8))
                     "UTF-8")
                    (t (upcase (symbol-name (plist-get sys :name)))))))
          'face face
          'mouse-face mouse-face
          'help-echo 'mode-line-mule-info-help-echo
          'local-map mode-line-coding-system-map)

         (doom-modeline-spc)))))
  (doom-modeline-def-segment kb/buffer-default-directory
    "Standard `buffer-default-directory' without the state, icon, and color change."
    (let* ((active (doom-modeline--active))
           (face (if active 'doom-modeline-buffer-path 'mode-line-inactive))) ; Don't use here
      (concat (doom-modeline-spc)
              (propertize (abbreviate-file-name default-directory) 'face 'bold-italic)
              (doom-modeline-spc))))
  (doom-modeline-def-segment me/major-mode
    "The current major mode, including environment information."
    (let* ((active (doom-modeline--active))
           (face (if active 'doom-modeline-buffer-major-mode 'mode-line-inactive)))
      (concat (doom-modeline-spc)
              (propertize (format-mode-line mode-name) 'face face)
              (doom-modeline-spc))))
  (doom-modeline-def-segment kb/major-mode
    "Standard `major-mode' but bolded."
    (propertize
     (concat
      (doom-modeline-spc)
      (propertize (format-mode-line
                   (or (and (boundp 'delighted-modes)
                            (cadr (assq major-mode delighted-modes)))
                       mode-name))
                  'help-echo "Major mode\n\
  mouse-1: Display major mode menu\n\
  mouse-2: Show help for major mode\n\
  mouse-3: Toggle minor modes"
                  'mouse-face 'mode-line-highlight
                  'local-map mode-line-major-mode-keymap)
      (when (and doom-modeline-env-version doom-modeline-env--version)
        (format " %s" doom-modeline-env--version))
      (and (boundp 'text-scale-mode-amount)
           (/= text-scale-mode-amount 0)
           (format
            (if (> text-scale-mode-amount 0)
                " (%+d)"
              " (%-d)")
            text-scale-mode-amount))
      (doom-modeline-spc))
     'face (if (doom-modeline--active)
               '(doom-modeline-buffer-major-mode bold) ; Make bold
             'mode-line-inactive)))

  (doom-modeline-def-modeline 'main
    '(kb/matches "   " kb/major-mode-icon " " kb/mu4e "  " bar "  " kb/eyebrowse kb/vcs kb/buffer-default-directory kb/buffer-info remote-host buffer-position " " selection-info)
    '(input-method process debug kb/time battery " " bar "  " kb/buffer-encoding checker "          "))
  )

;;; themes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'themes-rcp)
