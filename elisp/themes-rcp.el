;;; themes-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Here are all the themes that interest me. One is enabled and all the others
;; are disabled. I've also added my doom-modeline configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'fonts-rcp)

;;;; UI
;;;;; Hide-mode-line
;; Hide the modeline when you don't want to see it
(use-package hide-mode-line
  :commands hide-mode-line-mode
  )

;;;;; Transparency
(unless kb/linux-ubuntu
  (set-frame-parameter (selected-frame) 'alpha '(98 . 98))
  (add-to-list 'default-frame-alist '(alpha . (98 . 98)))
  )

;;;; Themes and toggling

;;;;; Install themes
;; (use-package atom-one-dark-theme :demand t)
;; (use-package apropospriate-theme :demand t)

(add-to-list 'custom-theme-load-path "/home/krisbalintona/.emacs.d/elisp/my-themes/")
(require 'uninspiring-dark-theme)
(use-package modus-themes)

;;;;; Variable declarations
(defvar kb/themes-light 'modus-operandi
  "My chosen light theme.")
(defvar kb/themes-dark 'uninspiring-dark
  "My chosen dark theme.")

(defvar kb/themes-hooks nil
  "Hook that runs after the `kb/proper-load-theme-light' and
`kb/proper-load-theme-dark'.")

;;;;; Function definitions
(defun kb/ensure-themes-loaded ()
  "Ensure that the themes in `kb/themes-list' are loaded."
  (unless (or (custom-theme-p kb/themes-dark)
              (custom-theme-p kb/themes-light))
    (load-theme kb/themes-dark t t)
    (load-theme kb/themes-light t t))
  )
(defun kb/proper-load-theme-light ()
  "Properly load `kb/theme-light' theme by disabling its dark counterpart as well.
Additionally, run `kb/themes-hooks'."
  (interactive)
  (disable-theme kb/themes-dark)
  (load-theme kb/themes-light t)
  (run-hooks 'kb/themes-hooks)
  )
(defun kb/proper-load-theme-dark ()
  "Properly load `kb/theme-dark' theme by disabling its light counterpart as well.
Additionally, run `kb/themes-hooks'."
  (interactive)
  (disable-theme kb/themes-light)
  (load-theme kb/themes-dark t)
  (run-hooks 'kb/themes-hooks)
  )

;;;;; Theme switcher
(defun kb/theme-switcher ()
  "Switch between the light and dark themes specified in `kb/themes-list'."
  (interactive)
  (kb/ensure-themes-loaded)
  (let* ((current (car custom-enabled-themes)))
    (cond ((equal kb/themes-light current) (kb/proper-load-theme-dark))
          ((equal kb/themes-dark current) (kb/proper-load-theme-light))
          ))
  )
(general-define-key "<f6>" 'kb/theme-switcher)

;;;;; Load default theme
(kb/proper-load-theme-dark)

;;;; Modeline segments
;; (Re)defining my own modeline segments
(defun kb/set-doom-modeline-segments ()
  "Define relevant doom modeline segments and define segment."
  (require 'doom-modeline-segments)

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

         ;; If the current branch is the main one, then don't show in modeline
         (if (string= (substring-no-properties (substring vc-mode (+ (if (eq (vc-backend buffer-file-name) 'Hg) 2 3) 2))) (magit-main-branch))
             ""
           text)
         (doom-modeline-spc))
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

         ;; Check for UTF-8. If so then do these 2 propertize sections.If not,
         ;; then nothing is propertized and thus shown.
         (if (or (eq buffer-file-coding-system 'utf-8-unix)
                 (eq buffer-file-coding-system 'utf-8)
                 (eq buffer-file-coding-system 'prefer-utf-8-unix)
                 (eq buffer-file-coding-system 'undecided-unix) ; Not sure what this is but it appears in my org-roam files
                 )
             nil

           ;; coding system
           (let* ((sys (coding-system-plist buffer-file-coding-system))
                  (cat (plist-get sys :category))
                  (sym (if (memq cat
                                 '(coding-category-undecided coding-category-utf-8))
                           'failure
                         (plist-get sys :name))))
             (when (or (eq doom-modeline-buffer-encoding t)
                       (and (eq doom-modeline-buffer-encoding 'nondefault)
                            (not (eq cat 'coding-category-undecided))
                            (not (eq sym doom-modeline-default-coding-system))))
               (propertize
                (upcase (symbol-name sym))
                'face face
                'mouse-face mouse-face
                'help-echo 'mode-line-mule-info-help-echo
                'local-map mode-line-coding-system-map)))
           )

         (doom-modeline-spc)))))

  (doom-modeline-def-segment kb/buffer-default-directory
    "Standard `buffer-default-directory' without the state, icon, and color change."
    (let* ((active (doom-modeline--active))
           (face (if active 'bold-italic 'mode-line-inactive))) ; Don't use here
      (concat (doom-modeline-spc)
              (propertize (cond ((stringp (vc-git-root (buffer-file-name)))
                                 (abbreviate-file-name (vc-git-root (buffer-file-name))))
                                (buffer-file-name ; Connected to file?
                                 default-directory)
                                (t "Not file!"))
                          'face face)
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
    '(kb/matches "   " kb/major-mode-icon "  " bar "  " kb/eyebrowse kb/vcs kb/buffer-default-directory kb/buffer-info remote-host buffer-position " " selection-info)
    '(input-method process debug kb/time battery " " bar " " kb/buffer-encoding checker))
  )

;;;;; Time
;; Enable time in the mode-line
(use-package time
  :after doom-modeline
  :hook (window-setup . display-time-mode)
  :custom
  (display-time-format "%H:%M") ; Use 24hr format
  (display-time-default-load-average 1) ; Don't show load average along with time
  )

;;;;; Battery
;; Display batter percentage
(use-package battery
  :straight nil
  :ghook ('doom-modeline-mode-hook 'display-battery-mode)
  :custom
  (battery-load-critical 15)
  (battery-load-low 25)
  )

;;;; Modeline
;;;;; Doom-modeline
;; Sleek modeline from Doom Emacs
(use-package doom-modeline
  :hook (window-configuration-change . doom-modeline-refresh-font-width-cache) ; Prevent modeline from being cut off
  :ghook 'server-after-make-frame-hook 'window-setup-hook
  :gfhook 'kb/set-doom-modeline-segments
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
  )

;;;; Display-line-numbers-mode
;; Show line numbers on the left fringe
(use-package display-line-numbers
  :ghook 'prog-mode-hook 'LaTeX-mode-hook
  ;; :ghook 'prog-mode-hook
  :gfhook 'column-number-mode ; Column number in modeline
  :general (kb/leader-keys
             "tl" '(display-line-numbers-mode :which-key "Line numbers"))
  :custom
  (display-line-numbers-type 'relative)
  )

;;; themes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'themes-rcp)
