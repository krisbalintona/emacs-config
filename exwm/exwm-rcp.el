;;; exwm-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Setup regarding using the Emacs Window Manager.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'straight-package-management-rcp)
(straight-use-package 'exwm)
(require 'exwm)

;;;; Hooks
;;;;; Functions
(defun kb/exwm-update-class ()
  "Update the name of a buffer to the program's class. This is the
default for every xwindow."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun kb/exwm-update-title ()
  "Update the name of a buffer to the window's title for the given
`exwm-class-name's."
  (pcase exwm-class-name
    ("Brave-browser" (exwm-workspace-rename-buffer (format "Brave-browser: %s" exwm-title)))
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ))

(defun kb/configure-window-by-class ()
  "Change the workspace a program opens in and how it opens (e.g. floating or not)."
  (interactive)
  (pcase exwm-class-name
    ("discord"
     (exwm-workspace-move-window 9)
     (exwm-layout-hide-mode-line))
    ("Brave-browser" (exwm-layout-hide-mode-line))
    ("Firefox" (exwm-layout-hide-mode-line))
    ;; ("mpv" (exwm-floating-toggle-floating)
    ;;  (exwm-layout-toggle-mode-line))
    ))

(defun kb/exwm-init-hook ()
  "A hook for other programs I want run at startup."
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 0)

  ;; ;; Launch apps that will run in the background
  ;; (kb/run-in-background "nm-applet")
  ;; (kb/run-in-background "pasystray")
  ;; (kb/run-in-background "blueman-applet")
  )

;;;;; Declarations
(add-hook 'exwm-update-class-hook #'kb/exwm-update-class) ; When window "class" updates, use it to set the buffer name
(add-hook 'exwm-update-title-hook #'kb/exwm-update-title) ; When window title updates, use it to set the buffer name
(add-hook 'exwm-manage-finish-hook #'kb/configure-window-by-class) ; Configure windows as they're created
(add-hook 'exwm-init-hook #'kb/exwm-init-hook) ; When EXWM starts up, do some extra configuration

;;;; Helper functions
(defun kb/set-wallpaper ()
  "Set my wallpaper."
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
   "feh" nil  "feh --bg-scale /usr/share/backgrounds/matt-mcnulty-nyc-2nd-ave.jpg"))

(defun kb/run-in-background (command)
  "Run a COMMAND asynchronously in the background."
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;;;; Configuration
;;;;; Set screen resolution
(require 'exwm-randr)
(exwm-randr-enable)
(start-process-shell-command "xrandr" nil "xrandr --output eDP --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-A-0 --off")

;;;;; Set wallpaper
;; (kb/set-wallpaper)

;;;;; Load system tray
;; Do it before `exwm-init'
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;;;;; Variables
(setq exwm-workspace-number 10            ; Default number of workspaces
      exwm-workspace-show-all-buffers nil ; Have a separate buffer list for each workspace?
      exwm-layout-show-all-buffers nil ; Move other xwindow to current workspace?
      ;; exwm-workspace-minibuffer-position 'bottom ; Keep at bottom but only show when needed
      exwm-workspace-display-echo-area-timeout 1 ; Timeout echo area in this many seconds
      mouse-autoselect-window t
      focus-follows-mouse t
      )

;;;;; Window resizing with mouse
;; Allow resizing with mouse, of non-floating windows.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

;;;;; Update modeline when changing mode
;; Update modeline when changing from `line-mode' and `char-mode'.
(add-hook 'exwm-input--input-mode-change-hook 'force-mode-line-update)

;;;;; Keybinds
;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
      '(?\C-x
        ?\M-x
        ?\M-\                           ; Alt+Space
        ?\M-`
        ?\M-1
        ?\M-2
        ?\M-3
        ?\M-4
        ?\M-5
        ?\M-6
        ?\M-7
        ?\M-8
        ?\M-9
        ?\M-0
        ))

;; Ctrl+Q will enable the next key to be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Set up global key bindings.  These always work, no matter the input state!
;; Keep in mind that changing this list after EXWM initializes has no effect.
(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset)          ; Resets everything, including to `line-mode'

        ;; Move between windows
        ([?\s-h] . windmove-left)
        ([?\s-l] . windmove-right)
        ([?\s-k] . windmove-up)
        ([?\s-j] . windmove-down)
        ([s-left] . windmove-left)
        ([s-right] . windmove-right)
        ([s-up] . windmove-up)
        ([s-down] . windmove-down)
        ([?\s-w] . evil-window-mru)

        ;; Swap windows
        ([?\s-H] . windmove-swap-states-left)
        ([?\s-L] . windmove-swap-states-right)
        ([?\s-K] . windmove-swap-states-up)
        ([?\s-J] . windmove-swap-states-down)

        ;; Window layouts
        ([?\s-f] . exwm-layout-toggle-fullscreen)

        ;; Launch applications via shell command
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

        ;; ;; Switch workspaces
        ([?\s-W] . exwm-workspace-switch)
        ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ))

;;;;; Application keybindings
(exwm-input-set-key (kbd "s-z") 'counsel-linux-app)
(exwm-input-set-key (kbd "s-b") '(lambda () (interactive) (start-process-shell-command "Rofi-bluetooth" nil "/usr/bin/rofi-bluetooth")))

;;;; Finally, enable exwm
(exwm-enable)

;;; exwm-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'exwm-rcp)
