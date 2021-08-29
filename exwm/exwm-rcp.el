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
  "Update the name of a buffer to the program's class (e.g.
Firefox)."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun kb/exwm-update-title ()
  "Update the name of a buffer to the window's title."
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun kb/configure-window-by-class ()
  "Change the workspace a program opens in and how it opens (e.g. floating or not)."
  (interactive)
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 9))
    ;; ("Sol" (exwm-workspace-move-window 3))
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
(setq exwm-systemtray-height 32)
(exwm-systemtray-enable)

;;;;; Variables
(setq exwm-workspace-number 10    ; Default number of workspaces
      exwm-workspace-show-all-buffers t ; Have a separate buffer list for each workspace
      exwm-layout-show-all-buffers nil) ; Only switch to buffers in current workspace
;; NOTE Uncomment this option if you want to detach the minibuffer!
;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
;; (exwm-workspace-minibuffer-position 'top)

;;;;; Keybinds
;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-u
        ?\C-h
        ?\M-x
        ?\M-`
        ?\M-&
        ?\M-:
        ;; ?\C-\M-j                      ; Buffer list
        ;; ?\C-\                         ; Ctrl+Space
        ))

;; Ctrl+Q will enable the next key to be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Set up global key bindings.  These always work, no matter the input state!
;; Keep in mind that changing this list after EXWM initializes has no effect.
(setq exwm-input-global-keys
      `(
        ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        ([?\s-r] . exwm-reset)

        ;; Move between windows
        ([s-left] . windmove-left)
        ([s-right] . windmove-right)
        ([s-up] . windmove-up)
        ([s-down] . windmove-down)

        ;; Launch applications via shell command
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

        ;; Switch workspace
        ([?\s-w] . exwm-workspace-switch)
        ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

        ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

;;;; Finally, enable exwm
(exwm-enable)

;;;; Other helper functions
;; This function isn't currently used, only serves as an example how to
;; position a window
(defun efs/position-window ()
  "Example for how to change the position of a window."
  (let* ((pos (frame-position))
         (pos-x (car pos))
         (pos-y (cdr pos)))

    (exwm-floating-move (- pos-x) (- pos-y))))

;;; exwm-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'exwm-rcp)
