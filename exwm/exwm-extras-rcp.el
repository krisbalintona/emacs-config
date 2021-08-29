;;; exwm-extras-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages with supplement my base exwm setup.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Desktop-environment
;; Allow setting desktop environment keybinds (e.g. volume)
(use-package desktop-environment
  :requires exwm
  :after exwm
  :commands desktop-environment-mode
  :general
  (:keymaps 'desktop-environment-mode-map
            ;; Backlight brightness
            "<XF86MonBrightnessUp>" 'desktop-environment-brightness-increment
            "<XF86MonBrightnessDown>" 'desktop-environment-brightness-decrement
            "S-<XF86MonBrightnessUp>" 'desktop-environment-brightness-increment-slowly
            "S-<XF86MonBrightnessDown>" 'desktop-environment-brightness-decrement-slowly
            ;; Keyboard brightness
            "<XF86KbdBrightnessUp>" 'desktop-environment-keyboard-backlight-increment
            "<XF86KbdBrightnessDown>" 'desktop-environment-keyboard-backlight-decrement
            ;; Volume
            "<XF86AudioRaiseVolume>" 'desktop-environment-volume-increment
            "<XF86AudioLowerVolume>" 'desktop-environment-volume-decrement
            "S-<XF86AudioRaiseVolume>" 'desktop-environment-volume-increment-slowly
            "S-<XF86AudioLowerVolume>" 'desktop-environment-volume-decrement-slowly
            "<XF86AudioMute>" 'desktop-environment-toggle-mute
            "<XF86AudioMicMute>" 'desktop-environment-toggle-microphone-mute
            ;; Screenshot
            "S-<print>" 'desktop-environment-screenshot-part
            "<print>" 'desktop-environment-screenshot
            ;; Screen locking
            "s-l" 'windmove-right       ; Manual intervention
            "<XF86ScreenSaver>" 'desktop-environment-lock-screen
            ;; Wifi controls
            "<XF86WLAN>" 'desktop-environment-toggle-wifi
            ;; Bluetooth controls
            "<XF86Bluetooth>" 'desktop-environment-toggle-bluetooth
            ;; Music controls
            "<XF86AudioPlay>" 'desktop-environment-toggle-music
            "<XF86AudioPrev>" 'desktop-environment-music-previous
            "<XF86AudioNext>" 'desktop-environment-music-next
            "<XF86AudioStop>" 'desktop-environment-music-stop
            )
  :custom
  ;; EXWM
  (desktop-environment-update-exwm-global-keys :global)
  ;; Backlight brightness
  (desktop-environment-brightness-small-increment "1%+")
  (desktop-environment-brightness-small-decrement "1%-")
  (desktop-environment-brightness-normal-increment "2%+")
  (desktop-environment-brightness-normal-decrement "2%-")
  ;; Volume
  (desktop-environment-volume-normal-increment "3%+")
  (desktop-environment-volume-normal-decrement "3%-")
  (desktop-environment-volume-small-increment "1%+")
  (desktop-environment-volume-small-decrement "1%-")
  ;; Screenshots
  (desktop-environment-screenshot-command "scrot")
  (desktop-environment-screenshot-directory (expand-file-name "~/Pictures/Screenshots/"))
  (desktop-environment-screenshot-partial-command "flameshot gui")
  ;; NOTE 2021-08-29: Call here (or later), otherwise it won't take the value of
  ;; `desktop-environment-update-exwm-global-keys'
  :config (desktop-environment-mode)
  )

;;;; Edwina
;; DWM-like (dynamic tiling) behavior for windows
(use-package edwina
  :requires exwm
  :ghook 'exwm-init-hook
  :gfhook 'edwina-setup-dwm-keys
  :custom
  (edwina-keymap-prefix (kbd "C-e"))
  (display-buffer-base-action '(display-buffer-below-selected))
  )

;;; exwm-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'exwm-extras-rcp)
