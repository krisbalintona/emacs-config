;;; MPV
;; Dependency for packages that interact with mpv
(use-package mpv
  ;; NOTE 2024-03-31: See
  ;; https://github.com/kljohann/mpv.el/issues/31#issuecomment-1856491594 for
  ;; why I use the latest GitHub version
  :pin melpa
  :custom
  (mpv-default-options (list "--save-position-on-quit")))

;;; Provide
(require 'krisb-videos)
