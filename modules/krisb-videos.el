;;; MPV
;; Dependency for packages that interact with mpv
(use-package mpv
  ;; NOTE 2024-03-31: See
  ;; https://github.com/kljohann/mpv.el/issues/31#issuecomment-1856491594 for
  ;; why I use the latest GitHub version
  :pin melpa
  :bind-keymap ("C-M-s-m" . krisb-mpv-map)
  :custom
  (mpv-default-options (list "--save-position-on-quit"))
  :config
  (require 'krisb-mpv))

;;; Ytdl
(use-package ytdl
  :ensure-system-package (yt-dlp)
  :custom
  (ytdl-command "yt-dlp")
  (ytdl-always-query-default-filename 'yes-confirm)
  (ytdl-music-folder (expand-file-name "~/Music/"))
  (ytdl-video-folder (expand-file-name "~/Videos/"))
  (ytdl-download-types
   `(("Downloads" "d" ytdl-download-folder ytdl-download-extra-args)
     ("Music"  "m" ytdl-music-folder ytdl-music-extra-args)
     ("Videos" "v"  ytdl-video-folder ytdl-video-extra-args)
     ("Temp" "t" ,(expand-file-name "/tmp/") ("-S" "res:720,fps"))))
  :config
  ;; Custom `org-attach' integration
  (defun krisb-ytdl-org-attach (url)
    "Download and video from URL and attach it to `org-attach-dir'.
A modified version of `ytdl-download'."
    (interactive "MProvide URL: ")
    (when (ytdl--youtube-dl-missing-p)
      (error "youtube-dl is not installed."))
    (let* ((dir (or (org-attach-dir) (org-attach-dir-get-create)))
           (destination (expand-file-name (ytdl--get-filename dir url) dir))
           (extra-ytdl-args '("--write-auto-sub" "--write-sub" "--sub-lang" "en" "--convert-subs" "srt" ; Create .srt file
                              ;; Set maximum resolution and file type
                              "-S" "res:720,fps,ext:mp4:m4a"
                              "--recode" "mp4"))
           (dl-type-name "Org-attach"))
      (ytdl--download-async url
                            destination
                            extra-ytdl-args
                            nil
                            dl-type-name)))
  (with-eval-after-load 'org-attach
    (add-to-list 'org-attach-commands
                 '((?Y ?\C-Y) krisb-ytdl-org-attach
                   "Provide a URL and have \"ytdl\" download the corresponding video and attach that file.")
                 t)))

;;; Provide
(require 'krisb-videos)
