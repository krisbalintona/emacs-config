;;; garbage-collection-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Faster Emacs startup and session.
;; Other things that can help:
;; 1) Native compilation (`libgccjit')
;; 2) Native (`libjansson') JSON support (alternative Elisp parser)
;;
;; The flags I use when manually compiling Emacs are:
;; ./configure --with-dbus --with-gif --with-jpeg --with-png --with-rsvg \
;; --with-tiff --with-xft --with-xpm --with-gpm=no --with-xwidgets \
;; --with-modules --with-native-compilation --with-pgtk --with-mailutils \
;; --with-json
;; This follows ./autogen.sh. Then I run make -j$(nproc) and then make install.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(setq garbage-collection-messages t)

;;;; GCMH
;; Garbage collect on when idle
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  ;; Set to Doom's threshold. For a related discussion, see
  ;; https://www.reddit.com/r/emacs/comments/bg85qm/comment/eln27qh/?utm_source=share&utm_medium=web2x&context=3
  ;; (gcmh-high-cons-threshold (* 138        ; mb
  ;;                              1024 1024))
  (gcmh-high-cons-threshold (* 50        ; mb
                               1024 1024))
  (gcmh-idle-delay 3)
  (gcmh-verbose nil)
  :config
  (setq garbage-collection-messages nil))

;;; Diagnose memory usage
;; See how Emacs is using memory. From
;; https://www.reddit.com/r/emacs/comments/ck4zb3/comment/evji1n7/?utm_source=share&utm_medium=web2x&context=3
(defun kb/diagnose-garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))

;;; garbage-collection-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'garbage-collection-rcp)
