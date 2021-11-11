;;; kb-doom-modeline-segments.el --- Summary
;;
;;; Commentary:
;;
;; Where I define my own Doom modeline segments and modelines.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'doom-modeline)

;;; Buffer-info
(doom-modeline-def-segment kb/buffer-info
  "The standard `buffer-info' but without the 'unsaved' icon and major mode
icon."
  (concat
   (doom-modeline-spc)
   (doom-modeline--buffer-name)
   ))

;;; Major-mode-icon
(doom-modeline-def-segment kb/major-mode-icon
  "Simply the major-mode icon."
  (doom-modeline--buffer-mode-icon)
  )

;;; Matches
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

;;; VCS
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

;;; Eyebrowse
(doom-modeline-def-segment kb/eyebrowse
  "Show eyebrowse workspace information."
  (when (and (doom-modeline--active)
             (not doom-modeline--limited-width-p))
    '((eyebrowse-mode (:eval (eyebrowse-mode-line-indicator))))
    ))

;;; Time
(doom-modeline-def-segment kb/time
  "Display time."
  (when (and (doom-modeline--active)
             (not doom-modeline--limited-width-p))
    '(("" display-time-string)
      " ")
    ))

;;; Mu4e
(doom-modeline-def-segment kb/mu4e
  "Display mu4e mail. Require `mu4e-alert'."
  (when (and (doom-modeline--active)
             (not doom-modeline--limited-width-p))
    '((:eval mu4e-alert-mode-line))
    ))

;;; Buffer-encoding
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

;;; Buffer-default-directory
(doom-modeline-def-segment kb/buffer-default-directory
  "Standard `buffer-default-directory' without the state, icon, and color change."
  (let* ((active (doom-modeline--active))
         (face (if active 'bold-italic 'mode-line-inactive))) ; Don't use here
    (concat (doom-modeline-spc)
            ;; NOTE 2021-09-03: Add support for org-roam node titles.
            (propertize (cond ((not buffer-file-name) ; Not visiting file
                               "")
                              ((project-current) ; Project root
                               (abbreviate-file-name (vc-git-root (buffer-file-name))))
                              (buffer-file-name ; Current directory
                               default-directory)
                              )
                        'face face)
            (doom-modeline-spc))))

;;; Modeline definitions
(doom-modeline-def-modeline 'main
  '(kb/matches "   " kb/major-mode-icon "  " bar "  " kb/eyebrowse kb/vcs kb/buffer-default-directory kb/buffer-info remote-host buffer-position " " selection-info)
  '(input-method process github debug kb/time battery " " bar " " lsp kb/buffer-encoding checker))

(doom-modeline-def-modeline 'special
  '(bar window-number modals matches buffer-info buffer-position word-count parrot selection-info)
  '(objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process))

(doom-modeline-def-modeline 'vcs
  '(kb/matches "   " kb/major-mode-icon "  " bar "  " kb/eyebrowse kb/vcs kb/buffer-default-directory kb/buffer-info remote-host buffer-position " " selection-info)
  '(input-method process github debug kb/time battery " " bar " " lsp kb/buffer-encoding checker))

(doom-modeline-def-modeline 'info
  '(kb/matches "   " kb/major-mode-icon "  " bar "  " kb/eyebrowse kb/vcs info-nodes kb/buffer-info remote-host buffer-position " " selection-info)
  '(input-method process github debug kb/time battery " " bar " " lsp kb/buffer-encoding checker))

(doom-modeline-def-modeline 'pdf
  '(bar window-number matches buffer-info pdf-pages)
  '(misc-info major-mode process vcs))

(doom-modeline-def-modeline 'org-src
  '(bar window-number modals matches buffer-info-simple buffer-position word-count parrot selection-info)
  '(objed-state misc-info debug minor-modes input-method indent-info lsp buffer-encoding major-mode process checker))

(doom-modeline-def-modeline 'timemachine
  '(bar window-number matches git-timemachine buffer-position word-count parrot selection-info)
  '(misc-info minor-modes indent-info buffer-encoding major-mode))

;;; kb-doom-modeline-segments.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-doom-modeline-segments)
