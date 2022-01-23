;;; kb-mood-line.el --- Summary
;;
;;; Commentary:
;;
;; My custom configuration for my Mood mode-line.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'vc)
(require 'doom-modeline-segments)
(require 'magit)

;;; Segments
;;;; Left
(defun kb/mood-line-segment-vc (&rest _)
  "Print git information (e.g. branch, conflicts). Hide text if on
main branch of repository."
  ;; NOTE 2022-01-22: Almost all of this function is taken from my modified
  ;; version of Doom Modeline's VC modeline segment.
  (if-let ((backend (vc-backend buffer-file-name))
           ;; Hard `doom-modeline' dependency
           (icon (concat doom-modeline--vcs-icon " "))
           (text (concat doom-modeline--vcs-text " "))
           ;; (text (concat mood-line--vc-text "  "))
           )
      (concat
       (propertize
        (if (doom-modeline--active)
            icon
          (doom-modeline-propertize-icon icon 'mode-line-inactive)
          ))
       ;; If the current branch is the main one, then don't show in modeline
       (unless (equal (substring-no-properties (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))
                      (magit-main-branch))
         (if (doom-modeline--active)
             text
           (propertize text 'face 'mode-line-inactive)
           ))
       )
    ""
    ))

(defun kb/mood-line-segment-pyvenv-indicator ()
  "Display the current python virtual environment from `pyvenv'.
Only displays if in a python buffer which has a currently active
virtual environment."
  (when (and (equal major-mode 'python-mode) ; Only show in python-mode
             (not (string-empty-p ; Only if buffer is actually using the current virtual env
                   (file-name-nondirectory
                    (directory-file-name
                     (file-name-directory
                      (directory-file-name default-directory))))
                   )))
    (concat
     "  "
     (mood-line--string-trim (format-mode-line pyvenv-mode-line-indicator))
     "  ")
    ))

(defun kb/mood-line-segment-default-directory ()
  "Display directory.

Don't display if not visiting a real file. Display project root
if in project. Display current directory (`default-directory') as
fallback. "
  (let* ((active (doom-modeline--active)) ; `doom-modeline' dependency
         (face (if active 'mode-line 'mode-line-inactive))
         )
    (propertize (cond ((not buffer-file-name) ; Not visiting file
                       "")
                      ((project-current) ; Project root
                       (abbreviate-file-name (vc-git-root (buffer-file-name))))
                      (buffer-file-name ; Current directory
                       default-directory))
                'face face)
    ))

(defun kb/mood-line-segment-buffer-name ()
  "Display buffer name.

If mode line is inactive, use a the `mode-line-inactive' face
instead."
  (let* ((active (doom-modeline--active)) ; `doom-modeline' dependency
         (face (if active
                   'mood-line-buffer-name
                 '(t (:inherit (mode-line-inactive mood-line-buffer-name)))
                 )))
    ;; TODO 2021-09-03: Add support for org-roam node titles.
    (propertize "%b" 'face face)
    ))

(defun kb/mood-line-segment-remote-host ()
  "Hostname for remote buffers."
  (when default-directory
    (when-let ((host (file-remote-p default-directory 'host)))
      (propertize (concat "@" host) 'face
                  (if (doom-modeline--active)
                      '(t (:inherit mode-line-emphasis :slant italic))
                    '(t (:inherit (mode-line-inactive mode-line-emphasis) :slant italic))
                    ))
      )))

(defun kb/mood-line-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (cond
   ((string-match-p "\\*.*\\*" (buffer-name))
    (doom-modeline-vspc))
   ((buffer-modified-p)
    (propertize "●" 'face 'mood-line-modified)
    )
   ((and buffer-read-only (buffer-file-name))
    (propertize "■" 'face 'mood-line-unimportant))
   (t
    (doom-modeline-vspc)
    (doom-modeline-vspc))
   ))

(defun kb/mood-line-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat "%l:%c" (when mood-line-show-cursor-point
                    (propertize (format ":%d" (point))
                                'face 'mood-line-unimportant))
          ))

(defun kb/mood-line-segment-selection-info ()
  "Show selection info of region."
  ;; NOTE 2022-01-22: Basically verbatim taken from Doom Modeline's
  ;; selection-info segment.
  (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual)))
             (doom-modeline--active))
    (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat " "
                 (cond
                  ((or (bound-and-true-p rectangle-mark-mode) ; Block/rectangle selection
                       (and (bound-and-true-p evil-visual-selection)
                            (eq 'block evil-visual-selection)))
                   (let ((cols (abs (- (doom-modeline-column end)
                                       (doom-modeline-column beg)))))
                     (format "%dx%dB" lines cols)))
                  ((and (bound-and-true-p evil-visual-selection) ; Regular, line selection
                        (eq evil-visual-selection 'line))
                   (format "%dL" lines))
                  ((> lines 1)
                   (format "%dC %dL" (- end beg) lines))
                  (t
                   (format "%dC" (- end beg)))
                  )
                 ;; Append word count, regardless of case
                 (format " %dW" (count-words beg end))
                 " "
                 ))
       'face 'mode-line-emphasis))
    ))

;;;; Right
(defun kb/mood-line-segment-which-func ()
  "Display a propertized `which-function-mode' indicator."
  (if (doom-modeline--active)
      (propertize (format-mode-line which-func-format) 'face
                  '(t (:inherit mood-line-unimportant :height 0.85)))
    (propertize (format-mode-line which-func-format) 'face
                '(t (:inherit mode-line-inactive :height 0.85)))
    ))

(defun kb/mood-line-segment-flycheck-doom ()
  "Displays color-coded error status in the current buffer with
pretty icons -- Doom modeline style."
  (let* ((active (doom-modeline--active))
         (seg `(,doom-modeline--flycheck-icon . ,doom-modeline--flycheck-text))
         (icon (car seg))
         (text (cdr seg))
         )
    (concat
     (when icon
       (if active
           icon
         (doom-modeline-propertize-icon icon 'mode-line-inactive)))
     (when text
       (concat
        " "
        (if active
            text
          (propertize text 'face 'mode-line-inactive))
        ))
     )))

(defun kb/mood-line--update-flycheck-segment (&optional status)
  "Update `mood-line--flycheck-text' against the reported flycheck STATUS."
  ;; Changed text of the original
  (setq mood-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             ;; (propertize (concat " " (number-to-string sum) " ")
                             (propertize " "
                                         'face (if .error
                                                   'mood-line-status-error
                                                 'mood-line-status-warning))))
                       (propertize " " 'face 'mood-line-status-success)))
          ('running (propertize "  " 'face 'mood-line-status-info))
          ('errored (propertize " " 'face 'mood-line-status-error))
          ('interrupted (propertize "⏸ " 'face 'mood-line-status-neutral))
          ('no-checker ""))
        ))
(advice-add 'mood-line--update-flycheck-segment :override #'kb/mood-line--update-flycheck-segment)

(defun kb/mood-line-segment-debug ()
  "The current debug state from debugger (i.e. edebug,
dap)."
  (let* ((dap doom-modeline--debug-dap)
         (edebug (doom-modeline--debug-edebug))
         (on-error (doom-modeline--debug-on-error))
         (on-quit (doom-modeline--debug-on-quit))
         (vsep " ")
         (sep (and
               (or dap edebug on-error on-quit)
               " "))
         (text (concat (and dap (concat dap (and (or edebug on-error on-quit) vsep))) ; For dap
                       (and edebug (concat edebug (and (or on-error on-quit) vsep))) ; For edebug
                       (and on-error (concat on-error (and on-quit vsep)))
                       on-quit
                       sep
                       ))
         )
    (if (doom-modeline--active)
        text
      (propertize text 'face 'mode-line-inactive))
    ))

(defun kb/mood-line-segment-lsp ()
  "The LSP server state."
  ;; NOTE 2022-01-22: Mostly taken from the Doom Modeline LSP segment.
  (if-let ((icon doom-modeline--lsp))
      (if (doom-modeline--active)
          (concat icon " ")
        (doom-modeline-propertize-icon icon 'mode-line-inactive))
    ""
    ))

(defun kb/mood-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (let ((text mode-name))
    (if (doom-modeline--active)
        (format-mode-line text 'mode-line-buffer-id)
      ;; Ensure entire text is inactive. Some major-modes have specialized
      ;; properties (e.g. Elisp/d)
      (propertize (format-mode-line text) 'face 'mode-line-inactive)
      )))

;;; Setting the mode line
(add-hook 'mood-line-mode-hook #'(lambda ()
                                   (setq-default mode-line-format
                                                 '((:eval
                                                    (mood-line--format
                                                     (concat
                                                      "  "
                                                      (doom-modeline--buffer-mode-icon)
                                                      " "
                                                      (let ((text (eyebrowse-mode-line-indicator)))
                                                        (if (doom-modeline--active)
                                                            text
                                                          (propertize text 'face 'mode-line-inactive)))
                                                      " "
                                                      (kb/mood-line-segment-vc)
                                                      (kb/mood-line-segment-pyvenv-indicator)
                                                      (kb/mood-line-segment-default-directory)
                                                      (kb/mood-line-segment-buffer-name)
                                                      (kb/mood-line-segment-remote-host)
                                                      " "
                                                      (kb/mood-line-segment-modified)
                                                      " "
                                                      (kb/mood-line-segment-position)
                                                      " "
                                                      (kb/mood-line-segment-selection-info)
                                                      " "
                                                      (mood-line-segment-anzu)
                                                      (mood-line-segment-multiple-cursors)
                                                      )
                                                     (concat
                                                      (kb/mood-line-segment-which-func)
                                                      (mood-line-segment-eol)
                                                      " "
                                                      display-time-string
                                                      (let ((text (fancy-battery-default-mode-line)))
                                                        (if (doom-modeline--active)
                                                            text
                                                          (propertize text 'face 'mode-line-inactive)))
                                                      " "
                                                      ;; (kb/mood-line-segment-flycheck-doom))
                                                      (let ((text (mood-line-segment-flycheck)))
                                                        (if (doom-modeline--active)
                                                            text
                                                          (propertize text 'face 'mode-line-inactive)))
                                                      (when (bound-and-true-p lsp-mode) ; Error if I don't check for its existence
                                                        lsp-modeline--code-actions-string)
                                                      (mood-line-segment-process)
                                                      (kb/mood-line-segment-lsp)
                                                      (when (bound-and-true-p lsp-mode) ; Error if I don't check for its existence
                                                        (lsp--progress-status))
                                                      ;; (when (bound-and-true-p lsp-mode) ; Error if I don't check for its existence
                                                      ;;   (lsp-modeline--diagnostics-update-modeline)) ; Shows number of errors like flycheck?
                                                      (kb/mood-line-segment-debug)
                                                      (kb/mood-line-segment-major-mode)
                                                      " "
                                                      (mood-line-segment-encoding)
                                                      ;; Occasionally check this to see
                                                      ;; if any new packages have added
                                                      ;; anything interesting here to
                                                      ;; add manually. In particular,
                                                      ;; make sure to check
                                                      ;; `global-mode-string'.
                                                      ;; (:eval
                                                      ;;  ;; (mood-line-segment-misc-info))
                                                      ;;  mode-line-misc-info)
                                                      ))
                                                    )))
                                   ))

;;; kb-mood-line.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-mood-line)
