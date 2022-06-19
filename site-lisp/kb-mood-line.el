;;; kb-mood-line.el --- Summary
;;
;;; Commentary:
;;
;; My custom configuration for my Mood mode-line.
;; TODO 2022-01-22: Would like to remove all Doom Modeline dependency from this
;; modeline config one day, but it will take some time...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'mood-line)

;;; Segments
;;;; Left
(defun kb/mood-line-segment-vc (&rest _)
  "Print git information (e.g. branch, conflicts). Hide text if on
main branch of repository."
  ;; NOTE 2022-01-22: Almost all of this function is taken from my modified
  ;; version of Doom Modeline's VC modeline segment.
  (let ((backend (vc-backend buffer-file-name))
        (icon (concat doom-modeline--vcs-icon " "))
        (text (concat doom-modeline--vcs-text " ")))
    (concat
     (if (doom-modeline--active)
         icon
       (doom-modeline-propertize-icon icon 'mode-line-inactive))
     ;; If the current branch is the main one, then don't show in modeline
     (unless (= 1 (length (vc-git-branches)))
       (if (doom-modeline--active)
           text
         (propertize text 'face 'mode-line-inactive))))))

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
fallback."
  (when-let* ((active
               (doom-modeline--active))
              (face
               (if active 'doom-modeline-project-dir 'mode-line-inactive))
              (file-name
               (file-local-name (or (buffer-name (buffer-base-buffer)) ; Indirect buffers
                                    (buffer-file-name)                 ; Real buffers
                                    "")))    ; Nothing if neither
              (root (if (project-current)
                        (project-root (project-current))
                      default-directory))
              (relative-path
               (file-relative-name default-directory root))
              (directory (cond ((or (string-match-p "\\*.*\\*" (buffer-name))
                                    (string= file-name ""))
                                "")
                               ((project-current)      ; If in project root
                                ;; Modified version of the truncate-with-project style in
                                ;; `doom-modeline-buffer-file-name'
                                (concat
                                 (file-name-nondirectory (directory-file-name root)) ; Add project root
                                 "/"
                                 (unless (string= relative-path "./") ; Add relative path
                                   (substring (shrink-path--dirs-internal relative-path t) 1))))
                               (t                   ; Default to current directory, abbreviated
                                (abbreviate-file-name default-directory)))))
    (propertize directory 'face face)))

(defun kb/mood-line-segment-buffer-name ()
  "Display buffer name.

If mode line is inactive, use a the `mode-line-inactive' face
instead."
  (let* ((active (doom-modeline--active)) ; `doom-modeline' dependency
         (face (if active
                   'mood-line-buffer-name
                 '(:inherit (mode-line-inactive mood-line-buffer-name))
                 )))
    ;; TODO 2021-09-03: Add support for org-roam node titles.
    (propertize "%b" 'face face)
    ))

(defun kb/mood-line-segment-remote-host ()
  "Hostname for remote buffers."
  (when-let ((default-directory)
             (host (file-remote-p default-directory 'host)))
    (propertize (concat "@" host) 'face
                (if (doom-modeline--active)
                    '(:inherit mode-line-emphasis :slant italic)
                  '(:inherit (mode-line-inactive mode-line-emphasis) :slant italic)
                  ))
    ))

(defun kb/mood-line-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (cond
   ((and buffer-read-only (buffer-file-name))
    (propertize "■" 'face 'mood-line-unimportant))
   ((when (or (buffer-file-name)
              (buffer-base-buffer))
      (buffer-modified-p))
    (propertize "●" 'face 'mood-line-modified))
   (t
    " ")))

(defun kb/mood-line-segment-position ()
  "Displays the current cursor position in the mode-line."
  (let ((face (if (doom-modeline--active)
                  'mood-line-unimportant
                'mode-line-inactive)))
    (propertize
     (concat "%l:%c" (when mood-line-show-cursor-point (format ":%d" (point))))
     'face
     `(:inherit ,face :height 0.9)
     )))

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
       'face '(:inherit mode-line-emphasis :height 0.85)))
    ))

;;;; Right
(defun kb/mood-line-segment-which-func ()
  "Display a propertized `which-function-mode' indicator."
  (let ((text (concat "("
                      (string-replace "%" "%%" ; Check `which-func-current'
                                      (or
                                       (gethash
                                        (selected-window)
                                        which-func-table)
                                       which-func-unknown))
                      ")"))
        )
    (cond
     ((equal text "(???)")              ; Don't show if empty
      "")
     ((not (or (derived-mode-p 'prog-mode) ; Only show in these major modes
               (derived-mode-p 'text-mode)
               ))
      "")
     ((doom-modeline--active)
      (propertize text 'face
                  '(:inherit mood-line-unimportant :height 0.85)))
     (t
      (propertize text 'face
                  '(:inherit mode-line-inactive :height 0.85)))
     )))

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

(defun kb/mood-line-segment-flymake ()
  "Displays information about flymake.

Also see `flymake-mode-line-format' and
`flymake-mode-line-counter-format'."
  (when (bound-and-true-p flymake-mode)
    (let ((text (concat " "
                        (mood-line--string-trim (format-mode-line flymake-mode-line-format))
                        " ")))
      (if (doom-modeline--active)
          text
        (propertize text 'face 'mode-line-inactive)))))

(defun kb/mood-line-segment-debug ()
  "The current debug state from the debugger.

Debuggers include edebug and dap."
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
  (if (bound-and-true-p lsp-mode)
      (concat
       (if-let ((icon doom-modeline--lsp))
           (if (doom-modeline--active)
               icon
             (doom-modeline-propertize-icon icon 'mode-line-inactive))
         ""
         )
       " ")))

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
(defun kb/mood-line-setup ()
  "Set up `mode-line-format'."
  (mood-line-mode)
  (setq-default mode-line-format
                '((:eval
                   (mood-line--format
                    (format-mode-line
                     '("  "
                       (:eval (doom-modeline--buffer-mode-icon))
                       " "
                       (:eval (let ((text (eyebrowse-mode-line-indicator)))
                                (if (doom-modeline--active)
                                    text
                                  (propertize text 'face 'mode-line-inactive))))
                       " "
                       (:eval (kb/mood-line-segment-vc))
                       (:eval (kb/mood-line-segment-pyvenv-indicator))
                       (:eval (kb/mood-line-segment-default-directory))
                       (:eval (kb/mood-line-segment-buffer-name))
                       (:eval (kb/mood-line-segment-remote-host))
                       " "
                       (:eval (kb/mood-line-segment-modified))
                       " "
                       (:eval (kb/mood-line-segment-position))
                       (:eval (kb/mood-line-segment-selection-info))
                       " "
                       (:eval (mood-line-segment-anzu))
                       (:eval (mood-line-segment-multiple-cursors))
                       ))
                    (format-mode-line
                     '((:eval (kb/mood-line-segment-which-func))
                       (:eval (mood-line-segment-eol))
                       " "
                       display-time-string
                       (:eval (let ((text battery-mode-line-string))
                                (if (doom-modeline--active)
                                    text
                                  (propertize text 'face 'mode-line-inactive))))
                       ;; (:eval (kb/mood-line-segment-flycheck-doom))
                       (:eval (when (bound-and-true-p flycheck-mode)
                                (or (mood-line-segment-flycheck) " ")))
                       (:eval (when (bound-and-true-p flymake-mode)
                                (kb/mood-line-segment-flymake)))
                       (:eval (kb/mood-line-segment-lsp))
                       (:eval (when (bound-and-true-p lsp-mode)
                                (lsp--progress-status)))
                       ;; Shows number of errors like flycheck?
                       (:eval (when (bound-and-true-p lsp-mode)
                                (lsp-modeline--diagnostics-update-modeline)))
                       (:eval (kb/mood-line-segment-debug))
                       (:eval (kb/mood-line-segment-major-mode))
                       " "
                       (:eval (mood-line-segment-process))
                       (:eval (mood-line-segment-encoding))
                       ;; Occasionally check this to see if any new packages have
                       ;; added anything interesting here to add manually. In
                       ;; particular, make sure to check `global-mode-string'.
                       ;; (:eval ;; (mood-line-segment-misc-info))
                       ;; mode-line-misc-info)
                       )))))))

;;; kb-mood-line.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kb-mood-line)
