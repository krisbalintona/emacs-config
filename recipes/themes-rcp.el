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
(require 'kb-themes)

;;; UI
;;;; Hide-mode-line
;; Hide the modeline when you don't want to see it
(use-package hide-mode-line
  :commands hide-mode-line-mode
  :general ("<f8>" 'hide-mode-line-mode)
  )

;;;; Transparency
(defun kb/set-default-transparency ()
  "Sets default transparency of current and default frame alist."
  (unless kb/linux-ubuntu
    (set-frame-parameter (selected-frame) 'alpha '(98 .98))
    (add-to-list 'default-frame-alist   '(alpha . (98 .98)))
    ))

(defvar alpha-background-transparency t
  "Enable background transparency? Meant for the `alpha-background' parameter.")

(defun kb/toggle-transparency ()
  "Toggle transparency. Requires a patched version of Emacs found
here: https://github.com/TheVaffel/emacs"
  (interactive)
  (if alpha-background-transparency
      (progn (set-frame-parameter (selected-frame) 'alpha-background 75)
             (setq alpha-background-transparency nil)
             (kb/set-default-transparency)
             )
    (progn (set-frame-parameter (selected-frame) 'alpha-background 100)
           (setq alpha-background-transparency t)
           (kb/set-default-transparency))
    ))
(general-define-key "<f7>" 'kb/toggle-transparency)

;;;; Solaire-mode
;; Have "non-real" (by my own predicate) buffers and other faces swapped.
(use-package solaire-mode
  :hook ((window-state-change window-configuration-change) . turn-on-solaire-mode)
  :custom
  (solaire-mode-remap-alist
   '(;; Defaults
     (default . solaire-default-face)
     (hl-line . solaire-hl-line-face)
     (region . solaire-region-face)
     (org-hide . solaire-org-hide-face)
     (org-indent . solaire-org-hide-face)
     (linum . solaire-line-number-face)
     (line-number . solaire-line-number-face)
     (header-line . solaire-header-line-face)
     (mode-line . solaire-mode-line-face)
     (mode-line-active . solaire-mode-line-active-face)
     (mode-line-inactive . solaire-mode-line-inactive-face)
     (highlight-indentation-face . solaire-hl-line-face)
     (fringe . solaire-fringe-face)
     ))
  (solaire-mode-themes-to-face-swap `(,kb/themes-dark ,kb/themes-light))
  (solaire-mode-swap-alist
   '(;; Defaults
     (default . solaire-default-face)
     (hl-line . solaire-hl-line-face)
     (region . solaire-region-face)
     (org-hide . solaire-org-hide-face)
     (org-indent . solaire-org-hide-face)
     (linum . solaire-line-number-face)
     (line-number . solaire-line-number-face)
     (header-line . solaire-header-line-face)
     (mode-line . solaire-mode-line-face)
     (mode-line-active . solaire-mode-line-active-face)
     (mode-line-inactive . solaire-mode-line-inactive-face)
     (highlight-indentation-face . solaire-hl-line-face)
     (fringe . solaire-fringe-face)
     ))
  :init
  ;; NOTE 2022-01-21: Enable `solaire-global-mode' if I want to swap the
  ;; background faces which solaire remaps, e.g., non-real buffers dark and real
  ;; light to non-real light and real dark.
  ;; (solaire-global-mode)
  )

;;; Modeline
;;;; Doom-modeline
;; Sleek modeline from Doom Emacs
(use-package doom-modeline
  :hook (window-configuration-change . doom-modeline-refresh-font-width-cache) ; Prevent modeline from being cut off
  ;; :ghook 'emacs-startup-hook
  :custom
  ;; Modeline settings
  (doom-modeline-window-width-limit fill-column) ; The limit of the window width.
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon (or(display-graphic-p) (server-running-p))) ; Show icons if in Emacs GUI or server
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
  :config (require 'kb-doom-modeline-segments)
  )

;;;; Mood-line
;; Mode line which accompanies the mood-one theme
;; TODO 2022-01-22: Would like to remove all Doom Modeline dependency from this
;; modeline config one day, but it will take some time...
(use-package mood-line
  :hook (mood-line-mode . (lambda ()
                            (setq-default mode-line-format
                                          '((:eval
                                             (mood-line--format
                                              (format-mode-line
                                               '("  "
                                                 (:eval
                                                  (doom-modeline--buffer-mode-icon))
                                                 " "
                                                 (:eval
                                                  (eyebrowse-mode-line-indicator))
                                                 " "
                                                 (:eval
                                                  (kb/mood-line-segment-vc))
                                                 (:eval
                                                  (kb/mood-line-segment-pyvenv-indicator))
                                                 (:eval
                                                  (kb/mood-line-segment-default-directory))
                                                 (:eval
                                                  (kb/mood-line-segment-buffer-name))
                                                 (:eval
                                                  (kb/mood-line-segment-remote-host))
                                                 " "
                                                 (:eval
                                                  (kb/mood-line-segment-modified))
                                                 " "
                                                 (:eval
                                                  (kb/mood-line-segment-position))
                                                 " "
                                                 (:eval
                                                  (kb/mood-line-segment-selection-info))
                                                 (:eval
                                                  (mood-line-segment-anzu))
                                                 (:eval
                                                  (mood-line-segment-multiple-cursors))
                                                 ))
                                              (format-mode-line
                                               '((:eval
                                                  (mood-line-segment-eol))
                                                 " "
                                                 display-time-string
                                                 (:eval
                                                  (fancy-battery-default-mode-line))
                                                 "  "
                                                 (:eval
                                                  ;; (kb/mood-line-segment-flycheck-doom))
                                                  (mood-line-segment-flycheck))
                                                 lsp-modeline--code-actions-string
                                                 (:eval
                                                  (mood-line-segment-process))
                                                 (:eval
                                                  (kb/mood-line-segment-debug))
                                                 (:eval
                                                  (kb/mood-line-segment-lsp))
                                                 (:eval
                                                  (if (bound-and-true-p lsp-mode) ; Check or else warnings will be outputted
                                                      (lsp--progress-status)
                                                    ""))
                                                 (:eval
                                                  (mood-line-segment-major-mode))
                                                 (:eval
                                                  (mood-line-segment-encoding))
                                                 ;; (:eval             ; Shows number of errors like flycheck?
                                                 ;;  (lsp-modeline--diagnostics-update-modeline))
                                                 ;; Occasionally check this to see if any new packages have added
                                                 ;; anything interesting here to add manually. In particular, make
                                                 ;; sure to check `global-mode-string'.
                                                 ;; (:eval
                                                 ;;  ;; (mood-line-segment-misc-info))
                                                 ;;  mode-line-misc-info)
                                                 ))
                                              ))))
                            ))
  :ghook 'after-init-hook
  :config
  (defun kb/mood-line-segment-position ()
    "Displays the current cursor position in the mode-line."
    (concat "%l:%c" (when mood-line-show-cursor-point
                      (propertize (format ":%d" (point))
                                  'face 'mood-line-unimportant))))
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
       "  ")))
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
                  'face face)))
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
      (propertize "%b" 'face face)))
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
  (defun kb/mood-line-segment-lsp ()
    "The LSP server state."
    ;; NOTE 2022-01-22: Mostly taken from the Doom Modeline LSP segment.
    (if-let ((icon (concat doom-modeline--lsp " ")))
        (if (doom-modeline--active)
            icon
          (doom-modeline-propertize-icon icon 'mode-line-inactive))
      ""
      ))
  (defun kb/mood-line--update-flycheck-segment (&optional status)
    "Update `mood-line--flycheck-text' against the reported flycheck STATUS."
    ;; Changed text of the original
    (setq mood-line--flycheck-text
          (pcase status
            ('finished (if flycheck-current-errors
                           (let-alist (flycheck-count-errors flycheck-current-errors)
                             (let ((sum (+ (or .error 0) (or .warning 0))))
                               (propertize (concat " " (number-to-string sum) " ")
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
           (text (concat sep
                         (and dap (concat dap (and (or edebug on-error on-quit) vsep))) ; For dap
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
  )

;;;; Time
;; Enable time in the mode-line
(use-package time
  ;; :hook (window-setup . display-time-mode)
  :custom
  (display-time-format "%H:%M:%S")     ; Use 24hr format with seconds
  (display-time-interval 1)            ; Update every since if I'm using seconds
  (display-time-default-load-average nil) ; Don't show load average
  (world-clock-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Nicosia" "Nicosia (capital of Cyprus)")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Tokyo" "Tokyo")
     ))
  :init
  (display-time-mode)
  )

;;;; Battery
;; Display batter percentage
(use-package battery
  :custom
  (battery-load-critical 15)
  (battery-load-low 25)
  (battery-mode-line-limit 100)
  ;; (battery-mode-line-format "%cmAh")
  (battery-mode-line-format "  %p%%")
  :init
  (display-battery-mode)
  )

;;;; Fancy-battery
(use-package fancy-battery
  :custom
  (fancy-battery-show-percentage t)
  :init
  (fancy-battery-mode)
  )

;;;; Display-line-numbers-mode
;; Show line numbers on the left fringe
(use-package display-line-numbers
  :ghook 'prog-mode-hook 'LaTeX-mode-hook
  :gfhook 'column-number-mode ; Column number in modeline
  :general (kb/toggle-keys
             "l" '(display-line-numbers-mode :which-key "Line numbers"))
  :custom
  (display-line-numbers-type 'relative)
  )

;;; themes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'themes-rcp)
