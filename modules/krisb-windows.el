;; -*- lexical-binding: t; -*-

;;; Window
(use-package window
  :ensure nil
  :bind (([remap other-window] . krisb-other-window-mru)
         :repeat-map other-window-repeat-map
         ("o" . krisb-other-window-mru))
  :custom
  (split-width-threshold 130)
  (split-height-threshold 80)
  (window-sides-vertical t)
  (window-combination-resize t) ; Allow to resize existing windows when splitting?
  (fit-window-to-buffer-horizontally t)

  (switch-to-buffer-in-dedicated-window 'pop)
  :config
  ;; Modified version of "other-window-mru" taken from
  ;; https://karthinks.com/software/emacs-window-management-almanac/#the-back-and-forth-method
  ;; that accepts a prefix arg
  (defun krisb-other-window-mru (&optional arg)
    "Select the most recently used window on this frame."
    (interactive "p")
    (when-let ((windows-by-mru              ; Used `get-mru-window' as a reference
                (sort (delq nil
                            (mapcar
                             (lambda (win)
                               (when (and (not (eq win (selected-window)))
                                          (not (window-no-other-p win)))
                                 (cons (window-use-time win) win)))
                             (window-list-1 nil nil nil)))
                      :lessp #'>
                      :key #'car)))
      (select-window (cdr (nth (1- (min (length windows-by-mru) (or arg 1))) windows-by-mru)))))


  ;; More predictable (at least, if I use `krisb-other-window-mru') window
  ;; selection of `scroll-other-window' and `scroll-other-window-down'.  Taken
  ;; from
  ;; https://karthinks.com/software/emacs-window-management-almanac/#scroll-other-window--built-in
  (setq-default other-window-scroll-default
                (lambda ()
                  (or (get-mru-window nil nil 'not-this-one-dummy)
                      (next-window)             ; Fall back to next window
                      (next-window nil nil 'visible)))))

;;; Stillness-mode
;; This package solves an annoyance: when invoking a tall minibuffer, if there
;; are other windows present with points low enough in their buffers, those
;; windows will scroll.  This is because points must be visible in buffers, so
;; Emacs must scroll to keep the point visible when e.g. tall minibuffer windows
;; are created.  As such, `stillness-mode' moves the points in existing buffers
;; lower such that Emacs does not need to scroll to keep them visible.
;;
;; 2025-04-07: I know of two other very similar packages: sinister.el
;; (https://github.com/positron-solutions/sinister) and mini-ontop.el
;; (https://github.com/hkjels/mini-ontop.el).  I have not had the time to
;; thoroughly test out the differences between each package (all of them seem to
;; have been written in a similar time period, and none of them compare and
;; contrast the merits with the other packages...), but for now I go with
;; stillness-mode.
(use-package stillness-mode
  :config
  (stillness-mode 1))

;;; Ace-window
(use-package ace-window
  :bind (("C-; w" . ace-window)
         ("C-; W" . krisb-ace-window-prefix))
  :custom
  (aw-scope 'global)
  (aw-swap-invert t)
  (aw-background t)
  (aw-display-mode-overlay nil)
  (aw-dispatch-always t) ; Dispatch available even when less than three windows are open
  (aw-minibuffer-flag t)
  (aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?p))
  (aw-fair-aspect-ratio 3)
  :custom-face
  (aw-leading-char-face ((t (:height 3.0 :weight bold))))
  :config
  ;; Is not a defcustom, so use setq
  (setq aw-dispatch-alist
        '((?k aw-delete-window "Delete window")
          (?K delete-other-windows "Delete other windows")
          (?s aw-swap-window "Swap windows")
          (?m krisb-ace-window-take-over-window "Go to window and delete current window")
          (?c aw-copy-window "Copy window")
          (?o aw-flip-window "Other window")
          (?v krisb-ace-window-set-other-window "Set to other-scroll-window's window")
          (?b aw-switch-buffer-in-window "Switch to buffer in window")
          (?B aw-switch-buffer-other-window "Change buffer in window")
          (?2 aw-split-window-vert "Split vertically")
          (?3 aw-split-window-horz "Split horizontally")
          (?+ aw-split-window-fair "Split heuristically") ; See `aw-fair-aspect-ratio'
          (?? aw-show-dispatch-help)))

  ;; Taken from Karthink's config
  (defun krisb-ace-window-take-over-window (window)
    "Move from current window to WINDOW.

  Delete current window in the process."
    (let ((buf (current-buffer)))
      (if (one-window-p)
          (delete-frame)
        (delete-window))
      (aw-switch-to-window window)
      (switch-to-buffer buf)))

  ;; Taken from Karthink's config
  (defun krisb-ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
  The next buffer is the buffer displayed by the next command invoked
  immediately after this command (ignoring reading from the minibuffer).
  Creates a new window before displaying the buffer. When
  `switch-to-buffer-obey-display-actions' is non-nil, `switch-to-buffer'
  commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))

  ;; Based off of similar code taken from
  ;; https://karthinks.com/software/emacs-window-management-almanac/#scroll-other-window--built-in
  (defun krisb-ace-window-set-other-window (window)
    "Set WINDOW as the \"other window\" for the current one.
  \"Other window\" is the window scrolled by `scroll-other-window' and
  `scroll-other-window-down'."
    (setq-local other-window-scroll-buffer (window-buffer window))))

;;; Pinching-margins
(use-package pinching-margins
  :ensure nil
  :custom
  (pinching-margins-visible-width 140)
  (pinching-margins-ignore-predicates
   '(window-minibuffer-p
     (lambda (win)
       (with-selected-window win
         (member major-mode '(exwm-mode doc-view-mode))))
     (lambda (win)
       (cl-some (lambda (regexp) (string-match-p regexp (buffer-name (window-buffer win))))
                (list (rx bol (0+ whitespace) (literal "*")))))
     (lambda (win)
       (with-selected-window win (bound-and-true-p olivetti-mode)))))
  (pinching-margins-force-predicates
   '((lambda (win)
       (with-selected-window win
         (member major-mode '())))
     (lambda (win)
       (cl-some (lambda (regexp) (string-match-p regexp (buffer-name (window-buffer win))))
                '("^\\*vc-")))))
  :config
  (pinching-margins-mode 1))

;;; Window-x
;; Awesome new window moving commands in Emacs 31.1 that took the former role of
;; transpose-frame.el.  See for more information about the commands themselves
;; as well as a little background on the development challenges that overcome to
;; make these commands possible:
;; https://p.bauherren.ovh/blog/tech/new_window_cmds.
(use-package window-x
  :ensure nil
  :bind ( :map window-prefix-map
          ("t" . window-layout-tranpose) ; Diagonal reflection
          ;; Rotate window layout
          ("r" . window-layout-rotate-clockwise)
          ("R" . window-layout-rotate-anticlockwise)
          ;; Flipping
          ("f" . window-layout-flip-leftright)
          ("F" . window-layout-flip-topdown)
          ;; Rotate buffers (keep window placements)
          ("c" . window-layout-rotate-clockwise)
          ("C" . window-layout-rotate-anticlockwise)
          ;; Repeat-mode map
          :repeat-map krisb-window-x-repeat-map
          ("t" . window-layout-tranpose)
          ("r" . window-layout-rotate-clockwise)
          ("R" . window-layout-rotate-anticlockwise)
          ("f" . window-layout-flip-leftright)
          ("F" . window-layout-flip-topdown)
          ("c" . window-layout-rotate-clockwise)
          ("C" . window-layout-rotate-anticlockwise)))

;;;; Display-buffer-alist
(with-eval-after-load 'window
  (setq display-buffer-alist
        `(
;;;;; Org-mime
          ("OrgMimeMailBody"
           (display-buffer-same-window))

;;;;; Diff-mode
          ((major-mode . diff-mode)
           (display-buffer-same-window))

;;;;; Eldoc
          ("^\\*eldoc"
           (display-buffer-at-bottom)
           (post-command-select-window . t)
           (window-height . shrink-window-if-larger-than-buffer)
           (window-parameters . ((mode-line-format . none))))

;;;;; Embark
          ("\\*Embark Actions\\*"
           (display-buffer-in-direction)
           (window-height . fit-window-to-buffer)
           (direction . above)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))

;;;;; Denote-interface
          ((major-mode . denote-interface-mode)
           (display-buffer-same-window))

;;;;; Org-roam
          ("\\*org-roam\\*"
           (display-buffer-below-selected)
           (window-height . fit-window-to-buffer))

;;;;; Customize buffers
          ("\\*Customize Group:"
           (display-buffer-reuse-mode-window display-buffer-pop-up-window display-buffer-below-selected))
          )))

;;; Provide
(provide 'krisb-windows)
