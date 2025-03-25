;;; Environment
;;;; Visual-wrap
;; Visually indent lines wrapped visually! This makes long-lines in lists
;; properly indented!
;; NOTE: This package is the same as the more often referred to
;; `adaptive-wrap-prefix-mode'.
(use-package visual-wrap
  ;; 2024-10-30: Using adaptive-wrap for now since there seems to be some kind
  ;; of error between the interactions of visual-wrap and org-modern.  See
  ;; https://github.com/minad/org-modern/discussions/238.
  :disabled t
  :ensure nil
  :config
  (global-visual-wrap-prefix-mode 1))

;;;; Adaptive-wrap
(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;;;; Olivetti
(use-package olivetti
  :hook (((org-mode Info-mode emacs-news-view-mode org-msg-edit-mode) . olivetti-mode)
         (olivetti-mode . krisb-olivetti-set-bookmark-face))
  :custom
  (olivetti-lighter nil)
  (olivetti-body-width 0.55)
  (olivetti-minimum-body-width 80)
  (olivetti-margin-width 8)
  (olivetti-style 'fancy)              ; Fancy makes the buffer look like a page
  ;; FIXME 2024-01-11: This is a temporary solution. Olivetti's changing of
  ;; margins and fringes messes with the calculation of
  ;; `mode--line-format-right-align', which determines where the right side of
  ;; the mode line is placed.
  (mode-line-format-right-align
   '(:eval (if (and (bound-and-true-p olivetti-mode)
                    olivetti-style)     ; 'fringes or 'fancy
               (let ((mode-line-right-align-edge 'right-fringe))
                 (mode--line-format-right-align))
             (mode--line-format-right-align))))
  :config
  (krisb-modus-themes-setup-faces
   "olivetti"
   (set-face-attribute 'olivetti-fringe nil
                       :background bg-dim
                       :inherit 'unspecified))

  ;; Set `bookmark-face' buffer-locally
  (defun krisb-olivetti-set-bookmark-face ()
    "Sets the buffer-local specification of `bookmark-face'.
We do this because the olivetti settings may change the background color
of the fringe, meaning bookmark fringe marks, which use the default
fringe background color, are out of place."
    (face-remap-add-relative 'bookmark-face
                             :inherit '(olivetti-fringe success))))

;;;; Astute.el
(use-package astute
  :hook (org-mode . astute-mode)
  :custom
  (astute-lighter "")
  (astute-prefix-single-quote-exceptions
   '("bout"
     "em"
     "n'"
     "cause"
     "round"
     "twas"
     "tis")))

;;;; Darkroom
(use-package darkroom
  :bind ( :map krisb-toggle-keymap
          ("d" . darkroom-mode)
          ("D" . darkroom-tentative-mode))
  :custom
  (darkroom-text-scale-increase 1.3))

;;;; Typewriter-roll-mode
(use-package typewriter-roll-mode
  :bind ( :map krisb-toggle-keymap
          ("r" . typewriter-roll-mode)))

;;;; Timers
;;;;; Tmr
(use-package tmr
  :bind ( :map krisb-open-keymap
          ("t" . krisb-tmr-dispatch))
  :custom
  ;; Useful variables
  (tmr-descriptions-list
   '("Stop working!" "Work time 😄"))
  (tmr-notification-urgency 'normal)
  (tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
  :config
  (require 'transient)
  (transient-define-prefix krisb-tmr-dispatch ()
    "Invoke a transient menu for `tmr'."
    ["Create or remove timers"
     [("t" "Create a timer" tmr)
      ("T" "Create a timer with description" tmr-with-details)
      ("C" "Clone a timer" tmr-clone)]
     [("r" "Remove finished" tmr-remove-finished)
      ("c" "Cancel timer" tmr-cancel)]]
    ["View timers"
     [("v" "Tabulated view" tmr-tabulated-view)]]))


;;;;; Hammy
(use-package hammy
  :bind ( :map krisb-open-keymap
          ("h h" . krisb-hammy-dwim)
          ("h S" . hammy-start)
          ("h n" . hammy-next)
          ("h s" . hammy-stop)
          ("h r" . hammy-reset)
          ("h t" . hammy-toggle)
          ("h a" . hammy-adjust)
          ("h v" . hammy-view-log)
          ("h R" . hammy-status)
          ("h I" . hammy-start-org-clock-in))
  :custom
  ;; TODO 2024-09-25: Have this found more locally. When I do, also change
  ;; `tmr-sound' to this file
  (hammy-sound-end-work "/home/krisbalintona/.emacs.d/elpa/work-timer/simple-notification.mp3")
  (hammy-sound-end-break "/home/krisbalintona/.emacs.d/elpa/work-timer/simple-notification.mp3")

  ;; Mode line
  (hammy-mode-always-show-lighter nil)
  (hammy-mode-update-mode-line-continuously t)
  (hammy-mode-lighter-seconds-format "%.2m:%.2s")
  (hammy-mode-lighter-prefix "[H]")
  (hammy-mode-lighter-overdue "!")
  (hammy-mode-lighter-pie t)
  (hammy-mode-lighter-pie-height 0.65)
  :config
  ;; Mode line
  (hammy-mode 1)

  ;; Override `hammy-start-org-clock-in' to work in org-agenda
  (el-patch-defun hammy-start-org-clock-in (&rest _ignore)
    "Call `org-clock-in' and start a hammy (or use an already-started one).
If point is in an Org entry, clock into it; otherwise, offer a
list of recently clocked tasks to clock into.  The Org task will
then automatically be clocked out during the hammy's second
interval (and when the hammy is stopped), and back in when the
first interval resumes.  (If the user clocks into a different
task while the hammy is running, the task that is clocked-in when
the work interval ends will be clocked back into when the next
work interval begins.)

Returns the hammy from `hammy-start'.  Assumes that the hammy's
first interval is the work interval (i.e. the one during which
the task should be clocked in)."
    (interactive)
    (require 'org)
    ;; MAYBE: Take a point-or-marker argument for the task to clock into.
    (el-patch-swap
      (if (and (eq major-mode 'org-mode)
               (not (org-before-first-heading-p)))
          ;; At an Org entry: clock in to heading at point.
          (org-clock-in)
        ;; Not in an Org entry: offer a list to choose from.
        (org-clock-in '(4)))
      (cond
       ;; At an Org entry: clock in to heading at point.
       ((and (eq major-mode 'org-mode)
             (not (org-before-first-heading-p)))
        (org-clock-in))
       ;; At an Org-agenda entry: clock in to entry at point.
       ((eq major-mode 'org-agenda-mode)
        (org-agenda-clock-in))
       ;; Not in an Org entry: offer a list to choose from.
       (t (org-clock-in '(4)))))
    (let ((hammy (hammy-complete "Clock in with Hammy: " hammy-hammys)))
      (unless (hammy-interval hammy)
        (hammy-start hammy))
      (cl-macrolet ((pushfn (fn place)
                      `(cl-pushnew ,fn ,place :test #'equal)))
        (pushfn #'hammy--org-clock-in (hammy-interval-before (hammy-interval hammy)))
        (pushfn #'hammy--org-clock-out (hammy-interval-after (hammy-interval hammy)))
        (pushfn #'hammy--org-clock-out (hammy-stopped hammy)))
      hammy))

  ;; Custom lighter
  (defun krisb-hammy-mode-lighter ()
    "Return lighter for `hammy-mode'."
    (cl-labels
        ((format-hammy (hammy)
           (let ((remaining
                  (abs
                   ;; We use the absolute value because `ts-human-format-duration'
                   ;; returns 0 for negative numbers.
                   (- (hammy-current-duration hammy)
                      (float-time (time-subtract (current-time)
                                                 (hammy-current-interval-start-time hammy)))))))
             (format "%s(%s%s:%s)"
                     (propertize (hammy-name hammy)
                                 'face 'hammy-mode-lighter-name)
                     (if (hammy-overduep hammy)
                         (propertize hammy-mode-lighter-overdue
                                     'face 'hammy-mode-lighter-overdue)
                       "")
                     (propertize (hammy-interval-name (hammy-interval hammy))
                                 'face `(hammy-mode-lighter-interval
                                         ,(hammy-interval-face (hammy-interval hammy))))
                     (concat (when hammy-mode-lighter-pie
                               (propertize " " 'display (hammy--pie hammy)))
                             (if (hammy-overduep hammy)
                                 ;; We use the negative sign when counting down to
                                 ;; the end of an interval (i.e. "T-minus...") .
                                 "+" "-")
                             (format-seconds (if (< remaining 60)
                                                 "%2ss" hammy-mode-lighter-seconds-format)
                                             remaining))))))
      (if hammy-active
          (concat (mapconcat #'format-hammy hammy-active ",") " ")
        ;; No active hammys.
        (when hammy-mode-always-show-lighter
          (concat (propertize hammy-mode-lighter-prefix
                              'face 'hammy-mode-lighter-prefix-inactive)
                  (if hammy-mode-lighter-suffix-inactive
                      (concat ":" hammy-mode-lighter-suffix-inactive))
                  " ")))))
  (advice-add 'hammy-mode-lighter :override #'krisb-hammy-mode-lighter)

  ;; Dwim command
  (defun krisb-hammy-dwim ()
    "DWIM with hammy."
    (interactive)
    (if hammy-active
        (call-interactively 'hammy-next)
      (call-interactively 'hammy-start)))

  ;; Hammy definitions
  (defun krisb-hammy-play-sound ()
    "Play end of timer sound."
    (interactive)
    (call-process-shell-command
     (format "ffplay -nodisp -autoexit %s >/dev/null 2>&1" hammy-sound-end-work) nil 0))

  (setq hammy-hammys nil)
  (hammy-define "Fractional"
    :documentation "Breaks that are ⅓ as long as the last work interval."
    :intervals
    (list
     (interval :name "Work"
               :duration "40 minutes"
               :before (do (announce "Starting work time (advance to break when ready)."))
               :after (do (krisb-hammy-play-sound))
               :advance (do (krisb-hammy-play-sound)
                            (let* ((current-duration
                                    (ts-human-format-duration
                                     (float-time
                                      (time-subtract (current-time)
                                                     current-interval-start-time))))
                                   (message (format "You've worked for %s!" current-duration)))
                              (announce message)
                              (notify message))))
     (interval :name "Break"
               :duration (do (cl-assert (equal "Work" (hammy-interval-name (caar history))))
                             (let ((duration (cl-loop for (interval start end) in history
                                                      while (equal "Work" (hammy-interval-name interval))
                                                      sum (float-time (time-subtract end start))
                                                      into work-seconds
                                                      finally return (* work-seconds 0.33))))
                               (when (alist-get 'unused-break etc)
                                 (cl-incf duration (alist-get 'unused-break etc))
                                 (setf (alist-get 'unused-break etc) nil))
                               duration))
               :before (do (let ((message (format "Starting break for %s."
                                                  (ts-human-format-duration current-duration))))
                             (announce message)))
               :after (do (krisb-hammy-play-sound)
                          (let* ((elapsed
                                  (float-time
                                   (time-subtract (current-time) current-interval-start-time)))
                                 (unused (- current-duration elapsed)))
                            (when (> unused 0)
                              (if (alist-get 'unused-break etc)
                                  (cl-incf (alist-get 'unused-break etc) unused)
                                (setf (alist-get 'unused-break etc) unused)))))
               :advance (remind "5 minutes"
                                (do (krisb-hammy-play-sound))))))
  (hammy-define "Ramp and decline"
    :documentation "Get your momentum going!"
    :intervals (list (interval :name "Work"
                               :face 'font-lock-builtin-face
                               :duration (climb "5 minutes" "40 minutes"
                                                :descend t :step "5 minutes")
                               :before (do (announce "Work time!"))
                               :advance (do (announce "Work time is over!")
                                            (notify "Work time is over!")
                                            (remind "5 minutes"
                                                    (do (krisb-hammy-play-sound)))))
                     (interval :name "Rest"
                               :face 'font-lock-type-face
                               :duration (do (let ((duration (cl-loop for (interval start end) in history
                                                                      while (equal "Work" (hammy-interval-name interval))
                                                                      sum (float-time (time-subtract end start))
                                                                      into work-seconds
                                                                      finally return (max (* 60 2) (* work-seconds 0.33)))))
                                               (when (alist-get 'unused-break etc)
                                                 (cl-incf duration (alist-get 'unused-break etc))
                                                 (setf (alist-get 'unused-break etc) nil))
                                               duration))
                               :before (do (announce "Rest time!"))
                               :after (do (let* ((elapsed
                                                  (float-time
                                                   (time-subtract (current-time) current-interval-start-time)))
                                                 (unused (- current-duration elapsed)))
                                            (when (> unused 0)
                                              (if (alist-get 'unused-break etc)
                                                  (cl-incf (alist-get 'unused-break etc) unused)
                                                (setf (alist-get 'unused-break etc) unused)))))
                               :advance (remind "5 minutes"
                                                (do (announce "Rest time is over!")
                                                    (notify "Rest time is over!")
                                                    (krisb-hammy-play-sound)))))
    :complete-p (do (and (> cycles 1)
                         interval
                         (equal "Work" interval-name)
                         (>= (duration "5 minutes") current-duration)))
    :after (do (announce "Flywheel session complete!")
               (notify "Flywheel session complete!")))

  (hammy-define (propertize "🍅" 'face '(:foreground "tomato"))
    :documentation "The classic pomodoro timer."
    :intervals
    (list
     (interval :name "Working"
               :duration "25 minutes"
               :before (do (announce "Starting work time.")
                           (notify "Starting work time."))
               :advance (remind "10 minutes"
                                (do (announce "Break time!")
                                    (notify "Break time!"))))
     (interval :name "Resting"
               :duration (do (if (and (not (zerop cycles))
                                      (zerop (mod cycles 3)))
                                 ;; If a multiple of three cycles have
                                 ;; elapsed, the fourth work period was
                                 ;; just completed, so take a longer break.
                                 "30 minutes"
                               "5 minutes"))
               :before (do (announce "Starting break time.")
                           (notify "Starting break time."))
               :advance (remind "10 minutes"
                                (do (announce "Break time is over!")
                                    (notify "Break time is over!"))))))

  (hammy-define "1-shot"
    :documentation "Single-use timer that prompts for name and duration."
    :complete-p (do (> cycles 0))
    :before
    (lambda (hammy)
      (hammy-reset hammy)
      (setf (hammy-intervals hammy)
            (ring-convert-sequence-to-ring
             (list (interval
                    :name (read-string "Interval name (optional): " nil nil "")
                    :duration (read-string "Duration: ")
                    :advance (remind "5 minutes"
                                     (do (let ((message (format "%s is over!" interval-name)))
                                           (krisb-hammy-play-sound)
                                           (notify message)))))))))))

;;; Spell checking
;;;; Jinx
;; JIT spell checker that uses `enchant'. The executable is enchant-2. See the
;; manual for more information:
;; https://abiword.github.io/enchant/src/enchant.html
(use-package jinx
  :ensure-system-package ((enchant-2 . enchant)
                          (pkgconf)
                          ;; Don't forget to install spell checker libraries!
                          (hunspell)
                          ("/usr/share/hunspell/en_US-large.dic" . hunspell-en_us)
                          (hspell)      ; Hebrew
                          (nuspell) ; Newest spell checker to be used by Firefox, Thunderbird, etc.
                          (voikkospell . libvoikko)) ; Finnish
  :demand t
  :diminish
  :bind ( :map jinx-mode-map
          ([remap ispell-word] . jinx-correct)
          ("C-," . jinx-correct)
          ("C-M-$" . jinx-languages))
  :config
  (global-jinx-mode 1)

  ;; Mimic `flyspell-abbrev-p'.  Taken from
  ;; https://github.com/minad/jinx/wiki#save-misspelling-and-correction-as-abbreviation
  (defun krisb-jinx--add-to-abbrev (overlay word)
    "Add abbreviation to `local-abbrev-table'.

The misspelled word is taken from OVERLAY. WORD is the corrected
word."
    (let ((abbrev (buffer-substring-no-properties
                   (overlay-start overlay)
                   (overlay-end overlay))))
      (message "Abbrev: %s -> %s" abbrev word)
      ;; Change this to `global-abbrev-table' if preferred
      (define-abbrev local-abbrev-table abbrev word)))
  (advice-add 'jinx--correct-replace :before #'krisb-jinx--add-to-abbrev)

  ;; Read Ispell's "LocalWords."  Taken from
  ;; https://github.com/minad/jinx/wiki#make-jinx-read-from-localwords
  (defun krisb-jinx-ispell--get-localwords ()
    "Return a string of ispell's local words.

Those are the words following `ispell-words-keyword' (usually
\"LocalWords\") in the current buffer."
    (require 'ispell)
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (search-forward ispell-words-keyword nil t)
               collect (string-trim (buffer-substring-no-properties (point) (line-end-position))) into result
               finally return (mapconcat #'identity result " "))))
  (defun krisb-jinx-ispell-add-localwords ()
    "Add ispell's local words to `jinx-local-words'."
    (let ((ispell-localwords (krisb-jinx-ispell--get-localwords)))
      (setq jinx-local-words (concat jinx-local-words ispell-localwords))
      (setq jinx--session-words (append jinx--session-words (split-string ispell-localwords)))))
  (add-hook 'jinx-mode-hook #'krisb-jinx-ispell-add-localwords)

  ;; Write to buffer's LocalWords instead of populating `jinx-local-words', a
  ;; local variable. Taken from
  ;; https://github.com/minad/jinx/wiki#make-jinx-write-localwords
  (defun krisb-jinx-save-as-ispell-localword (save key word)
    "Save WORD using ispell's `ispell-words-keyword'.
If SAVE is non-nil save, otherwise format candidate given action KEY."
    (if save
        (progn
          (require 'ispell)
          (ispell-add-per-file-word-list word)
          (add-to-list 'jinx--session-words word)
          (setq jinx-local-words
                (string-join
                 (sort (delete-dups
                        (cons word (split-string jinx-local-words)))
                       #'string<)
                 " "))))
    (list key word "File (LocalWords)"))
  ;; NOTE 2023-07-16: Can also directly add to `jinx--save-keys' directly
  (setf (alist-get ?* jinx--save-keys) #'krisb-jinx-save-as-ispell-localword))

;;; Grammar
(with-eval-after-load 'eglot
  (unless (executable-find "harper-ls")
    (system-packages-ensure "harper"))
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               ;; NOTE 2025-03-19: We give a language ID of "markdown" to
               ;; harper (see supported languages and their corresponding
               ;; language IDs here:
               ;; https://writewithharper.com/docs/integrations/language-server#Supported-Languages)
               ;; because org-mode is currently not supported, so markdown is
               ;; the closest we have (note: it is better than the "plaintext"
               ;; language ID; see the recommendation here:
               ;; https://github.com/Automattic/harper/issues/149#issuecomment-2619515397).
               ;; To check the status of adding org-mode to the list of
               ;; supported languages, see
               ;; https://github.com/Automattic/harper/issues/79#issuecomment-2638110954
               '((org-mode :language-id "markdown") . ("harper-ls" "--stdio"))))

;;; Other
;;;; Cm-mode (CriticMarkup minor mode)
;; Track suggested changes in plain text files.
(use-package cm-mode
  :vc ( :url "https://github.com/joostkremers/criticmarkup-emacs.git"
        :rev :newest)
  :ensure-system-package (pandiff . nodejs-pandiff))

;;; Provide
(provide 'krisb-prose)
