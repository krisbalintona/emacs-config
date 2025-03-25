;;;; Work-timer
  (use-package work-timer
    :disabled
    :vc (:url "git@github.com:krisbalintona/work-timer.git"
              :rev :newest)
    :bind-keymap ("C-c o w" . work-timer-prefix-map)
    :hook (kb/themes . kb/work-timer-set-faces)
    :custom
    (work-timer-debug nil)
    (work-timer-time-format "%.2m:%.2s")
    (work-timer-work-duration-function 'work-timer-work-duration-fractional)
    (work-timer-fractional-work-duration 25)
    (work-timer-break-duration-function 'work-timer-break-duration-fractional)
    (work-timer-fractional-break-duration-fraction 0.25)
    :config
    (with-eval-after-load 'org-agenda
      (work-timer-with-org-clock-mode 1))
    ;; Save relevant current timer variables to resume timer across Emacs sessions
    (when (bound-and-true-p savehist-mode)
      (dolist (var '(work-timer-start-time
                     work-timer-duration
                     work-timer-type
                     work-timer-pauses))
        (add-to-list 'savehist-additional-variables var)))
    (defun kb/work-timer-set-faces ()
      "Set `work-timer-mode-line' according to dark or light theme."
      (let* ((dark-p (color-dark-p (color-name-to-rgb (face-attribute 'default :background))))
             (dark-foreground "DarkOrange")
             (light-foreground
              ;; Darken color by 15% if using a light theme
              (color-darken-name dark-foreground 15))
             (foreground (if dark-p dark-foreground light-foreground)))
        (set-face-foreground 'work-timer-mode-line foreground)))
    (kb/work-timer-set-faces))
  ;;;; Super-hint
  (use-package super-hint
    :disabled t
    :vc (:url "https://github.com/eval-exec/super-hint.el.git"
              :rev :newest)
    :init
    (with-eval-after-load 'xref
      (require 'super-hint-xref)
      (super-hint-xref-mode 1)
      (diminish 'super-hint-xref-mode))
    (with-eval-after-load 'rg
      (require 'super-hint-rg)
      (super-hint-rg-mode 1)
      (diminish 'super-hint-rg-mode)))
  (use-package super-hint-xref
    :diminish
    :after xref
    :ensure nil
    :config
    (super-hint-xref-mode 1))
  (use-package super-hint-rg
    :diminish
    :after rg
    :ensure nil
    :config
    (super-hint-rg-mode 1))
  ;;;; Pixel-scroll
  (use-package pixel-scroll
    :ensure nil
    :hook ((on-first-input . pixel-scroll-mode)
           (on-first-input . pixel-scroll-precision-mode))
    :custom
    (pixel-scroll-precision-interpolate-page t)
    (pixel-scroll-precision-interpolation-factor 3.0)
    :config
    (defun kb/pixel-recenter (&optional arg redisplay)
      "Similar to `recenter' but with pixel scrolling.
  ARG and REDISPLAY are identical to the original function."
      ;; See the links in line 6676 in window.c for
      (when-let* ((current-pixel (pixel-posn-y-at-point))
                  (target-pixel (if (numberp arg)
                                    (* (line-pixel-height) arg)
                                  (* 0.5 (window-body-height nil t))))
                  (distance-in-pixels 0)
                  (pixel-scroll-precision-interpolation-total-time
                   (/ pixel-scroll-precision-interpolation-total-time 2.0)))
        (setq target-pixel
              (if (<= 0 target-pixel)
                  target-pixel
                (- (window-body-height nil t) (abs target-pixel))))
        (setq distance-in-pixels (- target-pixel current-pixel))
        ;; FIXME 2024-01-23: `pixel-scroll-precision-interpolate' seems to have a
        ;; bug where occasionally trying to go to pixel 0 (top of buffer) will
        ;; move point forward by one line. I've confirmed that this is an issue
        ;; not with my function but with `pixel-scroll'. This might also apply to
        ;; non-first line cases. I'll see...
        (condition-case err ; OPTIMIZE 2024-02-11: I use this to catch errors rather than being spit out into buffer/wherever
            (pixel-scroll-precision-interpolate distance-in-pixels nil 1)
          (error (message "[kb/pixel-recenter] %s" (error-message-string err))))
        (when redisplay (redisplay t))))
    ;; FIXME 2024-01-22: Seems to be off a few lines in e.g. Info-mode?
    (defun kb/pixel-scroll-up (&optional arg)
      "(Nearly) drop-in replacement for `scroll-up'."
      (cond
       ((eq this-command 'scroll-up-line)
        (funcall (ad-get-orig-definition 'scroll-up) (or arg 1)))
       (t
        (unless (eobp) ; Jittery window if trying to go down when already at bottom
          (pixel-scroll-precision-interpolate
           (- (* (line-pixel-height)
                 (or arg (- (window-text-height) next-screen-context-lines))))
           nil 1)))))
    (defun kb/pixel-scroll-down (&optional arg)
      "(Nearly) drop-in replacement for `scroll-down'."
      (cond
       ((eq this-command 'scroll-down-line)
        (funcall (ad-get-orig-definition 'scroll-down) (or arg 1)))
       (t
        (pixel-scroll-precision-interpolate
         (* (line-pixel-height)
            (or arg (- (window-text-height) next-screen-context-lines)))
         nil 1))))
    (defun kb/pixel-scroll-everything ()
      "Use pixel-scroll functions for scrolling and recentering.
  Dependent on the activation of `pixel-scroll-precision-mode'. Add to
  `pixel-scroll-precision-mode-hook'."
      (cond
       (pixel-scroll-precision-mode
        (advice-add 'scroll-up :override #'kb/pixel-scroll-up)
        (advice-add 'scroll-down :override #'kb/pixel-scroll-down)
        (advice-add 'recenter :override #'kb/pixel-recenter))
       (t
        (advice-remove 'scroll-up #'kb/pixel-scroll-up)
        (advice-remove 'scroll-down #'kb/pixel-scroll-down)
        (advice-remove 'recenter #'kb/pixel-recenter))))
    ;; (add-hook 'pixel-scroll-precision-mode-hook #'kb/pixel-scroll-everything)
    )
  ;;;; Gdb-mi
  ;; Built-in GDB
  (use-package gdb-mi
    :ensure nil
    :custom
    (gdb-many-windows t)
    (gdb-show-main t)
    (gud-gdb-command-name "gdb -i=mi --quiet")
    (gdb-restore-window-configuration-after-quit 'if-gdb-many-windows)
    :config
    (defun kb/gdb-non-stop-handler ()
      "Version of the original that avoids the GDB startup error
  regarding \"target-async\"."
      (goto-char (point-min))
      (if (re-search-forward "No symbol" nil t)
          (progn
            (message
             "This version of GDB doesn't support non-stop mode.  Turning it off.")
            (setq gdb-non-stop nil)
            (setq gdb-supports-non-stop nil))
        (setq gdb-supports-non-stop t)
        ;; (gdb-input "-gdb-set target-async 1" 'ignore)
        (gdb-input "-gdb-set mi-async 1" 'ignore) ; Change to this, as advised
        (gdb-input "-list-target-features" 'gdb-check-target-async)))
    (advice-add 'gdb-non-stop-handler :override #'kb/gdb-non-stop-handler))
  ;;;; Comments
  ;;;;; Alt-comment-dwim
  (use-package alt-comment-dwim
    :disabled
    ;; :ensure (:type git
    ;;                :host gitlab
    ;;                :protocol ssh
    ;;                :repo "PreciousPudding/alt-comment-dwim"
    ;;                :depth nil)
    :vc (:url "git@gitlab.com:PreciousPudding/alt-comment-dwim.git"
              :rev :newest)
    :bind
    (([remap comment-dwim] . alt-comment-dwim)
     ([remap comment-line] . alt-comment-dwim-line)
     ("C-M-;" . alt-comment-dwim-todo-and-timestamp))
    :custom
    (alt-comment-dwim-keyword-faces
     '(("TODO" . "orange")
       ("HACK" . (error bold))
       ("NOTE" . "cornflower blue")
       ("REVIEW" . "orchid")
       ("FIXME" . (error bold))
       ("OPTIMIZE" . "SandyBrown"))))
  ;;;; Writing
  ;;;;; Sentex
  ;; Alternative to `sentence-navigation'. Provides sentence navigation commands
  ;; that respect abbreviations, etc.
  (use-package pcre2el)
  (use-package sentex
    ;; :ensure (sentex :type git :host codeberg :repo "martianh/sentex"
    ;;                 ;; Need more than just elisp files
    ;;                 :files ("*"))
    :commands kb/forward-sentence-function
    :custom
    ;; NOTE 2023-01-18: icu4j has many more rules, but is "too thorough" for my
    ;; tastes, so I stick with the more modest omegat and add more rules
    ;; (sentex-ruleset-framework 'icu4j)
    (sentex-ruleset-framework 'omegat)
    ;; (sentex-ruleset-framework 'okapi-alt)
    (sentex-custom-rules-regex-list
     `(("English"
        (("[JS]r\\." "[[:space:]][[:lower:]]" :break nil)
         ("U\\.S\\." "[[:space:]][[:lower:]]" :break nil)
         ("[[:lower:]]+\\.[])}]" "[[:space:]][[:lower:]]" :break nil)
         ("[^\\.][[:space:]][\"“]?[A-Z]\\." "[[:space:]]" :break nil)
         ("[Oo]p\\." "[[:space:]][[:digit:]]" :break nil)
         ;; My regexps
         (,(rx "tk" (1+ (any punct)) ")") ,(rx (any space) (any alnum)) :break nil)
         (,(rx (1+ (any punct))) ,(rx " tk" (or ")" ";")) :break nil)
         (,(rx (or "p." "pp.")) ,(rx (any space) (any digit)) :break nil)
         (,(rx (or "..." "…")) ,(rx (any space) (any lower)) :break nil)
         (,(rx "etc.") ,(rx (any space) (any lower)) :break nil)
         (,(rx "Rev.") ,(rx (any space) (any upper)) :break nil)
         (,(rx "Jan.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Feb.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Mar.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Apr.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "May.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Jun.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Jul.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Aug.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Sep.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Oct.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Nov.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Dec.") ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
         (,(rx "Jr.") ,(rx (any space) (any upper)) :break nil)
         (,(rx "J.") ,(rx (any space) (any upper)) :break nil)
         (,(rx "Gen.") ,(rx (any space) (any upper)) :break nil)
         (,(rx "Dist.") ,(rx (any space) (any upper)) :break nil)
         (,(rx (seq (any punct) (any punct))) ,(rx (any space) (any lower)) :break nil)
         ;; Breaks
         (,(rx (seq (or (any alnum) (any punct)) ":")) ,(rx (seq (any space) (any upper))) :break t)
         ((lambda () (rx (regexp (when comment-start (string-trim comment-start (or comment-padding " "))))))
          (lambda () (when comment-start (rx (seq (* (regexp (or comment-padding " "))) (any alnum))))) :break t)
         (,(rx bol (or (literal "+") (literal "-") (literal "*"))) ,(rx (seq (any space) (any alnum))) :break t)
         (,(rx (seq bol (or (literal "+") (literal "-") (literal "*")) (any space))) ,(rx anychar) :break t)
         (,(rx (seq bol (any digit) (or (literal ".") (literal ")")) (any space))) ,(rx anychar) :break t)))))
    :init
    (setq forward-sentence-function 'kb/forward-sentence-function)
    :config
    ;; Without manual reevaluation of `sentex--get-ruleset-by-lang', things break
    ;; for some reason on startup...
    (defun sentex--get-ruleset-by-lang (language converted-file-set)
      "Get ruleset for LANGUAGE from CONVERTED-FILE-SET."
      (dolist (x converted-file-set)
        (when (equal language
                     (car x))
          (cl-return x))))
    ;; Do this to make sure ruleset is up to date upon first invocation of a
    ;; sentence movement command
    (sentex--build-rule-list)
    ;; My own code below
    (defun kb/sentex-reg-pair-car (reg-pair)
      "Return the before-break-regexp for REG-PAIR.
  REG-PAIR can either be a regexp or a function that returns a
  regexp. If the function returns nil, then nothing is matched
  instead."
      (if (functionp (car reg-pair))
          ;; If function returns nil, match nothing instead
          (or (funcall (car reg-pair)) (rx unmatchable))
        (car reg-pair)))
    (defun kb/sentex-reg-pair-cadr (reg-pair)
      "Return the after-break-regexp for REG-PAIR.
  REG-PAIR can either be a regexp or a function that returns a
  regexp. If the function returns nil, then nothing is matched
  instead."
      (if (functionp (cadr reg-pair))
          ;; If function returns nil, match nothing instead
          (or (funcall (cadr reg-pair)) (rx unmatchable))
        (cadr reg-pair)))
    (defun kb/sentex--test-rule-pairs (regex-alist &optional moving-backward)
      "Return non-nil when when point is surrounded by an element in REGEX-ALIST.
  MOVING-BACKWARD makes adjustments based on where `backward-sentence' places point."
      (cl-dolist (reg-pair regex-alist)
        (when (and
               (looking-back
                (if moving-backward
                    (concat (kb/sentex-reg-pair-car reg-pair) "[[:blank:]]*")
                  (kb/sentex-reg-pair-car reg-pair)))
               (if moving-backward
                   (save-excursion
                     (forward-whitespace -1)
                     (looking-at (kb/sentex-reg-pair-cadr reg-pair)))
                 (looking-at (kb/sentex-reg-pair-cadr reg-pair)))
               (not (plist-get reg-pair :break)))
          (cl-return reg-pair))))
    (advice-add 'sentex--test-rule-pairs :override #'kb/sentex--test-rule-pairs)
    (defun kb/forward-sentence-function (&optional arg)
      "Move forward to next end of sentence. With argument, repeat.
  When ARG is negative, move backward repeatedly to start of sentence.
  The variable `sentence-end' is a regular expression that matches ends of
  sentences.  Also, every paragraph boundary terminates sentences as well.
  Uses `sentex’s regexps to ignore common abbreviations (see
  `sentex-ruleset-framework’) as well as define custom

  sentence-breaks (see `sentex-custom-rules-regex-list').
  Also, when moving forward sentences, will jump the whitespace
  between the current and next sentence, i.e,leave the point on the
  first character of the next sentence."
      (require 'sentex)
      (or arg (setq arg 1))
      (let ((case-fold-search nil)
            (opoint (point))
            (sentence-end (sentence-end)))
        ;; Going backward
        (while (< arg 0)
          (let ((pos (point))
                par-beg par-text-beg
                default-break-point sentex-break-point)
            (save-excursion
              (start-of-paragraph-text)
              (setq par-text-beg (point))
              (beginning-of-line)
              (setq par-beg (point)))
            ;; The strategy here is to first find the sentence end based on the
            ;; `sentence-end' regexp then `sentex-current-break-rules'.
            ;; Afterwards, we compare them and `goto-char' the one closer to the
            ;; point.
            (setq default-break-point
                  (save-excursion
                    (let ((final-pos
                           (if (and (re-search-backward sentence-end par-beg t)
                                    (or (< (match-end 0) pos)
                                        (re-search-backward sentence-end par-beg t)))
                               (match-end 0)
                             par-text-beg)))
                      ;; The following while loop causes further movement if the
                      ;; initial movement landed us on a non-breaking sentence end
                      ;; regexp (e.g. and abbreviation) recognized by
                      ;; `sentex--looking-back-forward-map'
                      (while (save-excursion
                               (goto-char final-pos)
                               (sentex--looking-back-forward-map sentex-current-language :moving-backward))
                        (if (and (re-search-backward sentence-end par-beg t)
                                 (or (< (match-end 0) pos)
                                     (re-search-backward sentence-end par-beg t)))
                            (setq final-pos (match-end 0))
                          (setq final-pos par-text-beg)))
                      final-pos)))
            (setq sentex-break-point
                  (let ((closest-break-point most-negative-fixnum))
                    (cl-dolist (reg-pair sentex-current-break-rules)
                      (save-excursion
                        (when (and (re-search-backward (rx (seq (regexp (kb/sentex-reg-pair-car reg-pair))
                                                                (regexp (kb/sentex-reg-pair-cadr reg-pair))))
                                                       (or default-break-point par-beg) t)
                                   (or (< (match-end 0) pos)
                                       (re-search-backward (rx (seq (regexp (kb/sentex-reg-pair-car reg-pair))
                                                                    (regexp (kb/sentex-reg-pair-cadr reg-pair))))
                                                           (or default-break-point par-beg) t))
                                   (< closest-break-point (match-end 0)))
                          ;; Change the following lines depending on where we want
                          ;; the point to end for our custom line breaks
                          (re-search-forward (rx (regexp (kb/sentex-reg-pair-car reg-pair))) nil t)
                          (goto-char (match-end 0))
                          (skip-chars-forward " \t")
                          (setq closest-break-point (point)))))
                    closest-break-point))
            (goto-char (max default-break-point sentex-break-point)))
          (setq arg (1+ arg)))
        ;; Going forward
        (while (> arg 0)
          (let ((par-end (save-excursion (end-of-paragraph-text) (point)))
                default-break-point sentex-break-point)
            (setq default-break-point
                  (save-excursion
                    (if (and
                         (re-search-forward sentence-end par-end t)
                         ;; If the point hasn't moved, and the above is non-nil,
                         ;; then we are in between the final full sentence and the
                         ;; end of the paragraph (i.e. ahead of us is an
                         ;; incomplete sentence followed by the end of a
                         ;; paragraph). In this case, then just go to the end of
                         ;; the paragraph
                         (not (eq (point) opoint)))
                        (progn
                          ;; Continue moving forward sentences (defined by
                          ;; `sentence-end') until
                          ;; `sentex--looking-back-forward-map' considers the
                          ;; sentence break a valid one
                          (while (save-excursion
                                   ;; Consider point having been moved to the
                                   ;; first character of the sentence by moving
                                   ;; backward
                                   (unless (eq (point) par-end)
                                     ;; This is only done if the point isn't
                                     ;; already at the end of the paragraph.
                                     ;; Without this, there will be an infinite
                                     ;; loop when the final sentence break in the
                                     ;; paragraph should be skipped and there is
                                     ;; whitespace after the sentence end.
                                     (skip-chars-backward " \t\n"))
                                   (sentex--looking-back-forward-map sentex-current-language))
                            (unless (re-search-forward sentence-end par-end t)
                              ;; If no other sentence is found in the rest of the
                              ;; paragraph, then just leave the point at the end
                              ;; of the paragraph
                              (goto-char par-end)))
                          ;; The only case when I want to leave the point at the
                          ;; end of the current sentence is when a newline lies
                          ;; between two sentences in the same paragraph. With the
                          ;; following when clause, ensure point is left at the
                          ;; end of the current sentence rather than at the
                          ;; beginning of the next sentence.
                          (when (bolp) (skip-chars-backward " \t\n"))
                          (point))
                      par-end)))
            (setq sentex-break-point
                  (let ((closest-break-point most-positive-fixnum))
                    (cl-dolist (reg-pair sentex-current-break-rules)
                      (save-excursion
                        (when (and (re-search-forward (rx (seq (regexp (kb/sentex-reg-pair-car reg-pair))
                                                               (regexp (kb/sentex-reg-pair-cadr reg-pair))))
                                                      (or default-break-point par-end) t)
                                   (> closest-break-point (match-end 0)))
                          ;; Change the following lines depending on where we want
                          ;; the point to end. The following is for my own
                          ;; preference in behavior. Leave the point at the
                          ;; beginning of the next sentence.
                          (re-search-backward (rx (regexp (kb/sentex-reg-pair-car reg-pair))) nil t)
                          (goto-char (match-end 0))
                          (skip-chars-forward " \t")
                          (setq closest-break-point (point)))))
                    closest-break-point))
            (goto-char (min default-break-point sentex-break-point)))

          (setq arg (1- arg)))

        ;; Return point
        (constrain-to-field nil opoint t)))

    ;; Benchmarking the performance of my function
    (defun kb/forward-sentence-function-benchmark ()
      "Benchmarks the difference in time between the current and default
  value of `forward-sentence-function'."
      (interactive)
      (save-restriction
        (save-excursion
          (let ((orig-func forward-sentence-function)
                my-time-beg my-time-end my-time-diff
                default-time-beg default-time-end default-time-diff)
            ;; Benchmark my function
            (setq forward-sentence-function 'kb/forward-sentence-function)
            (setq my-time-beg (current-time))
            (count-sentences (point-min) (point-max))
            (setq my-time-end (current-time)
                  my-time-diff
                  (time-to-seconds (time-subtract my-time-end my-time-beg)))

            ;; Benchmark the default function
            (setq forward-sentence-function 'forward-sentence-default-function)
            (setq default-time-beg (current-time))
            (count-sentences
             (point-min) (point-max))
            (setq default-time-end (current-time)
                  default-time-diff
                  (time-to-seconds (time-subtract default-time-end default-time-beg)))

            ;; Report the difference
            (message "My function took %s seconds
   whereas the default function took %s seconds.
  This is a difference in multitude of %s."
                     my-time-diff default-time-diff
                     (number-to-string (/ my-time-diff default-time-diff)))

            ;; Restore original `forward-sentence-function'
            (setq forward-sentence-function orig-func))))))

  ;;;;; Kb/para-split-sentences and kb/para-merge-sentences
  ;; Modified from https://stackoverflow.com/a/43360152
  (defun kb/para-split-sentences ()
    "Separate sentences of paragraph with newlines."
    (interactive)
    (unless buffer-read-only
      (let ((end-para (save-excursion (end-of-paragraph-text) (point))))
        (save-excursion
          (start-of-paragraph-text)
          (while (< (point) end-para)
            (forward-sentence)
            ;; Delete spaces between sentences before making new new line
            (delete-horizontal-space)
            ;; Don't add a new line, if already at the end of the line
            (unless (= (line-end-position) (point))
              (newline 2)))))))

  (defun kb/para-merge-sentences (&optional beg end)
    "In the active region"
    (interactive "r")
    (replace-regexp "\n" " " nil beg end))

  ;;;;; Smog-mode
  ;; Report statistics on writing style, word use and readability of prose
  (use-package smog
    :ensure-system-package diction
    :custom
    (smog-command "style -L en --print-nom-passive"))

  ;;;; EAF
;; The Emacs application framework.
(use-package eaf
  :disabled
  ;; HACK 2023-07-16: Easier to install this package as a submodule since
  ;; managing external scripts with elpaca is currently a pain...
  :load-path "./site-lisp/emacs-application-framework/"
  :ensure nil
  :ensure-system-package (("/usr/share/licenses/python-opencv/" . python-opencv)
                          (gdb))     ; For debugging purposes, if I ever need to
  :custom
  ;; Install desired modules
  ;; NOTE 2023-07-14: These names are the module names minus the "eaf-" prefix.
  ;; "eaf-airshare" is "airshare," for example
  (eaf-apps-to-install
   '(video-player pdf-viewer markdown-previewer image-viewer browser))
  (eaf-app-extensions-alist
   '(("video-player" . eaf-video-extension-list)
     ;; ("office" . eaf-office-extension-list)
     ("pdf-viewer" . eaf-pdf-extension-list)
     ;; ("music-player" . eaf-music-extension-list)
     ;; ("mindmap" . eaf-mindmap-extension-list)
     ("markdown-previewer" . eaf-markdown-extension-list)
     ("image-viewer" . eaf-image-extension-list)
     ;; ("browser" . eaf-browser-extension-list)
     ))
  (eaf-config-location (concat no-littering-var-directory "eaf"))
  (eaf-buffer-background-color (face-attribute 'default :background)) ; Set background color to theme's background's
  (eaf-preview-display-function-alist
   '(;; ("org-previewer" . eaf--org-preview-display)
     ("markdown-previewer" . eaf--markdown-preview-display)))

  ;; Modules config

  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  ;; Browser
  (eaf-browser-continue-where-left-off t) ; Also note `eaf-browser-restore-buffers'
  (eaf-browser-enable-adblocker t)
  (eaf-browser-default-search-engine "duckduckgo")
  (eaf-browser-blank-page-url "https://duckduckgo.com")
  (eaf-browser-download-path "/tmp")
  (eaf-browser-default-zoom 1.25)

  ;; PDF
  (eaf-pdf-notify-file-changed nil)

  ;; Dark mode?
  (eaf-browser-dark-mode "follow")
  (eaf-terminal-dark-mode "follow")
  (eaf-mindmap-dark-mode "follow")
  (eaf-pdf-dark-mode "ignore")

  (eaf-enable-debug nil)                ; Enable if there is a seg-fault
  :init
  ;; HACK 2023-07-14: Don't demand since EAF can't be loaded while Emacs is
  ;; non-GUI. See, e.g.,
  ;; https://github.com/emacs-eaf/emacs-application-framework/issues/1042
  (add-hook (if (daemonp) 'server-after-make-frame-hook 'after-init-hook)
            #'(lambda () (require 'eaf)))
  :config
  ;; All-the-icons integration
  (require 'eaf-all-the-icons)

  ;; All modules
  ;; (require 'eaf-airshare)
  (require 'eaf-browser)
  ;; (require 'eaf-camera)
  ;; (require 'eaf-demo)
  ;; (require 'eaf-file-browser)
  ;; (require 'eaf-file-manager)
  ;; (require 'eaf-file-sender)
  (require 'eaf-image-viewer)
  ;; (require 'eaf-jupyter)
  (require 'eaf-markdown-previewer)
  ;; (require 'eaf-mermaid) ; NOTE 2022-05-25: Dependency error with eslint currently
  ;; (require 'eaf-mindmap)
  ;; (require 'eaf-music-player)
  ;; (require 'eaf-org-previewer)
  (require 'eaf-pdf-viewer)
  ;; (require 'eaf-system-monitor)
  ;; (require 'eaf-terminal)
  (require 'eaf-video-player)
  ;; (require 'eaf-vue-demo)
  ;; (require 'eaf-netease-cloud-music)
  ;; (require 'eaf-rss-reader)
  ;; (require 'eaf-git)

  ;; Bindings
  (with-eval-after-load 'eaf-browser
    (eaf-bind-key clear_focus "<escape>" eaf-browser-keybinding)
    (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
  (with-eval-after-load 'eaf-pdf-viewer
    (eaf-bind-key rotate_counterclockwise "C-<up>" eaf-pdf-viewer-keybinding)
    (eaf-bind-key rotate_clockwise "C-<down>" eaf-pdf-viewer-keybinding)
    (eaf-bind-key winner-undo "C-<left>" eaf-pdf-viewer-keybinding)
    (eaf-bind-key winner-redo "C-<right>" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_down_page "C-u" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_up_page "C-d" eaf-pdf-viewer-keybinding)
    (eaf-bind-key nil "SPC" eaf-pdf-viewer-keybinding)
    (eaf-bind-key nil "g" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_begin "gg" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eyebrowse-last-window-config "gv" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eyebrowse-prev-window-config "ga" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eyebrowse-next-window-config "gt" eaf-pdf-viewer-keybinding))

  (with-eval-after-load 'dash-docs
    (setq dash-docs-browser-func 'eaf-open-browser)))
;; ...And the clipboard on Windows is often a wider encoding (UTF-16), so leave
  ;; Emacs to its own devices there.
  (unless krisb-system-win-p
    (setq selection-coding-system 'utf-8))

    ;;;; Custom processing of #+INCLUDE keyword
  ;; Use denote links or denote:DENOTEID as the file path for #+INCLUDE keywords
  (with-eval-after-load 'ox
    (require 'denote)
    (defun kb/org-export-parse-include-value (value &optional dir)
      "Extract the various parameters from #+include: VALUE.

  More specifically, this extracts the following parameters to a
  plist: :file, :coding-system, :location, :only-contents, :lines,
  :env, :minlevel, :args, and :block.

  The :file parameter is expanded relative to DIR.

  The :file, :block, and :args parameters are extracted
  positionally, while the remaining parameters are extracted as
  plist-style keywords.

  Any remaining unmatched content is passed through
  `org-babel-parse-header-arguments' (without evaluation) and
  provided as the :unmatched parameter.

  This version that overrides the original takes into account denote IDs.
  See the HACK comment below."
      (let* (location
             (coding-system
              (and (string-match ":coding +\\(\\S-+\\)>" value)
                   (prog1 (intern (match-string 1 value))
                     (setq value (replace-match "" nil nil value)))))
             (file
              (and (string-match "^\\(\".+?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)" value)
                   (let ((matched (match-string 1 value)) stripped)
                     (setq value (replace-match "" nil nil value))
                     (when (string-match "\\(::\\(.*?\\)\\)\"?\\'"
                                         matched)
                       (setq location (match-string 2 matched))
                       (setq matched
                             (replace-match "" nil nil matched 1)))
                     ;; HACK 2024-02-25: Added the following sexp. Checks if link
                     ;; has a denote ID, and if so, changes the matched (file) to
                     ;; the file corresponding to that ID. Allows me to use denote
                     ;; links as the included file
                     (when (string-match denote-id-regexp matched)
                       (setq matched
                             (denote-get-path-by-id (match-string 0 matched))))
                     (setq stripped (org-strip-quotes matched))
                     (if (org-url-p stripped)
                         stripped
                       (expand-file-name stripped dir)))))
             (only-contents
              (and (string-match ":only-contents *\\([^: \r\t\n]\\S-*\\)?"
                                 value)
                   (prog1 (org-not-nil (match-string 1 value))
                     (setq value (replace-match "" nil nil value)))))
             (lines
              (and (string-match
                    ":lines +\"\\([0-9]*-[0-9]*\\)\""
                    value)
                   (prog1 (match-string 1 value)
                     (setq value (replace-match "" nil nil value)))))
             (env (cond
                   ((string-match "\\<example\\>" value) 'literal)
                   ((string-match "\\<export\\(?: +\\(.*\\)\\)?" value)
                    'literal)
                   ((string-match "\\<src\\(?: +\\(.*\\)\\)?" value)
                    'literal)))
             ;; Minimal level of included file defaults to the
             ;; child level of the current headline, if any, or
             ;; one.  It only applies is the file is meant to be
             ;; included as an Org one.
             (minlevel
              (and (not env)
                   (if (string-match ":minlevel +\\([0-9]+\\)" value)
                       (prog1 (string-to-number (match-string 1 value))
                         (setq value (replace-match "" nil nil value)))
                     (get-text-property (point)
                                        :org-include-induced-level))))
             (args (and (eq env 'literal)
                        (prog1 (match-string 1 value)
                          (when (match-string 1 value)
                            (setq value (replace-match "" nil nil value 1))))))
             (block (and (or (string-match "\"\\(\\S-+\\)\"" value)
                             (string-match "\\<\\(\\S-+\\)\\>" value))
                         (or (= (match-beginning 0) 0)
                             (not (= ?: (aref value (1- (match-beginning 0))))))
                         (prog1 (match-string 1 value)
                           (setq value (replace-match "" nil nil value))))))
        (list :file file
              :coding-system coding-system
              :location location
              :only-contents only-contents
              :lines lines
              :env env
              :minlevel minlevel
              :args args
              :block block
              :unmatched (org-babel-parse-header-arguments value t))))
 \\<\\(\\S-+\\)\\>" value))
                       (or (= (match-beginning 0) 0)
