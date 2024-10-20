;;; misc-packages-rcp.el --- Miscellaneous packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; I dunno where else to put these packages.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

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

;;;; Info
;;;;; Inform
;; Package `inform’ provides links from elisp symbols (quoted functions,
;; variables and fonts) in Gnu-Emacs Info viewer to their help documentation.
(use-package inform
  :disabled
  :demand
  :after info)

;;;; Timers
;;;;; Work-timer
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

;;;; Built-in Emacs modes/packages
(use-package emacs
  :ensure nil
  :bind (:map universal-argument-map    ; Multiple universal arguments
              ("u" . universal-argument-more))
  :custom
  (save-interprogram-paste-before-kill t)
  ;; Killing
  (kill-do-not-save-duplicates t)
  (kill-ring-deindent-mode nil)
  (window-divider-default-places 'bottom-only))

;;;;; Proced
;; Built in process monitor
(use-package proced
  :ensure nil
  :custom
  (proced-auto-update-flag t)           ; Update live
  (proced-auto-update-interval 1)
  (proced-descend t)                    ; Descending order?
  (proced-filter 'all)                  ; Which processes are shown?
  :config
  (with-eval-after-load 'evil
    (add-hook 'proced-mode-hook #'evil-emacs-state)))

;;;;; Midnight
(use-package midnight
  :ensure nil
  :defer 20
  :config
  (midnight-mode 1))

;;;;; Pixel-scroll
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

;;;; Other

;;;;; Clippety
;; Nearly system-wide mutual clipboard support
(use-package clipetty
  :hook (on-first-buffer . global-clipetty-mode)
  :diminish)

;;;;; Image-popup
;; Match height of image to line height (?)
(use-package image-popup
  ;; :ensure (image-popup :type git :host gitlab :repo "OlMon/image-popup" :branch "master")
  :vc (:url "https://gitlab.com/OlMon/image-popup.git"
            :rev :master
            :branch "master")
  :hook ((eww-after-render nov-post-html-render) . image-popup-reload))

;;;;; Casual
;; A suite of "casual" interfaces.
(use-package casual-suite
  ;; TODO 2024-10-07: Try to avoid using demand
  :demand
  :bind
  ( :map ibuffer-mode-map
    ("C-M-s-\\" . casual-ibuffer-tmenu)
    :map Info-mode-map
    ("C-M-s-\\" . casual-info-tmenu)
    :map calc-mode-map
    ("C-M-s-\\" . casual-calc-tmenu)))

;;;;; Async.el
;; Async library and a few small but useful implementations
(use-package async
  :hook ((on-first-buffer . dired-async-mode)
         (on-first-buffer . async-bytecomp-package-mode))
  :custom
  (async-bytecomp-allowed-packages 'all))

;;;;; Super-hint
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

;;;;; Wgrep
;; Edit lines in grep buffers
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(provide 'misc-packages-rcp)
;;; misc-packages-rcp.el ends here
