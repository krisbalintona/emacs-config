;;; org-third-time.el --- Implement Third Time with `org-timer' -*- lexical-binding: t; -*-

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://gitlab.com/PreciousPudding/org-third-time
;; Created: Feb 1, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This package adapts the Third Time system introduced in
;; https://www.lesswrong.com/posts/RWu8eZqbwgB9zaerh/third-time-a-better-way-to-work.
;; This is an alternative to `org-pomorodo-third-time':
;; https://github.com/telotortium/org-pomodoro-third-time

;;; Code:

;; Have custom versions of `org-timer-set-timer', `org-timer-pause-or-continue',
;; and `org-timer' variables for storing timers and times. A new work cycle is
;; manually started. When one is, with `C-u', the user will be prompted for a
;; custom period of time.

;; Have a relative timer. But alongside that, have a timer for the expected
;; length of the work cycle (if applicable).

;; Like `org-timer', the timers can be paused and continued on command

;; After a work cycle is terminated (manually), a user-customizable predicate
;; will be called. This predicate will determine the length (in seconds) of the
;; break time.

;; Have a variable that has the options to have only a relative timer, only a
;; countdown timer, or both. There should be predicates (for determining the
;; behavior of each, e.g., length of countdown timer and maximum time for
;; relative timer) for each.

;; Customizable mode line format

;; This package is meant to be flexible enough for users with different needs in
;; different context. E.g., this package can be used by the university student
;; and office employee alike. This is primarily done through user-exposed custom
;; variables that determine the duration of work and break cycles.

;;; Variables
(defvar org-third-time-countup-timer nil
  "tk")
(defvar org-third-time-countdown-timer nil
  "tk")

(defvar org-third-time-start-time nil
  "tk")
(defvar org-third-time-pause-time nil
  "tk")

(defvar org-third-time-mode-line-timer nil
  "tk")
(defvar org-third-time-mode-line-string nil
  "tk")

;;; Functions
(defun org-third-time-parse-duration (s)
  "tk"
  ;; TODO 2023-02-24: Don't appropriate
  (tmr--parse-duration
   (current-time)
   (concat (number-to-string s) "s")))

(defun org-third-time-value-string ()
  "tk"
  (format "%s"
          (org-third-time-parse-duration
           (let ((time (- (float-time org-third-time-pause-time)
                          (float-time org-third-time-start-time))))
             (abs (floor (if org-third-time-countdown-timer (- time) time)))))))

;;; Mode line
(defun org-third-time-set-mode-line (value)
  "tk"
  (unless global-mode-string (setq global-mode-string '("")))
  (unless (memq 'org-third-time-mode-line-string global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(org-third-time-mode-line-string))))
  (cl-case value
    (off
     (cancel-timer org-third-time-mode-line-timer)
     (setq org-third-time-mode-line-timer nil)
     (setq global-mode-string
           (delq 'org-third-time-mode-line-string global-mode-string)))
    (on
     (setq org-third-time-mode-line-timer
           (run-with-timer 1 1 #'org-third-time-update-mode-line)))))

(defun org-third-time-update-mode-line ()
  "tk"
  (setq org-third-time-mode-line-string
        (concat " [" (org-third-time-value-string) "]"))
  (force-mode-line-update))

;;; Commands
(defun org-third-time-work-start ()
  "tk"
  (setq org-third-time-start-time (current-time)
        org-timer-pause-time nil)

  ;; (run-with-timer  nil (lambda ()
  ;;                            (setq org-third-time-countdown-timer nil)
  ;;                            ;; TODO 2023-02-24: Add sound and notification
  ;;                            ))
  )

(provide 'org-third-time)

;;; org-third-time.el ends here
