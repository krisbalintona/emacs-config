;; -*- lexical-binding: t; -*-

;;; Emacs-gc-stats
;; Collect GC statistics. Requested by someone who'd like GC statistics:
;; https://www.reddit.com/r/emacs/comments/14dej62/please_help_collecting_statistics_to_optimize/.
;; Also see https://elpa.gnu.org/packages/emacs-gc-stats.html
(use-package emacs-gc-stats
  :disabled t                           ; Dont collecting data
  :hook (on-first-input . emacs-gc-stats-mode)
  :custom
  ;; Optionally reset Emacs GC settings to default values (recommended)
  (emacs-gc-stats-gc-defaults 'emacs-defaults)
  (emacs-gc-stats-remind (* 7))  ; Optionally set reminder to upload the stats
  (emacs-gc-stats-inhibit-command-name-logging nil))

;;; Provide
(provide 'krisb-garbage-collection)
