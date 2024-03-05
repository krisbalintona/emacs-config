;;; garbage-collection-rcp.el --- GC stuff           -*- lexical-binding: t; -*-

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

;; Faster Emacs startup and session.
;; Other things that can help:
;; 1) Native compilation (`libgccjit')
;; 2) Native (`libjansson') JSON support (alternative Elisp parser)
;;
;; The flags I use when manually compiling Emacs are:
;; CFLAGS='-march=native -O2' ./configure --with-dbus --with-gif --with-xwidgets --with-modules --with-json --with-x-toolkit=gtk3 --with-native-compilation --enable-link-time-optimization --with-imagemagick --with-mailutils --with-tree-sitter
;; This follows ./autogen.sh. Then I run make -j$(nproc) and then make install.

;;; Code:

;; NOTE 2024-02-11: PLEASE REFERENCE https://emacsconf.org/2023/talks/gc/ FOR A
;; STATISTICALLY-INFORMED RECOMMENDATION FOR GC VARIABLES
(setq garbage-collection-messages t)
(setq gc-cons-percentage 0.15)

(add-hook (if (featurep 'elpaca)
              'elpaca-after-init-hook
            'after-init-hook)
          (lambda () (setq gc-cons-threshold 800000)))

;;;; GCMH
;; Garbage collect on when idle
(use-package gcmh
  :diminish
  :hook ((after-init . gcmh-mode)
         (minibuffer-setup . kb/gcmh-minibuffer-setup)
         (minibuffer-exit . kb/gcmh-minibuffer-exit))
  :custom
  ;; For a related discussion, see
  ;; https://www.reddit.com/r/emacs/comments/bg85qm/comment/eln27qh/?utm_source=share&utm_medium=web2x&context=3
  (gcmh-high-cons-threshold (* 16       ; 16 mb, as Doom uses in doom-start.el
                               1024 1024))
  (gcmh-idle-delay 3)
  (gcmh-verbose nil)
  :config
  (setq garbage-collection-messages nil)

  ;; Increase GC threshold when in minibuffer
  (defvar kb/gc-minibuffer--original gcmh-high-cons-threshold
    "Temporary variable to hold `gcmh-high-cons-threshold'")

  (defun kb/gcmh-minibuffer-setup ()
    "Temporarily have \"limitless\" `gc-cons-threshold'."
    ;; (message "[kb/gcmh-minibuffer-setup] Increasing GC threshold")
    (setq gcmh-high-cons-threshold most-positive-fixnum))

  (defun kb/gcmh-minibuffer-exit ()
    "Restore value of `gc-cons-threshold'."
    ;; (message "[kb/gcmh-minibuffer-exit] Restoring GC threshold")
    (setq gcmh-high-cons-threshold kb/gc-minibuffer--original))

  ;; Increase `gc-cons-threshold' while using corfu, like we do for the
  ;; minibuffer
  (with-eval-after-load 'corfu
    (advice-add 'completion-at-point :before 'kb/gcmh-minibuffer-setup)
    (advice-add 'corfu-quit :before 'kb/gcmh-minibuffer-exit)
    (advice-add 'corfu-insert :before 'kb/gcmh-minibuffer-exit)))

;;;; Diagnose memory usage
;; See how Emacs is using memory. From
;; https://www.reddit.com/r/emacs/comments/ck4zb3/comment/evji1n7/?utm_source=share&utm_medium=web2x&context=3
(defun kb/diagnose-garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))

;;;; Emacs-gc-stats
;; Collect GC statistics. Requested by someone who'd like GC statistics:
;; https://www.reddit.com/r/emacs/comments/14dej62/please_help_collecting_statistics_to_optimize/.
;; Also see https://elpa.gnu.org/packages/emacs-gc-stats.html
(use-package emacs-gc-stats
  :disabled
  :custom
  ;; Optionally reset Emacs GC settings to default values (recommended)
  (emacs-gc-stats-gc-defaults 'emacs-defaults)
  (emacs-gc-stats-remind (* 7))  ; Optionally set reminder to upload the stats
  (emacs-gc-stats-inhibit-command-name-logging nil)
  :init
  (emacs-gc-stats-mode))

(provide 'garbage-collection-rcp)
;;; garbage-collection-rcp.el ends here
