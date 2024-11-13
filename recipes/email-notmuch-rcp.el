;;; email-notmuch-rcp.el --- Notmuch email client    -*- lexical-binding: t; -*-

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

;; These are packages pertinent to using the `notmuch' email client. It is also
;; important to note that this is only a frontend for the `notmuch' command line
;; tool. Syncing from remote servers can be done via `mbsync' (from the `mu'
;; utility set) and `offlineimap', whose configuration files are `.mbsyncrc' and
;; `.offlineimaprc', respectively.
;;
;; Protesilaos provides a wonderful guide here:
;; https://protesilaos.com/emacs/dotemacs#h:5ad80664-3163-4d9d-be65-462637d77903

;;; Code:
(require 'keybinds-general-rcp)

;;;; Notmuch-indicator
(use-package notmuch-indicator
  :disabled   ; REVIEW 2024-09-29: Trying out simple display-time mail indicator
  :after notmuch
  :demand
  :custom
  (notmuch-indicator-add-to-mode-line-misc-info nil) ; I add it to the modeline myself
  (notmuch-indicator-counter-format "%s%s")
  (notmuch-indicator-args '(( :terms "tag:unread and tag:inbox"
                              :label "M:"
                              :label-face modus-themes-fg-green)))
  (notmuch-indicator-refresh-count (* 60 3))
  (notmuch-indicator-hide-empty-counters t)
  (notmuch-indicator-force-refresh-commands '(notmuch notmuch-refresh-this-buffer))
  :config
  (notmuch-indicator-mode 1)

  ;; Override default mode line construct
  (setq-default notmuch-indicator-mode-line-construct
                '(notmuch-indicator-mode ((:eval notmuch-indicator--counters) " ")))
  ;; Add to mode line myself
  (add-to-list 'global-mode-string 'notmuch-indicator-mode-line-construct))

(provide 'email-notmuch-rcp)
;;; email-notmuch-rcp.el ends here
