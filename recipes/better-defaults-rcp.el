;;; better-defaults-rcp.el --- Better setq-default settings  -*- lexical-binding: t; -*-

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

;; Set more sane Emacs-wide settings and minor QoL changes.

;;; Code:
(require 'personal-variables-rcp)

;;;; Disable startup echo message
;; See `startup-echo-area-message'
(fset #'display-startup-echo-area-message #'ignore)

;;;; Require pin-entry for passowrds
;; Pinentry is responsible for querying passphrases
(require 'epg-config)
(setq epg-pinentry-mode 'loopback) ; Ask through the minibuffer, instead of external Pinentry program

;;;; Toggle visiting of image files as images (Auto Image File mode).
(auto-image-file-mode t)

;;;; Recenter upon `next-error'
(setq next-error-recenter '(4))

;;;; Indent and formatting
(setq-default left-fringe-width  8
              right-fringe-width 8)

;;;; Truncate lines
;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;;;; Continuation line indicator character
;; See for an explanation of these concepts
;; https://www.reddit.com/r/emacs/comments/1fxr1ci/comment/lqpf2bz/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(set-display-table-slot standard-display-table 1 ?⏎)

;;;; Header line text scaling
(setq-default text-scale-remap-header-line t)

;;;; More predictable window selection of `scroll-other-window' and `scroll-other-window-down'
;; Taken from
;; https://karthinks.com/software/emacs-window-management-almanac/#scroll-other-window--built-in
(setq other-window-scroll-default
      (lambda ()
        (or (get-mru-window nil nil 'not-this-one-dummy)
            (next-window)               ; Fall back to next window
            (next-window nil nil 'visible))))

(provide 'better-defaults-rcp)
;;; better-defaults-rcp.el ends here
