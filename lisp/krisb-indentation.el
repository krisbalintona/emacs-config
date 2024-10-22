;;; krisb-indentation.el --- Bespoke utilities for consistent indentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: convenience, tools, convenience

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

;; Functions and commands used for easily maintaining consistent indentation and
;; whitespace in buffers.

;;; Code:

;;; Indent whole buffer
(declare-function comint-bol "comint")
(declare-function comint-next-prompt "comint")
(declare-function conf-align-assignments "conf-mode")
(declare-function apheleia--get-formatters "apheleia")
(declare-function org-align-tags "org")
(declare-function krisb-org-ext-add-blank-lines "krisb-org-ext")

(defun krisb-format-buffer-indentation--base (&optional beg end)
  "Basic indentation fix using `indent-region'.
By default, indents entire buffer.  If BEG and END are specified, act
upon that region instead."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (untabify beg end)           ; Untabify before the following alters points
      ;; FIXME 2024-01-13: Because untabify could potentially increase the
      ;; number of tabs in the region, indent-region could potentially fail to
      ;; indent in the edge case that characters near the end of the region need
      ;; to be indented.
      (indent-region beg end nil)
      (delete-trailing-whitespace))))

(defun krisb-format-buffer-indentation--fill-comments (&optional beg end)
  "Basic indentation fill comments.
Filling of commands only occurs when region is not active.

See `krisb-format-buffer-indentation--base' for an explanation of the
parameters BEG and END."
  (krisb-format-buffer-indentation--base beg end)
  (unless (region-active-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward comment-start nil t)
        (call-interactively 'fill-paragraph)
        (forward-line 1)))))

;;;###autoload
(defun krisb-format-buffer-indentation (&optional beg end)
  "DWIM indent the buffer.
If called from Lisp, indent region from BEG to END, defaulting to the
first and last point, respectively.  If interactive, either indent the
entire buffer or the region if active."
  (interactive (list
                (when (region-active-p) (region-beginning))
                (when (region-active-p) (region-end))))
  (let ((start-time (current-time)))
    (message "Formatting buffer...")
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (krisb-format-buffer-indentation--base beg end))
     ((eq major-mode 'inferior-emacs-lisp-mode)
      (krisb-format-buffer-indentation--base
       (save-excursion (goto-char (point-max)) (comint-bol))
       (save-excursion (goto-char (point-max)) (comint-next-prompt 1))))
     ((eq major-mode 'conf-mode)
      (conf-align-assignments)
      (krisb-format-buffer-indentation--base beg end))
     ((eq major-mode 'org-mode)
      (org-align-tags (not (region-active-p)))
      (krisb-org-ext-add-blank-lines (unless (region-active-p) 'whole-buffer))
      (krisb-format-buffer-indentation--base beg end))
     ((apheleia--get-formatters)   ; If there is an available apheleia formatter
      (call-interactively 'apheleia-format-buffer))
     ((derived-mode-p 'message-mode)
      (delete-trailing-whitespace))
     ((derived-mode-p 'prog-mode)
      (krisb-format-buffer-indentation--fill-comments beg end))
     (t
      (krisb-format-buffer-indentation--base beg end)))
    (message (format-time-string "Formatting buffer... Done. Took %s.%3N seconds."
                                 (float-time (time-subtract (current-time) start-time))))))

;;; Provide
(provide 'krisb-indentation)
;;; krisb-indentation.el ends here
