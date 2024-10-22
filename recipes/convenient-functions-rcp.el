;;; convenient-functions-rcp.el --- Bespoke miscellaneous functions  -*- lexical-binding: t; -*-

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

;; These are small groups of code, many of which are self-defined, that I find
;; useful. Most of these functions are taken from elsewhere (e.g. Doom).

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'custom-directories-rcp)

;;;; Indent whole buffer
(defun kb/format-buffer-indentation--base (&optional beg end)
  "Basic indentation fix using `indent-region'.
By default, indents entire buffer. If BEG and END are specified,
act upon that region instead."
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

(defun kb/format-buffer-indentation--fill-column (beg end)
  "Basic indentation fix and wrap comments."
  (kb/format-buffer-indentation--base (or beg (point-min)) (or end (point-max)))
  (unless (region-active-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward comment-start nil t)
        (call-interactively 'fill-paragraph)
        (forward-line 1)))))

(defun kb/format-buffer-indentation (beg end)
  "Properly indent the entire buffer."
  (interactive (list
                (if (region-active-p) (region-beginning) (point-min))
                (if (region-active-p) (region-end) (point-max))))
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (let ((start-time (current-time)))
    (message "Formatting buffer...")
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (kb/format-buffer-indentation--base beg end))
     ((eq major-mode 'inferior-emacs-lisp-mode)
      (kb/format-buffer-indentation--base
       (save-excursion (end-of-buffer) (comint-bol))
       (save-excursion (end-of-buffer) (comint-next-prompt 1))))
     ((eq major-mode 'conf-mode)
      (conf-align-assignments)
      (kb/format-buffer-indentation--base beg end))
     ((eq major-mode 'latex-mode)
      (kb/format-buffer-indentation--base beg end)
      (require 'latex-general-rcp)
      (kb/tabular-magic))
     ((eq major-mode 'org-mode)
      (kb/format-buffer-indentation--base beg end)
      (kb/org-add-blank-lines (unless (region-active-p) 'whole-buffer))
      (org-align-tags (not (region-active-p)))
      (when (buffer-file-name)
        (save-buffer)))
     ((eq major-mode 'racket-mode)
      (kb/format-buffer-indentation--base beg end)
      (when (buffer-file-name)
        (save-buffer)))
     ((and (require 'apheleia nil t)
           (apheleia--get-formatters))    ; If available apheleia formatter
      (let* ((apheleia-mode t))           ; Save silently
        (apheleia-format-after-save)))
     ((derived-mode-p 'message-mode)
      (delete-trailing-whitespace))
     ((derived-mode-p '(prog-mode conf-mode))
      (kb/format-buffer-indentation--fill-column beg end))
     (t
      (kb/format-buffer-indentation--base beg end)))
    (message (format-time-string "Formatting buffer... Done. Took %s.%3N seconds."
                                 (float-time (time-subtract (current-time) start-time))))))
(bind-key [remap indent-region] #'kb/format-buffer-indentation)

(provide 'convenient-functions-rcp)
;;; convenient-functions-rcp.el ends here
