;;; epub-rcp.el --- Epub                             -*- lexical-binding: t; -*-

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

;; Reading .epub files.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Nov-mode
;; EPub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
         (nov-mode . olivetti-mode))
  :config
  (with-eval-after-load 'eaf
    (add-to-list 'eaf-find-file-ext-blacklist "epub")))

;;;; Justify-kp
;; Advanced justification of text with the Knuth/Plass algorithm
(use-package justify-kp
  :vc (:url "https://github.com/Fuco1/justify-kp.git"
            :rev :newest)
  :hook (nov-post-html-render . kb/nov-post-html-render-hook)
  :config
  (defun kb/nov-window-configuration-change-hook ()
    (kb/nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook 'kb/nov-window-configuration-change-hook t))

  (defun kb/nov-post-html-render-hook ()
    (if (get-buffer-window)
        (let ((max-width (pj-line-width))
              buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
                (goto-char (line-end-position))
                (when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      (add-hook 'window-configuration-change-hook 'kb/nov-window-configuration-change-hook nil t))))

(provide 'epub-rcp)
;;; epub-rcp.el ends here
