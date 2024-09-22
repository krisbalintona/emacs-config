;;; markdown-general-rcp.el --- Markdown             -*- lexical-binding: t; -*-

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

;; My configuration related to markdown-mode.

;;; Code:

;;;; Markdown-mode
(use-package markdown-mode
  :mode ("INSTALL\\'" "CONTRIBUTORS\\'" "LICENSE\\'" "README\\'")
  :hook
  (markdown-mode . visual-line-mode))

;;;; Markdown-xwidget
;; Similar to `grip-mode' but avoids sending many requests to GitHub's API and
;; more customization. However, `grip-mode' shows exactly what GitHub would show
(use-package markdown-xwidget
  :ensure-system-package pandoc
  ;; :ensure (:type git
  ;;                :host github
  ;;                :repo "cfclrk/markdown-xwidget"
  ;;                :files (:defaults "resources"))
  :vc (:url "https://github.com/cfclrk/markdown-xwidget.git")
  :bind
  ( :map markdown-mode-command-map
    ("x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-command "pandoc")
  (markdown-xwidget-github-theme "dark")
  (markdown-xwidget-mermaid-theme "dark")
  (markdown-xwidget-code-block-theme "github-dark"))

(provide 'markdown-general-rcp)
;;; markdown-general-rcp.el ends here
