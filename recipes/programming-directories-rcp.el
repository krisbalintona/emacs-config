;;; programming-directories-rcp.el --- Traversing and seeing directories  -*- lexical-binding: t; -*-

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

;; Everything to do with navigating and managing directories.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Dired
;;;;; Image-dired
(use-package image-dired
  :ensure nil
  :hook (dired-mode . image-dired-minor-mode)
  :custom
  (image-dired-thumb-size 150)          ; Slightly larger thumbnails
  (image-dired-external-viewer "xdg-open")
  (image-dired-thumb-relief 0)
  (dired-mouse-drag-files t))

;;;;; Dired-git
;; Show git information in dired
(use-package dired-git
  :disabled
  :hook
  (dired-mode . dired-git-mode)
  :custom
  (dired-git-disable-dirs '("~/"))
  (dired-git-parallel 7))               ; Number of parallel processes

;;;;; Dired-open
;; Override how dired opens files with certain extensions
(use-package dired-open
  :after dired
  :custom
  (dired-open-extensions '(("odt" . "soffice -writer")
                           ("docx" . "soffice -writer")
                           ("mp4" . "vlc")
                           ("mp3" . "vlc")
                           ("mkv" . "vlc")
                           ))
  :config
  ;; Try to use `xdg-open' before anything else
  (add-to-list 'dired-open-functions #'dired-open-xdg t)) ; Doesn't work as expected!

;;;;; Dired-hide-dotfiles
;; Hide dotfiles
(use-package dired-hide-dotfiles
  :custom
  (dired-hide-dotfiles-verbose nil)) ; No announcements about hiding in echo area

;;;;; Dired-hist
;; History for dired buffers
(use-package dired-hist
  ;; :ensure (:host github :repo "karthink/dired-hist")
  :vc (:url "https://github.com/karthink/dired-hist.git")
  :after dired
  :demand
  :bind
  ( :map dired-mode-map
    ("l" . dired-hist-go-back)
    ("r" . dired-hist-go-forward))
  :config
  (dired-hist-mode 1))

;;;;; Consult-dir
(use-package consult-dir
  :bind
  ([remap dired] . consult-dir))

;;;; Misc

;;;;; Dwim-shell-command
;; Many convenient wrappers involving shell commands in and out of `dired' (with
;; the ability to easily create my own)
(use-package dwim-shell-command
  :bind
  (([remap async-shell-command] . dwim-shell-command)
   :map dired-mode-map
   ([remap dired-do-async-shell-command] . dwim-shell-command)))

(provide 'programming-directories-rcp)
;;; programming-directories-rcp.el ends here
