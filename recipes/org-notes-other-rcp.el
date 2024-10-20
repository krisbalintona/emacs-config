;;; org-notes-other-rcp.el --- Other note-taking things  -*- lexical-binding: t; -*-

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

;; Packages indirectly related to my note-taking workflow.

;;; Code:
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Generalized

;;;;; Org-remark
;;;;;; Itself
(use-package org-remark
  :diminish (org-remark-mode org-remark-global-tracking-mode)
  :hook (on-first-file . org-remark-global-tracking-mode)
  :bind
  ( :map org-remark-mode-map
    ("C-c r r" . (lambda () (interactive) (org-remark-highlights-load)))
    ("C-c r l" . org-remark-mark-line)
    ("C-c r d" . org-remark-delete)
    ("C-c r c" . org-remark-change)
    ("C-c r t" . org-remark-toggle)
    ("C-c r o" . org-remark-open)
    ("C-c r v" . org-remark-view)
    ("C-c r n" . org-remark-next)
    ("C-c r p" . org-remark-prev)
    :repeat-map kb/org-remark-mode-repeat-map
    ("d" . org-remark-delete)
    ("c" . org-remark-change)
    ("t" . org-remark-toggle)
    ("o" . org-remark-open)
    ("v" . org-remark-view)
    ("n" . org-remark-next)
    ("p" . org-remark-prev))
  :custom
  (org-remark-source-file-name 'abbreviate-file-name)
  (org-remark-notes-file-name
   (no-littering-expand-var-file-name "org-remark/marginalia.org"))
  (org-remark-notes-display-buffer-action `((display-buffer-in-side-window)
                                            (side . right)
                                            (slot . 1)))
  (org-remark-create-default-pen-set nil) ; Make my own pens
  (org-remark-notes-auto-delete nil)
  :config
  (with-eval-after-load 'eww
    (org-remark-eww-mode 1))
  (with-eval-after-load 'nov
    (org-remark-nov-mode 1))
  (with-eval-after-load 'info
    (org-remark-info-mode 1))

  (with-eval-after-load 'all-the-icons
    (setopt org-remark-icon-notes (all-the-icons-material "details")
            org-remark-icon-position-adjusted (all-the-icons-material "error")
            org-remark-line-icon (all-the-icons-faicon "sticky-note"))))

;;;;;; Custom `org-remark' pens
;; Sets up `org-remark' faces according to the following schema:
;; - Resonant (red)
;; - Thesis (yellow)
;; - Detail (blue)
;; - Outline (green)
;; - External (purple/magenta)
(with-eval-after-load 'org-remark
  (defface kb/org-remark-resonant-face nil
    "Face for resonant annotations.")
  (defface kb/org-remark-resonant-minor-face nil
    "Face for less resonant (underlined) annotations.")

  (defface kb/org-remark-thesis-face nil
    "Face for thesis annotations.")
  (defface kb/org-remark-thesis-minor-face nil
    "Face for less thesis (underlined) annotations.")

  (defface kb/org-remark-detail-face nil
    "Face for detail annotations.")
  (defface kb/org-remark-detail-minor-face nil
    "Face for less detail (underlined) annotations.")

  (defface kb/org-remark-outline-face nil
    "Face for outline annotations.")
  (defface kb/org-remark-outline-minor-face nil
    "Face for less outline (underlined) annotations.")

  (defface kb/org-remark-external-face nil
    "Face for \"external\" annotations.")
  (defface kb/org-remark-external-minor-face nil
    "Face for less \"external\" (underlined) annotations.")

  (defun kb/org-remark-setup-pen-colors ()
    "Set up pen colors."
    (modus-themes-with-colors
      (set-face-attribute 'kb/org-remark-resonant-face nil
                          :background bg-red-intense)
      (set-face-attribute 'kb/org-remark-resonant-minor-face nil
                          :underline `(:color ,bg-red-intense :style wave))

      (set-face-attribute 'kb/org-remark-thesis-face nil
                          :background bg-yellow-subtle)
      (set-face-attribute 'kb/org-remark-thesis-minor-face nil
                          :underline `(:color ,bg-yellow-subtle :style wave))

      (set-face-attribute 'kb/org-remark-detail-face nil
                          :background bg-blue-subtle)
      (set-face-attribute 'kb/org-remark-detail-minor-face nil
                          :underline `(:color ,bg-blue-subtle :style wave))

      (set-face-attribute 'kb/org-remark-outline-face nil
                          :background bg-green-subtle)
      (set-face-attribute 'kb/org-remark-outline-minor-face nil
                          :underline `(:color ,bg-green-subtle :style wave))

      (set-face-attribute 'kb/org-remark-external-face nil
                          :background bg-magenta-intense)
      (set-face-attribute 'kb/org-remark-external-minor-face nil
                          :underline `(:color ,bg-magenta-intense :style wave))))
  (kb/org-remark-setup-pen-colors)
  (add-hook 'kb/themes-hook #'kb/org-remark-setup-pen-colors)

  (org-remark-create "resonant"
                     'kb/org-remark-resonant-face
                     `(CATEGORY "resonant" help-echo "Annotation that resonates with me."))
  (org-remark-create "resonant-underline"
                     'kb/org-remark-resonant-minor-face
                     `(CATEGORY "resonant" help-echo "Annotation that resonates with me but I don't want to be as noticeable."))

  (org-remark-create "thesis"
                     'kb/org-remark-thesis-face
                     `(CATEGORY "thesis" help-echo "Annotation that denotes something relevant to a thesis."))
  (org-remark-create "thesis-underline"
                     'kb/org-remark-thesis-minor-face
                     `(CATEGORY "thesis" help-echo "Annotation that denotes something relevant to a thesis but I don't want to be as noticeable."))

  (org-remark-create "detail"
                     'kb/org-remark-detail-face
                     `(CATEGORY "detail" help-echo "Annotation that denotes a notable detail."))
  (org-remark-create "detail-underline"
                     'kb/org-remark-detail-minor-face
                     `(CATEGORY "detail" help-echo "Annotation that denotes a notable detail but I don't want to be as noticeable."))

  (org-remark-create "outline"
                     'kb/org-remark-outline-face
                     `(CATEGORY "outline" help-echo "Annotation that foreshadows structure or main idea(s)."))
  (org-remark-create "outline-underline"
                     'kb/org-remark-outline-minor-face
                     `(CATEGORY "outline" help-echo "Annotation that foreshadows structure or main idea(s) but I don't want to be as noticeable."))

  (org-remark-create "external"
                     'kb/org-remark-external-face
                     `(CATEGORY "external" help-echo "Annotation that resonates with me but is external to the text."))
  (org-remark-create "external-underline"
                     'kb/org-remark-external-minor-face
                     `(CATEGORY "external" help-echo "Annotation that resonates with me but is external to the text but I don't want to be as noticeable."))

  (require 'transient)
  (transient-define-prefix kb/org-remark-mark-transient ()
    "Transient menu for my pre-defined `org-remark' pens."
    [["Resonant"
      ("r" "Highlight" org-remark-mark-resonant)
      ("R" "Underline" org-remark-mark-resonant-underline)]
     ["Thesis"
      ("t" "Highlight" org-remark-mark-thesis)
      ("T" "Underline" org-remark-mark-thesis-underline)]]
    [["Detail"
      ("d" "Highlight" org-remark-mark-detail)
      ("D" "Underline" org-remark-mark-detail-underline)]
     ["Outline"
      ("o" "Highlight" org-remark-mark-outline)
      ("O" "Underline" org-remark-mark-outline-underline)]
     ["External"
      ("e" "Highlight" org-remark-mark-external)
      ("E" "Underline" org-remark-mark-external-underline)]])
  (bind-key "C-c r m" #'kb/org-remark-mark-transient 'org-remark-mode-map))

;;;;; Org-transclusion
;; Enable transclusion of org files
(use-package org-transclusion
  :hook (org-mode . org-transclusion-mode)
  :custom
  (org-transclusion-include-first-section t)
  (org-transclusion-exclude-elements '(property-drawer)))

;;;;; Paw
(use-package paw
  :disabled    ; NOTE 2024-09-23: Such messy code and idiosyncratic practices...
  :vc (:url "https://github.com/chenyanming/paw.git"
            :rev :newest)
  :hook
  (wallabag-entry-mode . paw-annotation-mode)
  :custom
  ;; TODO 2024-09-23: The parent directory of the following three paths must be
  ;; set, or paw errors... Make issues about this.
  (paw-cache-dir (no-littering-expand-var-file-name "paw/cache"))
  (paw-tts-cache-dir (no-littering-expand-var-file-name "paw/edge-tts"))
  (paw-note-dir (no-littering-expand-var-file-name "paw/notes"))
  (paw-annotation-read-only-enable t)
  (paw-view-note-after-editting-note nil)
  (paw-svg-enable t))

;;;; PDFs
;;;;; Org-noter
(use-package org-noter
  ;; :ensure (:protocol ssh
  ;;                    :fetcher github
  ;;                    :repo "org-noter/org-noter"
  ;;                    :files ("*.el" "modules" (:exclude "*-test-utils.el" "*-devel.el"))
  ;;                    :remotes ("remote" :repo "krisbalintona/org-noter"))
  :vc (:url "https://github.com/krisbalintona/org-noter.git"
            :rev :newest)
  :bind
  ( :map pdf-misc-minor-mode-map
    (("I" . nil)
     ("M" . 'pdf-misc-display-metadata))
    :map org-noter-doc-mode-map
    (("i" . org-noter-insert-precise-note)
     ("I" . org-noter-insert-precise-note-toggle-no-questions)
     ("C-i" . org-noter-insert-note)
     ("C-S-i" . org-noter-insert-note-toggle-no-questions)
     ("C-M-i" . nil)
     ("M-i" . nil)
     ;; FIXME 2024-01-13: Choose better keybind
     ("C-M-s-\"" . org-noter-pdf--create-missing-annotation)))
  :custom
  (org-noter-notes-search-path `(,krisb-notes-directory))
  ;; FIXME 2024-01-12: I am not currently using org-noter, but when I do, I can
  ;; create a notes file and set it here like thus
  ;; (org-noter-default-notes-file-names
  ;;  (list (file-relative-name (car (directory-files-recursively
  ;;                                  krisb-notes-directory "20240111T235139"))
  ;;                            krisb-notes-directory)))
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session-end nil) ; Don't close frame when killing pdf buffer
  (org-noter-use-indirect-buffer t)
  (org-noter-disable-narrowing nil)
  (org-noter-hide-other t)
  (org-noter-auto-save-last-location nil)
  (org-noter-separate-notes-from-heading t)
  (org-noter-highlight-selected-text t) ; Always leave highlights from annotations
  (org-noter-arrow-foreground-color "red")
  (org-noter-arrow-background-color "black")
  (org-noter-doc-property-in-notes nil)
  (org-noter-insert-note-no-questions nil) ; Activate this setting if I rarely type my own titles
  (org-noter-max-short-selected-text-length 0) ; Always enclose in quote block
  :config
  (org-noter-enable-update-renames)

  (defun kb/org-noter-pdf--get-selected-text (mode)
    (when (and (eq mode 'pdf-view-mode)
               (pdf-view-active-region-p))
      (let* ((raw-text (mapconcat 'identity (pdf-view-active-region-text) ? ))
             (process-text-1 (replace-regexp-in-string "-\n" "" raw-text))
             (process-text-2 (replace-regexp-in-string "\n" " " process-text-1)))
        process-text-2)))
  (advice-add 'org-noter-pdf--get-selected-text :override #'kb/org-noter-pdf--get-selected-text))

;;;; Videos

;;;;; Custom MPV notes
(with-eval-after-load 'org
  (require 'mpv)

  (defun kb/mpv-play ()
    "Call `mpv-start'.
Prompts for file in `org-attach-directory' if existent. Otherwise,
prompts for file in `default-directory'.

Behaves specially in dired buffers. In those cases case, marked files
will be played as a playlist as chronologically displayed in the dired
buffer. If no files are marked, just the file at point is played. (It is
useful to `dired-sort-toggle-or-edit' to control the ordering of files.
To reverse the order, pass the \"-r\" flag to the listing switches, done
by calling `dired-sort-toggle-or-edit' with `C-u'.)"
    (interactive)
    (let* ((dir (if (org-attach-dir)
                    (file-name-as-directory (org-attach-dir))
                  default-directory))
           (files
            (or (cl-remove-if
                 (lambda (f)
                   "Ensure F is not a directory and is a video file."
                   (not (and (not (directory-name-p f))
                             (member (file-name-extension f)
                                     ;; OPTIMIZE 2024-03-31: I hard-code this,
                                     ;; but I don't know if there's a better way
                                     ;; to recognize video extensions
                                     (list "mp4" "avi" "mkv" "mov" "wmv" "flv" "webm" "mpeg" "m4v" "3gp")))))
                 ;; NOTE 2024-03-31: If no files are marked, the file at point
                 ;; is treated as marked
                 (dired-get-marked-files))
                (expand-file-name (read-file-name "Select video: " dir)))))
      (apply #'mpv-start files)))

  (defun kb/mpv-jump-to-playback-position (time)
    "Prompt user for a time to jump to.
Takes HH:MM:SS time format. Uses `org-timer-hms-to-secs' to parse user
input."
    (interactive "MJump to time (HH:MM:SS format): ")
    (let ((secs (org-timer-hms-to-secs time)))
      (mpv-seek secs)))

  ;; Keymap
  (defvar-keymap kb/mpv-map
    :doc "Keymap for my mpv.el commands for use in `org-mode'.
Commands that control MPV playback mimic MPV keybinds."
    :repeat (mpv-pause mpv-seek-backward mpv-seek-forward)
    "o" #'kb/mpv-play
    "O" #'mpv-play-url
    "k" #'mpv-kill
    "p" #'mpv-pause
    "b" #'mpv-seek-backward
    "f" #'mpv-seek-forward
    "g" #'kb/mpv-jump-to-playback-position
    "9" #'mpv-volume-decrease
    "0" #'mpv-volume-increase
    "[" #'mpv-speed-decrease
    "]" #'mpv-speed-increase
    "P" #'mpv-jump-to-playlist-entry
    "i" #'mpv-insert-playback-position)
  (define-key global-map (kbd "C-M-s-m") kb/mpv-map)

  ;; Taken from https://github.com/kljohann/mpv.el/wiki
  (defun kb/mpv-org-metareturn-insert-playback-position ()
    "When on an `org-timer' formatted list, insert playback position."
    (when-let ((item-beg (org-in-item-p)))
      (when (and (not (bound-and-true-p org-timer-start-time))
                 (mpv-live-p)
                 (save-excursion
                   (goto-char item-beg)
                   (and (not (org-invisible-p)) (org-at-item-timer-p))))
        (mpv-insert-playback-position t))))
  (add-hook 'org-metareturn-hook #'kb/mpv-org-metareturn-insert-playback-position)

  ;; Go to timestamps with `org-open-at-point'
  (add-hook 'org-open-at-point-functions 'mpv-seek-to-position-at-point))

;;;;; Ytdl
(use-package ytdl
  :ensure-system-package (yt-dlp)
  :custom
  (ytdl-command "yt-dlp")
  (ytdl-always-query-default-filename 'yes-confirm)
  (ytdl-music-folder (expand-file-name "~/Music/"))
  (ytdl-video-folder (expand-file-name "~/Videos/"))
  (ytdl-download-types
   `(("Downloads" "d" ytdl-download-folder ytdl-download-extra-args)
     ("Music"  "m" ytdl-music-folder ytdl-music-extra-args)
     ("Videos" "v"  ytdl-video-folder ytdl-video-extra-args)
     ("Temp" "t" ,(expand-file-name "/tmp/") ("-S" "res:720,fps"))))
  :config
  (with-eval-after-load 'org
    ;; Custom `org-attach' integration
    (defun kb/ytdl-org-attach (url)
      "Download and video from URL and attach it to `org-attach-dir'.
A modified version of `ytdl-download'."
      (interactive "MProvide URL: ")
      (when (ytdl--youtube-dl-missing-p)
        (error "youtube-dl is not installed."))
      (let* ((dir (or (org-attach-dir) (org-attach-dir-get-create)))
             (destination (expand-file-name (ytdl--get-filename dir url) dir))
             (extra-ytdl-args '("--write-auto-sub" "--write-sub" "--sub-lang" "en" "--convert-subs" "srt" ; Create .srt file
                                ;; Set maximum resolution and file type
                                "-S" "res:720,fps,ext:mp4:m4a"
                                "--recode" "mp4"))
             (dl-type-name "Org-attach"))
        (ytdl--download-async url
                              destination
                              extra-ytdl-args
                              nil
                              dl-type-name)))

    (add-to-list 'org-attach-commands
                 '((?Y ?\C-Y) kb/ytdl-org-attach
                   "Provide a URL and have \"ytdl\" download the corresponding video and attach that file.")
                 t)))

;;;; Delve
(use-package delve
  :disabled t                           ; Don't use
  :ensure (delve :type git :host github :repo "publicimageltd/delve")
  :hook
  (delve-mode . delve-compact-view-mode)
  ;; FIXME 2022-05-27: `delve--zettel-preview' seems necessary to prevent cmacro
  ;; compiler error for `kb/delve--key--toggle-preview'.
  :commands delve delve--zettel-preview
  :general
  ("<f6>" 'delve)
  (:keymaps 'delve-mode-map
            "<backtab>" 'kb/delve--key-backtab
            )
  (:keymaps 'delve-mode-map
            :states 'visual
            "d" 'delve--key--multi-delete
            "DEL" 'delve--key--multi-delete
            "y" 'kb/delve-visual-copy-nodes
            )
  (:keymaps 'delve-mode-map
            :states 'normal
            "DEL" 'delve--key--multi-delete
            "q" 'bury-buffer
            "t" 'delve--key--insert-tagged
            "T" 'delve--key--insert-node-by-tags
            "dd" '(lambda ()
                    (interactive)
                    (kill-line)
                    (forward-line -1))
            "P" 'delve--key--yank       ; Paste above
            "p" '(lambda ()                  ; Paste below
                   (interactive)
                   (save-excursion
                     (next-line)
                     (delve--key--yank))
                   (next-line))
            "r" 'delve--key--sync
            "gr" 'delve--key--sync
            "h" 'delve--key--insert-heading
            "n" 'delve--node-transient-key
            "s" 'delve--key--sort
            "C-p" 'delve--key--collect-into-pile
            "I" 'delve--key--insert-query-or-pile
            "v" 'delve-compact-view-mode
            "f" 'delve--key--fromlinks
            "b" 'delve--key--backlinks
            "RET" 'kb/delve--key--toggle-preview
            "C-o" 'delve--key--open-zettel
            "o" 'delve--key--open-zettel
            "go" 'delve--key--open-zettel
            "+" 'delve--key--add-tags
            "-" 'delve--key--remove-tags
            "M-d" 'kb/delve-mark-duplicates
            "Y" 'kb/delve-copy-zettel-title
            "yy" 'evil-yank-line
            )
  :custom
  (delve-storage-paths (concat krisb-notes-directory "delve-storage/"))
  (delve-dashboard-tags '("working"))
  :init
  ;; Must be loaded before delve
  (setq delve-minor-mode-prefix-key (kbd "M-n"))
  :config
  (delve-global-minor-mode)

  ;; My own functions below
  (lister-defkey kb/delve--key--toggle-preview (ewoc pos prefix node)
                 "Toggle the display of the preview of ZETTEL."
                 (let ((zettel (delve--current-item-or-error 'delve--zettel ewoc pos)))
                   ;; Unhide the sublist first
                   (lister-with-sublist-below ewoc pos beg end
                                              (lister--outline-hide-show ewoc beg end nil))
                   ;; Then show preview
                   (let ((preview (and (not (delve--zettel-preview zettel))
                                       (or (delve--get-preview-contents zettel)
                                           "No preview available"))))
                     (setf (delve--zettel-preview zettel) preview)
                     (lister-refresh-at lister-local-ewoc :point))
                   ))

  (defvar kb/delve-cycle-global-contents t
    "Are all the contents of the given delve buffer (EWOC)
        shown or hidden? nil if hidden, t if shown.")
  (defun kb/delve--key-backtab (ewoc &optional prefix)
    "In EWOC, toggle hiding or showing all sublists.

When called with PREFIX, hide all previews."
    (interactive (list lister-local-ewoc current-prefix-arg))
    (lister-walk-nodes ewoc
                       #'(lambda (ewoc new-node)
                           (let* ((current-item (delve--current-item nil ewoc new-node))
                                  (is-zettel (cl-typep current-item 'delve--zettel))
                                  (has-preview (when is-zettel (delve--zettel-preview current-item)))
                                  )
                             ;; Hide only when...
                             (when (and (not (lister-sublist-below-p ewoc new-node)) ; Non-parents
                                        (not has-preview) ; No preview
                                        (< 0 (lister-node-get-level new-node)) ; Not top-level
                                        )
                               (lister--outline-hide-show ewoc new-node new-node kb/delve-cycle-global-contents))
                             ;; Then hide if with prefix, hide all previews
                             (when (and prefix has-preview)
                               (setf (delve--zettel-preview current-item) nil)
                               (lister-refresh-at ewoc new-node)) ; Refresh to update visually
                             ))
                       :first :last)
    (if (lister--outline-invisible-p ewoc :point) ; End a non-invisible node
        (lister-goto ewoc (lister-parent-node ewoc :point)))
    (setq-local kb/delve-cycle-global-contents (not kb/delve-cycle-global-contents)))

  (lister-defkey kb/delve-mark-duplicates (ewoc pos prefix node)
                 "Mark duplicate org-roam nodes in the current delve buffer."
                 (let ((id-tracker (list)))
                   (lister-save-current-node ewoc
                                             (lister-walk-nodes ewoc
                                                                #'(lambda (ewoc new-node)
                                                                    (let ((id (delve--zettel-id (delve--current-item nil ewoc new-node))))
                                                                      (if (member id id-tracker)
                                                                          ;; (lister-delete-at ewoc new-node)
                                                                          (lister-mode-mark ewoc new-node)
                                                                        (setq id-tracker (append id-tracker (list id))))))
                                                                :first :last
                                                                #'(lambda (new-node) (cl-typep (delve--current-item nil ewoc new-node) 'delve--zettel))))))
  (lister-defkey kb/delve-visual-copy-nodes (ewoc pos prefix node)
                 "Copy current node(s) when region is active."
                 (when (region-active-p)
                   (copy-region-as-kill (mark) (point) 'region)))
  (lister-defkey kb/delve-copy-zettel-title (ewoc pos prefix node)
                 "Copy current org-roam node's (delve--zettel) title."
                 (let* ((item (delve--current-item-or-error 'delve--zettel ewoc node))
                        (title (delve--zettel-title item))
                        (file (delve--zettel-file item)))
                   (kill-new title)
                   (message (format "The node title \"%s\" from %s has been copied" title file)))))

;;;; Lister
;; Interactive list library for `delve'
(use-package lister
  :disabled
  :general
  (:keymaps 'lister-mode-map
            "M-k" 'kb/lister-mode-up
            "M-j" 'kb/lister-mode-down
            "M-h" 'lister-mode-left
            "M-l" 'kb/lister-mode-right
            ;; Use the initial versions of the functions for these
            "M-K" '(lambda ()
                     (interactive) ; Ignore constraint of same indentation level
                     (funcall-interactively 'lister-mode-up lister-local-ewoc :point '(4)))
            "M-J" '(lambda ()
                     (interactive) ; Ignore constraint of same indentation level
                     (funcall-interactively 'lister-mode-down lister-local-ewoc :point '(4)))
            "M-H" 'kb/lister-mode-left-sublist
            "M-L" 'kb/lister-mode-right-sublist
            "gk" 'lister-mode-forward-same-level
            "gj" 'lister-mode-backward-same-level
            "zu" 'lister-mode-up-parent
            "gh" 'lister-mode-up-parent
            )
  (:keymaps 'lister-mode-map
            :states '(normal visual)
            "m" 'lister-mode-mark
            "u" 'lister-mode-unmark
            "U" 'lister-mode-unmark-all
            )
  :config
  (require 'lister-mode) ; Require since this proves the "core" definitions for the functions below

  ;; Helpers: movement vertically
  (defun kb/lister-move-item-up (ewoc pos)
    "Move item and its sublist one up, preserving `org-mode'-like indentation."
    (let* ((move-fn 'lister--prev-visible-node)
           (from-node (lister--parse-position ewoc pos)) ; Current node
           ;; Prefer `lister-sublist-below-p' over `lister-sublist-at-p' because
           ;; the latter, for some reason, does not consider the first and last
           ;; nodes to be a part of sublists...
           (to-node (if (lister-sublist-below-p ewoc from-node)
                        (cadr (lister--locate-sublist ewoc (ewoc-next ewoc from-node)))
                      from-node))
           (move-to-node (lister--next-node-same-level ewoc from-node move-fn)))
      (unless move-to-node
        (error "No movement possible"))
      ;; Move the range of nodes starting from the current node (from-node) and
      ;; ending with to-node to move-to-node's position. If from-node does not
      ;; have a sublist, then to-node will be from-node. If it does, then
      ;; to-node will be the last node in the sublist.
      (lister--move-list ewoc from-node to-node move-to-node nil)
      ))
  (defun kb/lister-move-item-down (ewoc pos &optional ignore-level)
    "Move item and its sublist one down, preserving `org-mode'-like indentation."
    (let* ((move-fn 'lister--next-visible-node)
           (current-node (lister--parse-position ewoc pos))
           (target-node (lister--next-node-same-level ewoc current-node move-fn)))
      (unless target-node
        (error "No movement possible"))
      ;; Move the next valid node (target-node), which takes its sublist if it
      ;; exists (see `kb/lister-move-item-up'), above the current-node.
      (kb/lister-move-item-up ewoc target-node)
      ))

  ;; Helpers: movement horizontally
  (defun kb/lister-move-item-right (ewoc pos node)
    "In EWOC, increase indentation level of the item at POS.

But don't indent if indenting breaks the structure of the tree."
    (let ((indentation-current (lister-get-level-at ewoc pos))
          (first-node (lister--parse-position ewoc :first)))
      ;; Don't indent if it's the first node
      (unless (equal node first-node)
        (lister-set-level-at ewoc pos (1+ indentation-current)))
      ))

  ;; New keybinds
  (lister-defkey kb/lister-mode-up (ewoc pos prefix node)
                 "Move the item at point one up, preserving `org-mode'-like tree
structure."
                 (kb/lister-move-item-up ewoc pos))
  (lister-defkey kb/lister-mode-down (ewoc pos prefix node)
                 "Move the item at point one down, preserving `org-mode'-like tree
structure."
                 (kb/lister-move-item-down ewoc pos prefix))
  (lister-defkey kb/lister-mode-right (ewoc pos prefix node)
                 "Move the item at point to the right, preserving `org-mode'-like
tree structure."
                 (kb/lister-move-item-right ewoc pos node))
  (lister-defkey kb/lister-mode-left-sublist (ewoc pos prefix node)
                 "Move the node at point and its sublist, if any, to the left."
                 (let ((indentation-current (lister-node-get-level node)))
                   (if (and (< 0 indentation-current) (lister-sublist-below-p ewoc node))
                       (progn
                         (lister-set-node-level ewoc node (1- indentation-current))
                         (lister-move-sublist-left ewoc (ewoc-next ewoc node)))
                     (lister-move-item-left ewoc pos))))
  (lister-defkey kb/lister-mode-right-sublist (ewoc pos prefix node)
                 "Move the node at point and its sublist, if any, to the right."
                 (if (lister-sublist-below-p ewoc node)
                     (progn
                       (lister-move-sublist-right ewoc (ewoc-next ewoc node))
                       ;; Move sublist before current node because the current node becomes
                       ;; part of the sublist if indented first
                       (lister-set-node-level ewoc node (1+ (lister-node-get-level node))))
                   (kb/lister-move-item-right ewoc pos node))))

(provide 'org-notes-other-rcp)
;;; org-notes-other-rcp.el ends here
