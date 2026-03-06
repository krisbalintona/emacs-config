;;; svelte-publish.el --- Tab bookmarks -*- lexical-binding: t -*-

;; Copyright (C) 2026 Kristoffer Balintona

;; Author: Kristoffer Balintona
;; Created: 2026

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bespoke org-publish setup for exporting posts to HTML that is
;; ingested by SvelteKit.

;;; Code:

(require 'ox-publish)
(require 'esxml)

;; NOTE 2026-02-12: This is set to nil as I develop.  When in use, a
;; value of t is more appropriate.
(setopt org-publish-use-timestamps-flag nil)

;; Define project
(setopt org-publish-project-alist
        `(("posts"
           :base-directory ,krisb-blog-manuscripts-directory
           :publishing-directory "/tmp/new_blog/posts/"
           :base-extension "org"
           :recursive t
           :publishing-function krisb-org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "/tmp/new_blog/index.org"
           :sitemap-title "My Posts"
           :sitemap-sort-files anti-chronologically
           :sitemap-format-entry krisb-org-publish-sitemap-format-entry
           :html-head-include-default-style nil
           :html-head ,(sxml-to-xml `(link (@ (rel "stylesheet")
                                              (href "../css/stylesheet.css")
                                              (type "text/css"))))
           :html-prefer-user-labels nil ; We have our own function for anchors
           :org-export-with-broken-links t
           :with-toc nil
           :with-cite-processors t)
          ("css"
           :base-extension "css"
           :base-directory ,(expand-file-name "css" krisb-blog-manuscripts-directory)
           :publishing-directory "/tmp/new_blog/css/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("blog" :components ("css" "posts"))))

;;; Provide
(provide 'svelte-publish)
;;; svelte-publish.el ends here
