;;; personal-site.el --- Personal site org exporter -*- lexical-binding: t -*-

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

;; Bespoke org backend and org-publish setup for exporting posts to my
;; personal site (found at https://kristofferbalintona.me/).

;; TODO:
;; 1. Figure out exporting of assets to assets post subdirectory.
;; 3. Ensure cross-post org id links work.
;; 5. Improve formatting of footnotes.
;; 6. Improve formatting of bibliographies.

;;; Code:

(require 'ox)
(require 'ox-html)
(require 'ox-publish)
(require 'esxml)
(require 'el-patch)

;;;; Variables and options

(defcustom personal-site-root-dir "~/Documents/svelte-blog/"
  "Directory of blog project.
This option is used to define the value of other relevant paths."
  :type 'directory
  :group 'personal-site)

(defcustom personal-site-destination-dir
  (expand-file-name "src/lib/posts/" personal-site-root-dir)
  "Directory where posts will be exported to."
  :type 'directory
  :group 'personal-site)

;;;; Org backend
;; Define a bespoke org export backend. For a detailed description of
;; the export process, see (info "(org) Advanced Export
;; Configuration")

(defconst personal-site-org-post-id-timestamp-format "%Y%m%d%H%M"
  "Timestamp format for post IDs.
For my site, I construct post IDs using the date of the post.")

;; TODO 2026-03-28: When considering export of subtrees, in which case
;; we would use the EXPORT_DATE subtree property
(defun personal-site--org-get-date (info fmt)
  "Return current post\\='s publish date as a string.
The date is derived with this precedence:
1. The value of the file\\='s last \"#+date\" keyword.  This date is
   formatted using the time format string FMT.

If none of the above apply, return nil.

INFO is a plist used as a communication channel."
  ;; FIXME 2026-03-28: Should I even use `org-export-get-date'?  It
  ;; seems like the only built-in option, but it doesn't really do
  ;; what I want
  (let ((date (org-export-get-date info fmt)))
    (when (listp date)
      (setq date (org-format-timestamp (car (last date)) fmt)))
    (org-string-nw-p date)))

(defun personal-site-org-html--meta-info (info)
  "Return meta tags for exported document.
These meta tags store information about the post that will be used later
by SvelteKit when ingesting the document HTML.

INFO is a plist used as a communication channel."
  (let* ((title (org-no-properties
                 (org-html-plain-text
                  (org-element-interpret-data (plist-get info :title)) info)))
         ;; Set title to an invisible character instead of leaving it
         ;; empty, which is invalid.
         (title (if (org-string-nw-p title) title "&lrm;"))
         (charset (or (and org-html-coding-system
                           (symbol-name
                            (coding-system-get org-html-coding-system 'mime-charset)))
                      "iso-8859-1")))
    (string-trim
     (mapconcat
      #'esxml-to-xml
      (list
       ;; Timestamp for creation of export file
       (when (plist-get info :time-stamp-file)
         `(comment nil ,(format-time-string (plist-get info :html-metadata-timestamp-format))))
       
       ;; Title
       `(meta ((name . "title") (content . ,title)))

       ;; Date
       ;;
       ;; We have a meta tag specifically for the date so SvelteKit can
       ;; show the same date in different formats, if needed
       (let ((date (personal-site--org-get-date info (plist-get info :html-metadata-timestamp-format))))
         `(meta ((name . "date") (content . ,date))))

       ;; Character encoding
       (if (org-html-html5-p info)
           `(meta ((charset . ,charset)))
         `(meta ((http-equiv . "Content-Type")
                 (content . ,(concat "text/html;charset=" charset)))))

       (let* (;; We construct each post's ID using their ISO 8601 with
              ;; all non-numeric characters removed
              (postid (personal-site--org-get-date info personal-site-org-post-id-timestamp-format)))
         ;; Post ID
         `(meta ((name . "postid") (content . ,postid)))))
      "\n"))))

(defun personal-site-org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist holding
export options."
  (concat
   ;; Metadata for post (stored as meta tags prior to body tag)
   (personal-site-org-html--meta-info info)

   ;; Document body (body tag)
   (let* ((title (and (plist-get info :with-title)
                      (plist-get info :title)))
          (title-element (when title
                           `(h1 ((class . "title")) ,(org-export-data title info))))
          (subtitle (plist-get info :subtitle))
          (subtitle-element (when subtitle
                              `(p ((class . "subtitle") (role . "doc-subtitle"))
                                  ,(org-export-data subtitle info))))

          (date (when (plist-get info :date)
                  (personal-site--org-get-date info "%B %e, %Y")))
          (date-element (when date
                          `(i ((class . "date")) ,(org-export-data date info))))
          (html5-fancy (org-html--html5-fancy-p info))
          (content (assq 'content (plist-get info :html-divs)))
          (content-tag (intern (nth 1 content)))
          (content-id (nth 2 content))
          (content-class (plist-get info :html-content-class)))
     (esxml-to-xml
      `(,content-tag ((id . ,content-id) (class . ,content-class))
                     (,(if html5-fancy 'header 'div) nil
                      ,date-element
                      ,@(when title
                          (list title-element
                                subtitle-element)))
                     (raw-string ,contents))))))

(defun personal-site--headline-text-to-slug (headline)
  "Slugify HEADLINE\\'s text.
Return a slug for the HEADLINE element suitable for use as a URL anchor.
This function converts the text of HEADLINE to lowercase, replaces
spaces with hyphens, and removes any characters that are not
alphanumeric or hyphens.

In the special case where HEADLINE\\'s text is an empty string, return
\"headline\".

HEADLINE is an org headline element.  INFO is the current export state,
as a plist."
  (let* ((title (org-element-property :title headline))
         (raw (org-no-properties (org-element-interpret-data title)))
         (lowercased (downcase raw))
         (trimmed (string-trim lowercased))
         (hyphenated (replace-regexp-in-string "[[:space:]]+" "-" trimmed))
         (cleaned (replace-regexp-in-string "[^a-z0-9-]" "" hyphenated))
         (deduplicated (replace-regexp-in-string "-+" "-" cleaned))
         (result (string-trim deduplicated "-")))
    (if (string-empty-p result) "headline" result)))

(defun personal-site--org-export-get-reference-advice (orig datum info)
  "Advice for `org-export-get-reference' to generate stable IDs.
When exporting with the 'html-svelte' backend:
- For headlines, generate a deterministic slug from the headline text,
  appending a counter only for duplicates.
- For other elements, generate a reference of the form TYPE-N.
When not using the 'html-svelte' backend, call
`org-export-get-reference' normally.

ORIG is the original function.  See `org-export-get-reference' for a
description of DATUM and INFO."
  (if (not (eq (org-export-backend-name (plist-get info :back-end)) 'html-svelte))
      (funcall orig datum info)
    ;; Keep a cache in INFO like `org-export-get-reference' does
    (let ((cache (plist-get info :internal-references)))
      (or (car (rassq datum cache))
          (let* ((type (org-element-type datum))
                 (reference
                  (if (org-element-type-p datum 'headline)
                      ;; For headlines: SLUG-N counter after the first
                      ;; duplicate headline
                      (let* ((slug-cache
                              ;; Table of DATUM to slug
                              (or (plist-get info :personal-site-headline-slug-cache)
                                  (let ((table (make-hash-table :test #'eq)))
                                    (plist-put info :personal-site-headline-slug-cache table)
                                    table)))
                             (cached (gethash datum slug-cache)))
                        (or
                         ;; If DATUM has a matching reference already,
                         ;; just return it
                         cached
                         ;; New headline element, so compute new slug
                         ;; then cache the relevant info.  We also
                         ;; have to have another cache for the number
                         ;; of times a given base slug appears, so as
                         ;; to increment appropriately
                         (let* ((base-slug (personal-site--headline-text-to-slug datum))
                                (slug-counts
                                 ;; Table of BASE-SLUG to count
                                 (or (plist-get info :personal-site-headline-slug-counts)
                                     (let ((table (make-hash-table :test #'equal)))
                                       (plist-put info :personal-site-headline-slug-counts table)
                                       table)))
                                (n (gethash base-slug slug-counts 0))
                                (final-slug (if (zerop n)
                                                base-slug
                                              (format "%s-%d" base-slug n))))
                           (puthash base-slug (1+ n) slug-counts)
                           (puthash datum final-slug slug-cache)
                           final-slug)))
                    ;; For non-headlines: simple TYPE-N counter.
                    (let* ((counters
                            (or (plist-get info :personal-site-display-type-counters)
                                (let ((table (make-hash-table :test #'eq)))
                                  (plist-put info :personal-site-display-type-counters table)
                                  table)))
                           (display-type
                            (cond ((and (eq type 'paragraph)
                                        (org-html-standalone-image-p datum info))
                                   'figure)
                                  (t type)))
                           (n (1+ (gethash display-type counters 0))))
                      (puthash display-type n counters)
                      (format "%s-%d" display-type n))))
                 (cache (cons (cons reference datum) cache)))
            (plist-put info :internal-references cache)
            reference)))))
;; We use advice rather than redefining the functions
;; `org-export-get-reference' uses because many functions use it for
;; various DATUMs, e.g., figures, headlines, src blocks -- it's easier
;; to just advise it
(advice-add 'org-export-get-reference :around #'personal-site--org-export-get-reference-advice)

(el-patch-defun (el-patch-swap org-html-src-block personal-site-org-html-src-block)
  (src-block _contents info)
  (el-patch-swap
    "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
    "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.

This is identical to `org-html-format-code' except in the following
ways:
- The src block languages are converted into the appropriate Shiki
  language grammar name (see https://shiki.style/languages).")
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
           ;; Convert LANG to proper Shiki language grammar name
           (el-patch-add
             (lang (pcase lang
                     ("LaTeX" "latex")
                     (_ lang))))
           (code (org-html-format-code src-block info))
           (label (let ((lbl (org-html--reference src-block info t)))
                    (if lbl (format " id=\"%s\"" lbl) "")))
           (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
                                           "ruby" "scheme" "clojure" "php" "html")))))
      (format "<div class=\"org-src-container\">\n%s%s\n</div>"
              ;; Build caption.
              (let ((caption (org-export-get-caption src-block)))
                (if (not caption) ""
                  (let ((listing-number
                         (format
                          "<span class=\"listing-number\">%s </span>"
                          (format
                           (org-html--translate "Listing %d:" info)
                           (org-export-get-ordinal
                            src-block info nil #'org-html--has-caption-p)))))
                    (format "<label class=\"org-src-name\">%s%s</label>"
                            listing-number
                            (org-trim (org-export-data caption info))))))
              ;; Contents.
              (if klipsify
                  (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
                          lang ; lang being nil is OK.
                          label
                          (if (string= lang "html")
                              " data-editor-type=\"html\""
                            "")
                          code)
                (format "<pre class=\"src src-%s\"%s><code>%s</code></pre>"
                        ;; Lang being nil is OK.
                        lang label code))))))

;;;;; Exporters

(defun personal-site--title-to-slug (title)
  "Return slug associated with TITLE.
This slug is used as the directory name associated with a post (inside
`personal-site-destination-dir')."
  (downcase
   (replace-regexp-in-string
    "-+" "-"
    (replace-regexp-in-string
     "[^a-z0-9]+" "_"
     (downcase title)))))

(defun personal-site-org-output-directory (&optional subtreep)
  "Return the output directory path of the current post.
The returned path is of the form ID-SLUG, where ID is based on the date
property of the post and SLUG is the title of the post passed to
`personal-site--title-to-slug'.

This function is called from the point in org buffer to-be exported.

With a non-nil optional argument SUBTREEP, use the \"EXPORT_FILE_NAME\"
property of subtree at point as the SLUG portion of the output
directory."
  (let* ((export-environment (org-export-get-environment 'html-svelte))
         (title (org-element-interpret-data (plist-get export-environment :title)))
         (date-timestamp (car (plist-get export-environment :date)))
         (postid (org-format-timestamp date-timestamp personal-site-org-post-id-timestamp-format))
         (directory
          (or
           ;; Check EXPORT_FILE_NAME subtree property (when SUBTREEP
           ;; is non-nil)
           (when subtreep (org-entry-get nil "EXPORT_FILE_NAME" 'selective))
           ;; REVIEW 2026-03-27: Should I do this? I'm going to
           ;; standardize the file names anyway...
           ;;
           ;; Check #+EXPORT_FILE_NAME keyword
           (org-with-point-at (point-min)
             (catch :found
               (let ((case-fold-search t))
                 (while (re-search-forward "^[ \t]*#\\+EXPORT_FILE_NAME:[ \t]+\\S-" nil t)
                   (let ((element (org-element-at-point)))
                     (when (org-element-type-p element 'keyword)
                       (throw :found (org-element-property :value element))))))))
           ;; Determine export file path for buffer
           (concat postid "--" (personal-site--title-to-slug title))
           ;; As a fallback, ask user
           (read-file-name "Output directory: " personal-site-destination-dir))))
    (expand-file-name directory personal-site-destination-dir)))

(defun personal-site--org-prepare-output-directory (directory)
  "Prepare output directory for receiving exported files.
Create DIRECTORY if it does not exist.  If it does, delete any existing
files and directories.

DIRECTORY is the targeted output directory."
  (if (file-exists-p directory)
      ;; Clear out DIRECTORY if it already exists
      (mapc (lambda (f)
              (if (file-directory-p f)
                  (delete-directory f)
                (delete-file f)))
            (directory-files-recursively directory ".*" t))
    ;; Make DIRECTORY if it doesn't exist yet
    (make-directory output-directory)))

(defun personal-site-org-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.
Behaves identically to `org-html-export-as-html' but with the
\\='html-svelte backend.

If narrowing is active in the current buffer, only export its narrowed
part.

If a region is active, export that region.

See the docstring of `org-html-export-as-html' for a description of the
ASYNC, SUBTREEP, VISIBLE-ONLY, and BODY-ONLY arguments.

EXT-PLIST, when provided, is a property list with external parameters
overriding Org default settings, but still inferior to file-local
settings.

Export is done in a buffer named \"*Org HTML Export*\", which will be
displayed when `org-export-show-temporary-export-buffer' is non-nil."
  (interactive)
  (let (;; Force exporting raw HTML without styling
        (org-html-htmlize-output-type nil))
    (org-export-to-buffer 'html-svelte "*Org HTML Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (set-auto-mode t)))))

(defun personal-site-org-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.
Behaves identically to `org-html-export-to-html', but with a bespoke
export file destination (see `personal-site-destination-dir') and using
the \\='html-svelte backend..

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

See the docstring of `org-html-export-to-html' for a description of the
ASYNC, SUBTREEP, VISIBLE-ONLY, and BODY-ONLY arguments.

EXT-PLIST, when provided, is a property list with external parameters
overriding Org default settings, but still inferior to file-local
settings.

Return output file's name."
  (interactive)
  (let* ((org-export-coding-system org-html-coding-system)
         (output-dir (personal-site-org-output-directory subtreep))
         (file (expand-file-name "index.html" output-dir))
         ;; Force exporting raw HTML without styling
         (org-html-htmlize-output-type nil))
    (personal-site--org-prepare-output-directory output-dir)
    (org-export-to-file 'html-svelte file
      async subtreep visible-only body-only ext-plist)))

(org-export-define-derived-backend 'html-svelte 'html
  ;; Used to modify or remove org elements on export, overwriting the
  ;; filters of the parent backend.  See (info "(org) Advanced Export
  ;; Configuration") for more information on backend filters
  ;; :filters-alist

  ;; Entry for backend in the org export menu
  :menu-entry
  '(?p "Export to personal site HTML"
       ((?H "As HTML buffer" personal-site-org-export-as-html)
        (?h "As HTML file" personal-site-org-export-to-html)
        (?o "As HTML file and open with Emacs"
            (lambda (async subtreep visible-only body-only)
              (if async
                  (personal-site-org-export-to-html t subtreep visible-only body-only)
                (find-file-other-window
                 (personal-site-org-export-to-html nil subtreep visible-only body-only)))))))

  ;; Used to define new options or overwrite those of the parent
  ;; backend
  :options-alist
  '((:time-stamp-file nil nil nil)
    (:html-metadata-timestamp-format nil nil "%Y-%m-%dT%H:%M:%S")
    ;; Default date set to Unix Epoch (for documents without a date)
    (:date "DATE" nil "[1970-01-01 Thu 00:00]" parse)
    ;; Force using the modern HTML5
    (:html-doctype "HTML_DOCTYPE" nil "html5")
    (:html-html5-fancy nil "html5-fancy" t))

  ;; Used to add new transcoders or overwrite those of the parent
  ;; backend.  See `org-export-define-backend' for more information on
  ;; backend transcoders
  :translate-alist
  '((template . personal-site-org-html-template)
    (src-block . personal-site-org-html-src-block)))

;;;; Org-publish
;; I use org-publish to make it easier to export all my blog posts
;; altogether.

(defun personal-site-org-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML for the personal site.
PLIST is the property list for the current project.  FILENAME is the
filename of the org file to be published.  PUB-DIR is the publishing
directory.

This function is used as the :publishing-function in
`org-publish-project-alist'."
  ;; We can't use `org-publish-org-to' directly because that would use
  ;; `org-export-output-file-name' instead of our
  ;; `personal-site-org-output-directory' to determine the output
  ;; file.  So we define a modified version of `org-publish-org-to'
  ;; instead
  (org-with-file-buffer filename
    (let* ((org-export-coding-system org-html-coding-system))
      (personal-site-org-export-to-html
       nil nil nil (plist-get plist :body-only)
       (org-combine-plists
        plist
        `( :crossrefs ,(org-publish-cache-get-file-property
                        (file-truename filename) :crossrefs nil t)
           :filter-final-output (org-publish--store-crossrefs
                                 org-publish-collect-index
                                 ,@(plist-get plist :filter-final-output))))))))

(setopt
 ;; NOTE 2026-02-12: This is set to nil as I develop, to force
 ;; publishing every file.  When in use, a value of t is more
 ;; appropriate.
 org-publish-use-timestamps-flag nil
 org-publish-project-alist
 `(("personal-site-posts"
    :base-directory ,(expand-file-name "posts" krisb-blog-manuscripts-directory)
    :publishing-directory ,personal-site-destination-dir
    :base-extension "org"
    :recursive t
    :publishing-function personal-site-org-publish-to-html
    :html-head-include-default-style nil
    :html-prefer-user-labels nil
    :with-toc nil
    :with-tags nil
    :with-todo-keywords nil
    :time-stamp-file nil)))

;;; Provide
(provide 'personal-site)
;;; personal-site.el ends here
