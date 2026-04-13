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
;; 3. Ensure cross-post org id links work.
;; 5. Improve formatting of footnotes.
;; 6. Improve formatting of bibliographies.

;;; Code:
(require 'ox)
(require 'ox-html)
(require 'ox-publish)
(require 'esxml)
(require 'json)
(require 'el-patch)

;;;; Variables and options

(defcustom personal-site-root-dir (expand-file-name "~/Documents/personal-site-astro/")
  "Directory of blog project.
This option is used to define the value of other relevant paths."
  :type 'directory
  :group 'personal-site)

(defcustom personal-site-destination-dir
  (expand-file-name "src/lib/posts/" personal-site-root-dir)
  "Directory where posts will be exported to."
  :type 'directory
  :group 'personal-site)

;;;; Org backends
;; Define two bespoke org export backends: one for the HTML and
;; another for the post metadata.
;;
;; For a detailed description of the export process, see (info "(org)
;; Advanced Export Configuration").

;;;;; HTML (contant) backend
;; This backend outputs HTML that will be the content/body of the post
;; (before a few possible server-side modifications to it).

;; TODO 2026-03-28: When considering export of subtrees, in which case
;; we would use the EXPORT_DATE subtree property
(defun personal-site--org-get-date (info fmt)
  "Return current post's publish date as a string.
The date is derived with this precedence:
1. The value of the file's last \"#+date\" keyword.  This date is
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

(defun personal-site-org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist holding
export options."
  (concat
   ;; Metadata
   ;;
   ;; Timestamp for creation of export file
   (when-let (((plist-get info :time-stamp-file))
              (timestamp-format (plist-get info :personal-site-metadata-timestamp-format)))
     (esxml-to-xml
      `(comment nil ,(format "%s" (concat "Generated on" (format-time-string timestamp-format))))))

   ;; Document content
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
When exporting with the 'personal-site-html' backend:
- For headlines, generate a deterministic slug from the headline text,
  appending a counter only for duplicates.
- For other elements, generate a reference of the form TYPE-N.
When not using the 'personal-site-html' backend, call
`org-export-get-reference' normally.

ORIG is the original function.  See `org-export-get-reference' for a
description of DATUM and INFO."
  (if (not (eq (org-export-backend-name (plist-get info :back-end)) 'personal-site-html))
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

(defun personal-site--attachment-copy (path info)
  "Copy asset at PATH to the post subdirectory.
Return the path of the asset relative to the post subdirectory.

PATH is the absolute path of the local asset.  INFO is a plist used as a
communication channel."
  (let* ((output-dir (plist-get info :personal-site-output-directory))
         (filename (file-name-nondirectory path))
         (asset-subdir (expand-file-name "assets" output-dir))
         (asset-path (expand-file-name filename asset-subdir))
         (link-path filename))
    ;; TODO 2026-04-04: Assumes that attachments are uniquely named.
    ;; Perhaps allow non-unique attachment file names by appending a
    ;; number after each duplicate (like we do for element IDs in
    ;; `personal-site--org-export-get-reference-advice')?
    (if (file-exists-p asset-path)
        (message "[personal-site] Asset %s already exists, not overwriting" asset-path)
      (unless (file-exists-p asset-subdir)
        (make-directory asset-subdir))
      (copy-file path asset-path nil))
    link-path))

(el-patch-defun (el-patch-swap org-html-link personal-org-html-link)
  (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((html-ext (plist-get info :html-extension))
         (dot (when (> (length html-ext) 0) "."))
         (link-org-files-as-html-maybe
          (lambda (raw-path info)
            ;; Treat links to `file.org' as links to `file.html', if
            ;; needed.  See `org-html-link-org-files-as-html'.
            (save-match-data
              (cond
               ((and (plist-get info :html-link-org-files-as-html)
                     (let ((case-fold-search t))
                       (string-match "\\(.+\\)\\.org\\(?:\\.gpg\\)?$" raw-path)))
                (concat (match-string 1 raw-path) dot html-ext))
               (t raw-path)))))
         (type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (org-string-nw-p desc))
         (path
          (cond
           ((string= "file" type)
            ;; During publishing, turn absolute file names belonging
            ;; to base directory into relative file names.  Otherwise,
            ;; append "file" protocol to absolute file name.
            (setq raw-path
                  (org-export-file-uri
                   (org-publish-file-relative-name raw-path info)))
            ;; Possibly append `:html-link-home' to relative file
            ;; name.
            (let ((home (and (plist-get info :html-link-home)
                             (org-trim (plist-get info :html-link-home)))))
              (when (and home
                         (plist-get info :html-link-use-abs-url)
                         (not (file-name-absolute-p raw-path)))
                (setq raw-path (concat (file-name-as-directory home) raw-path))))
            ;; Maybe turn ".org" into ".html".
            (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
            ;; Add search option, if any.  A search option can be
            ;; relative to a custom-id, a headline title, a name or
            ;; a target.
            (let ((option (org-element-property :search-option link)))
              (if (not option) raw-path
                (let ((path (org-element-property :path link)))
                  (concat raw-path
                          "#"
                          (org-publish-resolve-external-link option path t))))))
           (t (url-encode-url (concat type ":" raw-path)))))
         (attributes-plist
          (org-combine-plists
           ;; Extract attributes from parent's paragraph.  HACK: Only
           ;; do this for the first link in parent (inner image link
           ;; for inline images).  This is needed as long as
           ;; attributes cannot be set on a per link basis.
           (let* ((parent (org-element-parent-element link))
                  (link (let ((container (org-element-parent link)))
                          (if (and (org-element-type-p container 'link)
                                   (org-html-inline-image-p link info))
                              container
                            link))))
             (and (eq link (org-element-map parent 'link #'identity info t))
                  (org-export-read-attribute :attr_html parent)))
           ;; Also add attributes from link itself.  Currently, those
           ;; need to be added programmatically before `org-html-link'
           ;; is invoked, for example, by backends building upon HTML
           ;; export.
           (org-export-read-attribute :attr_html link)))
         (attributes
          (let ((attr (org-html--make-attribute-string attributes-plist)))
            (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html info))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
           (org-export-inline-image-p
            link (plist-get info :html-inline-image-rules)))
      (el-patch-remove (org-html--format-image path attributes-plist info))
      (el-patch-add
        (let* ((raw-path (org-element-property :path link))
               ;; When we are exporting to a buffer, we leave the
               ;; links as they are.  However, when we are exporting
               ;; to a file, we copy the asset over to the appropriate
               ;; location and modify the link to point to the
               ;; appropriate path on the website
               (asset-path
                (if (plist-get info :output-file)
                    (personal-site--attachment-copy raw-path info)
                  path)))
          (org-html--format-image asset-path attributes-plist info))))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (if (not destination) desc
          (format "<a href=\"#%s\"%s>%s</a>"
                  (org-html--reference destination info)
                  attributes
                  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (pcase (org-element-type destination)
          ;; ID link points to an external file.
          (`plain-text
           (let ((fragment (concat org-html--id-attr-prefix raw-path))
                 ;; Treat links to ".org" files as ".html", if needed.
                 (path (funcall link-org-files-as-html-maybe
                                destination info)))
             (format "<a href=\"%s#%s\"%s>%s</a>"
                     path fragment attributes (or desc destination))))
          ;; Fuzzy link points nowhere.
          (`nil
           (format "<i>%s</i>"
                   (or desc
                       (org-export-data
                        (org-element-property :raw-link link) info))))
          ;; Link points to a headline.
          (`headline
           (let ((href (org-html--reference destination info))
                 ;; What description to use?
                 (desc
                  ;; Case 1: Headline is numbered and LINK has no
                  ;; description.  Display section number.
                  (if (and (org-export-numbered-headline-p destination info)
                           (not desc))
                      (mapconcat #'number-to-string
                                 (org-export-get-headline-number
                                  destination info) ".")
                    ;; Case 2: Either the headline is un-numbered or
                    ;; LINK has a custom description.  Display LINK's
                    ;; description or headline's title.
                    (or desc
                        (org-export-data
                         (org-element-property :title destination) info)))))
             (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
          ;; Fuzzy link points to a target or an element.
          (_
           (require 'ox-latex)
           (declare-function org-latex--environment-type "ox-latex" (latex-environment))
           (if (and destination
                    (memq (plist-get info :with-latex) '(mathjax t))
                    (org-element-type-p destination 'latex-environment)
                    (eq 'math (org-latex--environment-type destination)))
               ;; Caption and labels are introduced within LaTeX
               ;; environment.  Use "ref" or "eqref" macro, depending on user
               ;; preference to refer to those in the document.
               (format (plist-get info :html-equation-reference-format)
                       (org-html--reference destination info))
             (let* ((ref (org-html--reference destination info))
                    (org-html-standalone-image-predicate
                     #'org-html--has-caption-p)
                    (counter-predicate
                     (if (org-element-type-p destination 'latex-environment)
                         #'org-html--math-environment-p
                       #'org-html--has-caption-p))
                    (number
                     (cond
                      (desc nil)
                      ((org-html-standalone-image-p destination info)
                       (org-export-get-ordinal
                        (org-element-map destination 'link #'identity info t)
                        info '(link) 'org-html-standalone-image-p))
                      (t (org-export-get-ordinal
                          destination info nil counter-predicate))))
                    (desc
                     (cond (desc)
                           ((not number) "No description for this link")
                           ((numberp number) (number-to-string number))
                           (t (mapconcat #'number-to-string number ".")))))
               (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" (org-html-encode-plain-text raw-path))))
        (format "<a href=\"#%s\" %s%s>%s</a>"
                fragment
                (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
                        fragment fragment)
                attributes
                (format (org-export-get-coderef-format raw-path desc)
                        (org-export-resolve-coderef raw-path info)))))
     ;; External link with a description part.
     ((and path desc)
      (format "<a href=\"%s\"%s>%s</a>"
              (org-html-encode-plain-text path)
              attributes
              desc))
     ;; External link without a description part.
     (path
      (let ((path (org-html-encode-plain-text path)))
        (format "<a href=\"%s\"%s>%s</a>" path attributes path)))
     ;; No path, only description.  Try to do something useful.
     (t
      (format "<i>%s</i>" desc)))))

(defun personal-site--fix-empty-anchors (text _backend _info)
  "Replace all empty anchor elements with spans in TEXT.
In Org's export output, empty anchors (i.e., `<a id=\"...\"></a>') are
sometimes emitted, being used merely as targets.  Replacing the 'a' tags
with 'span' tags is semantically cleaner while avoiding violations of
WCAG 2.4.4 and 4.1.2 (Web Content Accessibility Guidelines)."
  (replace-regexp-in-string
   (rx "<a id=\"" (group (one-or-more (not "\""))) "\"></a>")
   "<span id=\"\\1\"></span>"
   text))

(org-export-define-derived-backend 'personal-site-html 'html
  ;; Used to modify or remove org elements on export, overwriting the
  ;; filters of the parent backend.  See (info "(org) Advanced Export
  ;; Configuration") for more information on backend filters
  :filters-alist
  '((:filter-final-output . personal-site--fix-empty-anchors))
  
  ;; Entry for backend in the org export menu
  :menu-entry
  '(?p "Export to personal site"
       ((?H "Post as HTML buffer" personal-site-org-export-as-html)
        (?J "Post metadata as JSON buffer " personal-site-org-export-as-json)
        (?p "Full post to directory" personal-site-org-export-to-site)
        (?o "Full post directory and open the directory"
            (lambda (async subtreep visible-only body-only)
              (if async
                  (personal-site-org-export-to-site t subtreep visible-only body-only)
                (pop-to-buffer
                 (find-file-noselect
                  (personal-site-org-export-to-site nil subtreep visible-only body-only))))))))

  ;; Used to define new options or overwrite those of the parent
  ;; backend
  :options-alist
  '((:time-stamp-file nil nil nil)
    ;; 2026-04-10: I don't think this backend makes use of
    ;; `:html-metadata-timestamp-format' anymore, with what I've kept
    ;; from 'ox-html', but I keep this here anyway, just in case but
    ;; also as a reminder that it exists.
    (:html-metadata-timestamp-format nil nil "%Y-%m-%dT%H:%M:%S")
    ;; Default date set to Unix Epoch (for documents without a date)
    (:date "DATE" nil "[1970-01-01 Thu 00:00]" parse)
    ;; Force using the modern HTML5
    (:html-doctype "HTML_DOCTYPE" nil "html5")
    (:html-html5-fancy nil "html5-fancy" t)
    ;; Personal site-specific options
    (:personal-site-metadata-timestamp-format nil nil "%Y-%m-%dT%H:%M:%S"))

  ;; Used to add new transcoders or overwrite those of the parent
  ;; backend.  See `org-export-define-backend' for more information on
  ;; backend transcoders
  :translate-alist
  '((template . personal-site-org-html-template)
    (src-block . personal-site-org-html-src-block)
    (link . personal-org-html-link)))

;;;;; JSON (metadata) backend
;; This backend outputs JSON that stores metadata about the post: a
;; "metadata.json".  This JSON file is used by Astro for various
;; purposes, such as dynamic routing.

(defun personal-site-org-json-template (contents info)
  "Return post metadata as a JSON string.
CONTENTS is the transcoded contents string.  INFO is a plist holding
export options."
  (let* ((title (org-no-properties
                 (org-html-plain-text
                  (org-element-interpret-data (plist-get info :title)) info)))
         ;; Set title to an invisible character instead of leaving it
         ;; empty, which is invalid.
         (title (if (org-string-nw-p title) title "&lrm;"))
         (slug (personal-site--title-to-slug title))
         (timestamp-format (plist-get info :personal-site-metadata-timestamp-format))
         (date (personal-site--org-get-date info timestamp-format))
         (postid (plist-get info :personal-site-postid)))

    ;; Timestamp for creation of export file
    (when-let (((plist-get info :time-stamp-file))
               (timestamp-format (plist-get info :personal-site-metadata-timestamp-format)))
      (concat "// Generated on " (format-time-string timestamp-format)))
    
    ;; Content (metadata as JSON)
    (json-encode
     `((postid . ,postid)
       (title . ,title)
       (slug . ,slug)
       (date . ,date)))))

(org-export-define-derived-backend 'personal-site-json 'personal-site-html
  :options-alist
  '((:time-stamp-file nil nil nil)
    (:personal-site-metadata-timestamp-format nil nil "%Y-%m-%dT%H:%M:%S"))
  
  :translate-alist
  '((template . personal-site-org-json-template)))

;;;;; Exporters
;; Export (to file, to buffer) functions for the backends defined
;; above.

(defconst personal-site-org-post-id-timestamp-format "%Y%m%d%H%M"
  "Timestamp format for post IDs.
For my site, I construct post IDs using the date of the post.")

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
  (let* ((export-environment (org-export-get-environment 'personal-site-html))
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

(defun personal-site--org-prepare-output-directory (output-directory)
  "Prepare output directory for receiving exported files.
Create OUTPUT-DIRECTORY if it does not exist.  If it does, delete any
existing files and directories.

DIRECTORY is the targeted output directory."
  (if (file-exists-p output-directory)
      ;; Clear out DIRECTORY if it already exists
      (mapc (lambda (f)
              (if (file-directory-p f)
                  (delete-directory f)
                (delete-file f)))
            (directory-files-recursively output-directory ".*" t))
    ;; Make DIRECTORY if it doesn't exist yet
    (make-directory output-directory)))

(defun personal-site-org-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.
Behaves identically to `org-html-export-as-html' but with the
'personal-site-html' backend.

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
  (let ((output-dir (personal-site-org-output-directory subtreep))
        ;; Force exporting raw HTML without styling
        (org-html-htmlize-output-type nil))
    (org-export-to-buffer 'personal-site-html "*Org HTML Export*"
      async subtreep visible-only body-only
      (org-combine-plists
       ext-plist
       `(:personal-site-output-directory ,output-dir))
      #'html-mode)))

(defun personal-site-org-export-as-json
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a JSON buffer.
If narrowing is active in the current buffer, only export its narrowed
part.

If a region is active, export that region.

See the docstring of `org-html-export-as-html' for a description of the
ASYNC, SUBTREEP, VISIBLE-ONLY, and BODY-ONLY arguments.

EXT-PLIST, when provided, is a property list with external parameters
overriding Org default settings, but still inferior to file-local
settings.

Export is done in a buffer named \"*Org JSON Export*\", which will be
displayed when `org-export-show-temporary-export-buffer' is non-nil."
  (interactive)
  (let ((output-dir (personal-site-org-output-directory subtreep))
        (postid (org-id-get)))
    (org-export-to-buffer 'personal-site-json "*Org JSON Export*"
      async subtreep visible-only body-only
      (org-combine-plists
       ext-plist
       `( :personal-site-output-directory ,output-dir
          :personal-site-postid ,postid))
      #'js-json-mode)))

(defun personal-site-org-export-to-site
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to post subdirectory.
The post subdirectory is determined by `personal-site-destination-dir'.
Several files will be created in this directory:
- An \"index.html\", containing the post HTML content.
- A \"metadata.json\", containing metadata about the post.
- An \"assets/\" subdirectory, containing all attachments (see
  `personal-site--attachment-copy').
These files constitute all the files needed for the site.

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
         (org-html-htmlize-output-type nil) ; Force exporting raw HTML without styling
         (output-dir (personal-site-org-output-directory subtreep))
         (index-file (expand-file-name "index.html" output-dir))
         (postid (org-id-get))
         (metadata-file (expand-file-name "metadata.json" output-dir)))
    (personal-site--org-prepare-output-directory output-dir)
    
    ;; Metadata
    (org-export-to-file 'personal-site-json metadata-file
      async subtreep visible-only body-only
      (org-combine-plists
       ext-plist
       `( :personal-site-output-directory ,output-dir
          :personal-site-postid ,postid)))
    ;; Content
    (org-export-to-file 'personal-site-html index-file
      async subtreep visible-only body-only
      (org-combine-plists
       ext-plist
       `(:personal-site-output-directory ,output-dir)))))

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
      (personal-site-org-export-to-site
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
    :base-directory ,krisb-manuscript-blog-posts-directory
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
