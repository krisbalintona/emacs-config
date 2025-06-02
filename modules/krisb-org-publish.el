;; -*- lexical-binding: t; -*-

;; WISHLIST:
;; - Tags + tag pages
;; - Backlinks (“Mentions of this page...”)
;; - “Series” --- post groups?
;; - Better home page
;;   - Archive
;; - Post categories
;;   - Tech
;;   - Other
;; - Non-js search
;; - Better post metadata
;;   - Word count
;;
;; IMPLEMENTATION NOTES:
;;
;; TAGS:
;;
;; 2025-06-02: We can have multiple functions for
;; :publication-function.  I think I should scrape all files to
;; generate a tag list, then create an org page that is the exported
;; for each tag page.  Not sure how to add option to add custom
;; content to these pages.  Maybe check for the existence of an
;; associated file/note with a tag?

;;; Bespoke org-export HTML backend

;;;; Headline transcoder
;; NOTE 2025-05-30: The functions
;; `krisb-org-publish-new-title-reference’ and
;; `krisb-org-export-new-title-reference'’ were created with
;; https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#export-to-html-with-useful-anchors
;; as a reference.  The difference between my code and the reference
;; code is that my code (1) is structured as not to advise the
;; original org export functions and (2) sluggifies titles according
;; to kebab-case, rather than `url-hexify-string’ with parent headline
;; text prepended.

;; Take in a heading title and output a meaningful anchor name.
;; Copied from `org-node-slugify-for-web’
(defun krisb-org-publish-sluggify-headline-for-web (title)
  "From TITLE, make a filename slug meant to look nice as URL component.

A title like \"Löb\\='s Theorem\" becomes \"lobs-theorem\".

Diacritical marks U+0300 to U+0331 are stripped \(mostly used with Latin
alphabets).  Also stripped are all glyphs not categorized in Unicode as
belonging to an alphabet or number system."
  (unless (stringp title)
    (error "[krisb-org-publish-sluggify-headline-for-web] Title is not a string!"))
  (thread-last title
               (string-glyph-decompose)
               (seq-remove (lambda (char) (<= #x300 char #x331)))
               (concat)
               (string-glyph-compose)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[[:space:]]+" "-")
               (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
               (replace-regexp-in-string "\\/" "-")
               (replace-regexp-in-string "--*" "-")
               (replace-regexp-in-string "^-" "")
               (replace-regexp-in-string "-$" "")))

;; Inspired by code outlined here:
;; https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#export-to-html-with-useful-anchors
(defun krisb-org-publish-new-headline-reference (headline cache)
  "Return new reference for HEADLINE that is unique in CACHE.
This function returns the text of HEADLINE sluggified into kebab-case,
and uniquified with other identically-titled headlines with appended
numbers.  For example, if two headlines have the title \"foo\”, then the
second one’s reference will be \“foo-1\”."
  (let* ((title (org-element-property :raw-value headline))
         (ref (krisb-org-publish-sluggify-headline-for-web (substring-no-properties title))) ; Sluggify title into kebab-case
         ;; Find all existing cached references that a prefixed with
         ;; ref; these are “duplicates” in the sense that the title
         ;; text of these are identical
         (duplicates (thread-last cache
                                  (seq-filter
                                   (lambda (cell)
                                     (and (stringp (car cell))
                                          (string-match-p (rx (literal ref) (zero-or-one "-" digit digit))
                                                          (car cell)))))
                                  (mapcar #'car))))
    ;; If there are no duplicates, then this ref is original and we
    ;; return it.  If there are duplicates, then we find then
    ;; increment the largest index number and append it to ref,
    ;; returning a result like “foo-2”.
    (if duplicates
        (let* ((largest-duplicate
                (string-remove-prefix ref (car (sort duplicates :reverse t))))
               ;; A special case is the first duplicate: it is just
               ;; the ref without an appended index
               (new-index (1+ (string-to-number (if (string-empty-p largest-duplicate)
                                                    "0"
                                                  (substring largest-duplicate 1))))))
          (format "%s-%s" ref new-index))
      ref)))

;; Inspired by code outlined here:
;; https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#export-to-html-with-useful-anchors
(defun krisb-org-publish-get-headline-reference (headline info)
  "Return a unique reference for HEADLINE, as a string.
Like `org-export-get-reference’ but only for headlines that have text
and returns a human-meaningful version (sluggified) of the headline
title (see `krisb-org-publish-sluggify-headline-for-web')."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq headline cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells headline))
               ;; Preserve any pre-existing association between a
               ;; search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (reference-string
                ;; [KB] 2025-05-30: For the cells which belong to
                ;; previously cached reference strings, check if any
                ;; of them are prefixed by our slug.  The reason we
                ;; check the prefix is because we uniquify slugs by
                ;; appending e.g. “-1” to them.  Return all of those
                ;; cells, if any.
                (or (cl-some
                     (lambda (cell)
                       (let ((stored (cdr (assoc cell crossrefs))))
                         (and stored
                              (stringp stored)
                              (not (assoc (krisb-org-publish-sluggify-headline-for-web stored) cache)))))
                     cells)
                    (krisb-org-publish-new-headline-reference headline cache))))
          ;; Cache contains both data already associated to a
          ;; reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell reference-string) cache))
          ;; Retain a direct association between reference string and
          ;; HEADLINE since (1) not every object or element can be
          ;; given a search cell (2) it permits quick lookup.
          (push (cons reference-string headline) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun krisb-org-publish-html--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
  (let* ((type (org-element-type datum))
         (custom-id (and (memq type '(headline inlinetask))
                         (org-element-property :CUSTOM_ID datum)))
         (user-label
          (or
           custom-id
           (and (memq type '(radio-target target))
                (org-element-property :value datum))
           (org-element-property :name datum)
           (when-let* ((id (org-element-property :ID datum)))
             (concat org-html--id-attr-prefix id)))))
    (cond
     ((and user-label
           ;; [KB] 2025-05-30: Normally, the presence of the CUSTOM_ID
           ;; property will be used unconditionally.  I dislike that,
           ;; so I change the behavior here: only use a user-label if
           ;; and only if :html-prefer-user-labels is non-nil.
           (plist-get info :html-prefer-user-labels))
      user-label)
     ((and named-only
           (not (memq type '(headline inlinetask radio-target target)))
           (not user-label))
      nil)
     ;; [KB] 2025-05-29: When DATUM is a headline (with text), then
     ;; use our own reference-string-creator that outputs a
     ;; human-meaningful reference string
     ((and (eq type 'headline)
           (org-element-property :raw-value datum))
      (krisb-org-publish-get-headline-reference datum info))
     (t
      (org-export-get-reference datum info)))))

(defun krisb-org-publish-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist holding
contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
           ;; [KB] 2025-05-29: Use meaningful anchor names
           (id (krisb-org-publish-html--reference headline info))
           (formatted-text
            (if (plist-get info :html-self-link-headlines)
                (format "<a href=\"#%s\">%s</a>" id full-text)
              full-text)))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
            (concat
             (and (org-export-first-sibling-p headline info)
                  (apply #'format "<%s class=\"org-%s\">\n"
                         (make-list 2 html-type)))
             (org-html-format-list-item
              contents (if numberedp 'ordered 'unordered)
              nil info nil
              (concat (org-html--anchor id nil nil info) formatted-text)) "\n"
             (and (org-export-last-sibling-p headline info)
                  (format "</%s>\n" html-type))))
        ;; Standard headline.  Export it as a section.
        (let ((extra-class
               (org-element-property :HTML_CONTAINER_CLASS headline))
              (headline-class
               (org-element-property :HTML_HEADLINE_CLASS headline))
              (first-content (car (org-element-contents headline))))
          (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (format "outline-container-%s" id)
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  (format "\n<h%d id=\"%s\"%s>%s</h%d>\n"
                          level
                          id
                          (if (not headline-class) ""
                            (format " class=\"%s\"" headline-class))
                          (concat
                           (and numberedp
                                (format
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (concat (mapconcat #'number-to-string numbers ".") ".")))
                           formatted-text)
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (org-element-type-p first-content 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))

;;; Bespoke org-publish backend and publishing function
(defun krisb-org-publish-post-file-name (file-name project)
  "Transform FILE-NAME into one suitable for a post.
FILE-NAME is a file name without its directory or an absolute file path.
The returned file name will be the file name as a timestamp of the org
file (as returned by `org-publish-find-date').

PROJECT is an org-project entry."
  (let* ((file-name (file-name-nondirectory file-name))
         (time (org-publish-find-date file-name project))
         (timestamp (format-time-string "%Y%m%d%H%M" time)))
    (file-name-with-extension timestamp "html")))

(defun krisb-org-publish-sitemap-format-entry (entry style project)
  "Format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project.

Like `org-publish-sitemap-default-entry’ but transform file paths (which
end up as the URL) to place all posts in the posts subdirectory of the
publishing directory and change post file names to their sluggified
titles.

With this, since it formats entries the way I like, I ignore the value
of :sitemap-style."
  (if (not (directory-name-p entry))
      (let* ((file-name (krisb-org-publish-post-file-name entry project))
             (path (file-name-concat "posts" file-name))
             (title (org-publish-find-title entry project)))
        (format "[[file:%s][%s]]" path title))
    (t entry)))

(defun krisb-org-publish-org-to (backend filename extension plist &optional pub-dir)
  "Publish an Org file to a specified backend.

BACKEND is a symbol representing the backend used for transcoding.
FILENAME is the filename of the Org file to be published.  EXTENSION is
the extension used for the output string, with the leading dot.  PLIST
is the property list for the given project.

Optional argument PUB-DIR, when non-nil is the publishing directory.

Return output file name.

Like `org-publish-org-to’ but hijack the output file name, since these
names will be the URL on the site.  In particular, if the org file is a
regular post, we change the output file name to a timestamp.  This lets
each post have a non-changing URL (i.e., permalink)."
  (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))
  ;; Check if a buffer visiting FILENAME is already open.
  (let* ((org-inhibit-startup t))
    (org-with-file-buffer filename
      ;; [KB] NOTE 2025-05-30: Right now, we assume that the thing
      ;; we’re exporting is the file (and cannot be a subtree).  I
      ;; think this is a correct assumption since org-publish seems
      ;; only to support file-level exports.
      (let* ((project (org-publish-get-project-from-filename filename))
             ;; [KB] NOTE 2025-06-01: We must take care to consider
             ;; the sitemap file if it is generated; it goes through
             ;; this as well.
             (output
              ;; [KB] FIXME 2025-06-01: Figure out the best way not to
              ;; hard-code the sitemap org file.
              (if (equal (file-name-nondirectory filename) "index.org")
                  (org-export-output-file-name extension nil pub-dir)
                (krisb-org-publish-post-file-name filename project))))
        (org-export-to-file backend (expand-file-name output pub-dir)
          nil nil nil (plist-get plist :body-only)
          ;; Add `org-publish--store-crossrefs' and
          ;; `org-publish-collect-index' to final output filters.  The
          ;; latter isn't dependent on `:makeindex', since we want to
          ;; keep it up-to-date in cache anyway.
          (org-combine-plists
           plist
           `(:crossrefs
             ,(org-publish-cache-get-file-property
               ;; Normalize file names in cache.
               (file-truename filename) :crossrefs nil t)
             :filter-final-output
             (org-publish--store-crossrefs
              org-publish-collect-index
              ,@(plist-get plist :filter-final-output)))))))))

;; Our own org-publish publishing function
(defun krisb-org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.
FILENAME is the file-name of the Org file to be published.  PLIST is the
property list for the given project.  PUB-DIR is the publishing
directory.

Return output file name."
  (krisb-org-publish-org-to 'krisb-site-html filename
                            (concat (when (> (length org-html-extension) 0) ".")
                                    (or (plist-get plist :html-extension)
                                        org-html-extension
                                        "html"))
                            plist pub-dir))

;; Define our org-publish backend
(with-eval-after-load 'ox
  (require 'ox-html)
  (org-export-define-derived-backend
      'krisb-site-html 'html
    :translate-alist
    '(;; (template . dw/org-html-template)
      ;; (link . dw/org-html-link)
      ;; (src-block . dw/org-html-src-block)
      ;; (special-block . dw/org-html-special-block)
      (headline . krisb-org-publish-html-headline))
    ;; :options-alist
    ;; '((:video "VIDEO" nil nil)
    ;;   (:page-type "PAGE-TYPE" nil nil))
    ))

;;; Ox-publish
(use-package ox-publish
  :ensure nil
  :defer t
  :custom
  ;; NOTE 2025-05-30: I set this to nil for now because I am
  ;; developing the site.  Probably should set this to non-nil in the
  ;; future.
  (org-publish-use-timestamps-flag nil)
  (org-publish-project-alist
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
  :init
  (use-package esxml :ensure t)
  :config
  (use-package simple-httpd :ensure t))

;;; Provide
(provide 'krisb-org-publish)
