;;; org-blogging-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Everything necessary for creating static websites using org-mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'cl)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Ox-hugo
;;;; Itself
;; Using the Hugo static cite generator as an option for exporting files
(use-package ox-hugo
  :demand
  :after ox
  :ensure-system-package (hugo go)
  :custom
  (org-hugo-base-dir (expand-file-name "hugo/" org-directory))
  (org-hugo-section "posts")
  ;; `nil' if you don't want to export to the static directory. This is
  ;; desirable if, for instance, you leverage page bundles for each post.
  (org-hugo-default-static-subdirectory-for-externals nil)
  (org-hugo-auto-set-lastmod nil)       ; Use lastmod?
  (org-hugo-suppress-lastmod-period 604800) ; Only use lastmod if modified at least a week later
  :config
  (defvar kb/org-hugo-exclude-tags
    '("ATTACH" "project" "PROJECT" "draft"
      "section" "series" "tag" "category"
      "creative-writing")
    "Tags to exclude. Look at `kb/org-hugo--tag-processing-fn-ignore-tags-maybe'.")
  (defun kb/org-hugo--tag-processing-fn-ignore-tags-maybe (tag-list info)
    "Ignore tags which match a string found in `kb/org-hugo-exclude-tags'."
    (cl-set-difference tag-list kb/org-hugo-exclude-tags :test #'equal))
  (add-to-list 'org-hugo-tag-processing-functions #'kb/org-hugo--tag-processing-fn-ignore-tags-maybe))

;;;; Bundle support
(with-eval-after-load 'ox-hugo
  (defvar kb/org-hugo-bundle-workflow t
    "Whether I am using a Hugo bundle workflow. Relevant for my
`kb/org-hugo--get-pub-dir'. A non-nil value means bundles are
given a default name; see `kb/org-hugo--get-pub-dir'.")

  (defun kb/org-hugo-title-slug (title)
    "Turn an org-roam processed slug into a hyphenated slug, that is,
replacing underscores with hyphens. Returns a string."
    ;; (message "[kb/org-hugo-title-slug DBG] title: %s" title)
    (file-name-as-directory
     (cond
      ((featurep 'denote)
       (denote-sluggify title))
      ((featurep 'org-roam)
       (string-replace "_" "-" title)))))

  ;; Set a default bundle name (there isn't one without this)
  (defun kb/org-hugo--get-pub-dir (info)
    "Return the post publication directory path.

The publication directory is created if it does not exist.

INFO is a plist used as a communication channel.

My version of this function sets the default bundle-dir to be the
slug of the file's title with underscores replaced for hyphens."
    (let* ((base-dir (if (plist-get info :hugo-base-dir)
                         (file-name-as-directory (plist-get info :hugo-base-dir))
                       (user-error "It is mandatory to set the HUGO_BASE_DIR property or the `org-hugo-base-dir' local variable")))
           (content-dir "content/")
           (section-path (org-hugo--get-section-path info))
           ;; Use the bundle path if its value exists (with underscores replaced
           ;; with hyphens). If it doesn't, then defer to using the file's title
           ;; slug as a default value, but only if `kb/org-hugo-bundle-workflow'
           ;; is non-nil. Otherwise, have bundle-dir be an empty string.
           (bundle-dir (let ((bundle-path (or ; Hugo bundle set in the post subtree gets higher precedence
                                           (org-hugo--entry-get-concat nil "EXPORT_HUGO_BUNDLE" "/")
                                           (plist-get info :hugo-bundle))) ;This is mainly to support per-file flow
                             (default-bundle-path
                              (cond
                               ((featurep 'denote)
                                (kb/org-hugo-title-slug (car (plist-get info :title))))
                               ((featurep 'org-roam)
                                (org-roam-node-slug
                                 (org-roam-node-create :title (car (plist-get info :title))))))))
                         (cond
                          ;; If hugo_bundle is set
                          ((and bundle-path ; Keyword must exist unless error
                                (not (string= bundle-path ""))) ; Non-empty string value
                           (kb/org-hugo-title-slug bundle-path))
                          (kb/org-hugo-bundle-workflow
                           (kb/org-hugo-title-slug default-bundle-path))
                          (t ""))))
           (pub-dir (let ((dir (concat base-dir content-dir section-path bundle-dir)))
                      (make-directory dir :parents) ;Create the directory if it does not exist
                      dir)))
      (file-truename pub-dir)))
  (advice-add 'org-hugo--get-pub-dir :override #'kb/org-hugo--get-pub-dir))

;;;; Exporting links
(with-eval-after-load 'ox-hugo
  (defun kb/org-export-resolve-denote-link (link info)
    "Return `denote' file referenced as LINK destination.

INFO is a plist used as a communication channel.

Return value will be the file name of LINK destination. Throw an
error if no match is found."
    (let* ((denote-id (org-element-property :path link))
           (denote-files (denote-directory-files-matching-regexp denote-id)))
      ;; (message "[denote-files]: %s" denote-files)
      ;; (message "[car of denote-files]: %s" (car denote-files))
      (cond
       ((and denote-files (equal 1 (length denote-files)))
        (car denote-files))
       (denote-files
        (user-error "[kb/org-export-resolve-denote-link]: Multiple notes with that ID! %s" denote-files))
       (t                               ; If link is broken
        (signal 'org-link-broken (list denote-id))))))

  ;; NOTE 2022-03-12: This is a janky way to get links working with page
  ;; bundles. **REQUIRES THE BUNDLE NAME OF EACH POST TO MATCH THE POST'S FILE
  ;; NAME.**
  (defun kb/org-hugo-link (link desc info)
    "Convert LINK to Markdown format.

DESC is the link's description.
INFO is a plist used as a communication channel.

Unlike `org-md-link', this function will also copy local images
and rewrite link paths to make blogging more seamless."
    (let* ((raw-link (org-element-property :raw-link link))
           (raw-path (org-element-property :path link))
           (type (org-element-property :type link))
           (link-is-url (member type '("http" "https" "ftp" "mailto"))))
      ;; (message "[org-hugo-link DBG] raw-path 1: %s" raw-path)

      (when (and (stringp raw-path)
                 link-is-url)
        (setq raw-path (org-blackfriday--url-sanitize-maybe
                        info (url-encode-url raw-path))))
      ;; (message "[org-hugo-link DBG] raw-link: %s" raw-link)
      ;; (message "[org-hugo-link DBG] raw-path 2: %s" raw-path)
      ;; (message "[org-hugo-link DBG] link: %S" link)
      ;; (message "[org-hugo-link DBG] link type: %s" type)
      (cond
       ;; For `denote' support!
       ((string= type "denote")
        ;; (message "[org-hugo-link DBG] hugo-bundle: %s" (plist-get info :hugo-bundle))
        ;; (message "[org-hugo-link DBG] title %s" (plist-get info :title))
        (let* ((destination (kb/org-export-resolve-denote-link link info))
               (note-file (denote-retrieve-title-value destination 'org))
               (path
                (if (string= ".org" (downcase (file-name-extension destination ".")))
                    (if (and kb/org-hugo-bundle-workflow (plist-get info :hugo-bundle))
                        (concat (file-name-as-directory (kb/org-hugo-title-slug note-file))
                                "index.md")
                      (concat note-file ".md"))
                  destination)))
          ;; (message "[org-hugo-link DBG] link path: %s" path)
          ;; (message "[org-hugo-link DBG] link desc: %s" desc)
          (if desc
              (format "[%s]({{< relref \"%s\" >}})" desc path)
            (format "[%s]({{< relref \"%s\" >}})" path path))))
       ;; Link type is handled by a special function.
       ((org-export-custom-protocol-maybe link desc 'md))
       ((member type '("custom-id" "id"
                       "fuzzy")) ;<<target>>, #+name, heading links
        (let ((destination (if (string= type "fuzzy")
                               (org-export-resolve-fuzzy-link link info)
                             (org-export-resolve-id-link link info))))
          ;; (message "[org-hugo-link DBG] link type: %s" type)
          ;; (message "[org-hugo-link DBG] destination: %s" destination)
          ;; (message "[org-hugo-link DBG] link: %S" link)
          ;; (message "[org-hugo-link DBG] link destination elem type: %S" (org-element-type destination))
          (pcase (org-element-type destination)
            ;; External file.
            (`plain-text
             (let ((path (progn
                           ;; (message "[org-hugo-link DBG] hugo-bundle: %s" (plist-get info :hugo-bundle))
                           ;; (message "[org-hugo-link DBG] title %s" (plist-get info :title))
                           ;; Treat links to `file.org' as links to `file.md'.
                           (if (string= ".org" (downcase (file-name-extension destination ".")))
                               ;; NOTE 2022-06-02: I made changes here to get
                               ;; links to between bundles working. If the
                               ;; hugo_bundle file property exists, then this
                               ;; changes the destination appropriately.
                               (if (and kb/org-hugo-bundle-workflow (plist-get info :hugo-bundle))
                                   (concat (kb/org-hugo-title-slug (file-name-sans-extension destination))
                                           "index.md")
                                 (concat (file-name-sans-extension destination) ".md"))
                             destination))))
               ;; (message "[org-hugo-link DBG] plain-text path: %s" path)
               (if (org-id-find-id-file raw-path)
                   (let* ((anchor (org-hugo-link--heading-anchor-maybe link info))
                          (ref (if (and (org-string-nw-p anchor)
                                        (not (string-prefix-p "#" anchor)))
                                   ;; If the "anchor" doesn't begin with
                                   ;; "#", it's a direct reference to a
                                   ;; post subtree.
                                   anchor
                                 (concat path anchor))))
                     ;; (message "[org-hugo-link DBG] plain-text org-id anchor: %S" anchor)
                     (format "[%s]({{< relref \"%s\" >}})" (or desc path) ref))
                 (if desc
                     (format "[%s](%s)" desc path)
                   (format "<%s>" path)))))
            ;; Links of type [[* Some heading]].
            (`headline
             (let ((title (org-export-data (org-element-property :title destination) info)))
               (format
                "[%s](#%s)"
                ;; Description
                (cond ((org-string-nw-p desc))
                      ((org-export-numbered-headline-p destination info)
                       (mapconcat #'number-to-string
                                  (org-export-get-headline-number destination info)
                                  "."))
                      (t
                       title))
                ;; Reference
                (org-hugo--get-anchor destination info))))
            ;; Links to other Org elements like source blocks, tables,
            ;; paragraphs, standalone figures, <<target>> links, etc.
            (_
             (let ((description
                    (or (org-string-nw-p desc)
                        (let ((number (org-export-get-ordinal
                                       destination info
                                       nil #'org-html--has-caption-p)))
                          (when number
                            (let ((num-str (if (atom number)
                                               (number-to-string number)
                                             (mapconcat #'number-to-string number "."))))
                              ;; (message "[org-hugo-link DBG] num-str: %s" num-str)
                              (if org-hugo-link-desc-insert-type
                                  (let* ((type (org-element-type destination))
                                         ;; Org doesn't have a specific
                                         ;; element for figures. So if
                                         ;; the element is `paragraph',
                                         ;; and as this element has an
                                         ;; ordinal, we will assume that
                                         ;; to be a figure.
                                         (type (if (equal 'paragraph type)
                                                   'figure
                                                 type))
                                         (type-str (org-blackfriday--translate type info)))
                                    (format "%s %s" type-str num-str))
                                num-str)))))))
               ;; (message "[org-hugo-link DBG] link description: %s" description)
               (when description
                 (let ((dest-link (cond
                                   ;; Ref to a source block or table.
                                   ((memq (org-element-type destination) '(src-block table))
                                    (org-blackfriday--get-reference destination))
                                   ;; Ref to a standalone figure.
                                   ((and (org-html-standalone-image-p destination info)
                                         (eq (org-element-type destination) 'paragraph))
                                    (let ((figure-ref (org-blackfriday--get-reference destination)))
                                      (if (org-string-nw-p figure-ref)
                                          (replace-regexp-in-string
                                           "\\`org-paragraph--"
                                           (org-blackfriday--get-ref-prefix 'figure)
                                           figure-ref)
                                        (org-export-get-reference destination info))))
                                   ;; Ref to a <<target>>.
                                   ((eq (org-element-type destination) 'target)
                                    (org-blackfriday--get-target-anchor destination))
                                   ;; Ref to all other link destinations.
                                   (t
                                    (org-export-get-reference destination info)))))
                   (format "[%s](#%s)" description dest-link))))))))
       ((org-export-inline-image-p link org-html-inline-image-rules)
        ;; (message "[org-hugo-link DBG] processing an image: %s" desc)
        (let* ((parent (org-export-get-parent link))
               (parent-type (org-element-type parent))
               ;; If this is a hyper-linked image, it's parent type will
               ;; be a link too. Get the parent of *that* link in that
               ;; case.
               (grand-parent (when (eq parent-type 'link)
                               (org-export-get-parent parent)))
               (useful-parent (if grand-parent
                                  grand-parent
                                parent))
               (attr (org-export-read-attribute :attr_html useful-parent))
               (caption (or
                         ;; Caption set using #+caption takes higher precedence.
                         (org-string-nw-p
                          (org-export-data  ;Look for caption set using #+caption
                           (org-export-get-caption (org-export-get-parent-element link))
                           info))
                         (plist-get attr :caption)))
               (caption (when (org-string-nw-p caption)
                          (format "%s%s%s%s"
                                  "<span class=\"figure-number\">"
                                  (format (org-html--translate
                                           (concat
                                            (cdr (assoc 'figure org-blackfriday--org-element-string))
                                            " %d:")
                                           info)
                                          (org-export-get-ordinal
                                           useful-parent info
                                           nil #'org-html--has-caption-p))
                                  " </span>"
                                  caption)))
               (extension (file-name-extension raw-path))
               (inlined-svg (and (stringp extension)
                                 (string= "svg" (downcase extension))
                                 (plist-get attr :inlined))))
          ;; (message "[org-hugo-link DBG] Inline image: %s, extension: %s" raw-path extension)
          ;; (message "[org-hugo-link DBG] inlined svg? %S" inlined-svg)
          ;; (message "[org-hugo-link DBG] caption: %s" caption)
          (if inlined-svg
              (let* ((svg-contents (with-temp-buffer
                                     (insert-file-contents raw-path)
                                     (fill-region (point-min) (point-max)) ;Make huge one-liner SVGs sane
                                     (buffer-substring-no-properties (point-min) (point-max))))
                     (svg-contents-sanitized (replace-regexp-in-string
                                              ;; Remove the HTML comments.
                                              "<!--\\(.\\|\n\\)*?-->" ""
                                              (replace-regexp-in-string
                                               ;; Remove the xml document tag as that cannot be inlined in-between
                                               ;; a Markdown (or even an HTML) file.
                                               "<\\?xml version=\"1\\.0\" encoding=\"UTF-8\" standalone=\"no\"\\?>" ""
                                               ;; Remove !DOCTYPE tag from the inlined SVG.
                                               (replace-regexp-in-string
                                                "<!DOCTYPE svg[^>]+>" ""
                                                svg-contents))))
                     (svg-html (if caption
                                   (format "<figure>\n%s\n<figcaption>\n\n  %s\n</figcaption>\n</figure>"
                                           svg-contents-sanitized caption)
                                 svg-contents-sanitized)))
                ;; (message "[org-hugo-link DBG] svg contents: %s" svg-contents)
                ;; (message "[org-hugo-link DBG] svg contents sanitized: %s" svg-contents-sanitized)
                svg-html)
            (let* ((path (org-hugo--attachment-rewrite-maybe raw-path info))
                   (inline-image (not (org-html-standalone-image-p useful-parent info)))
                   (source (if link-is-url
                               (concat type ":" path)
                             path))
                   (num-attr (/ (length attr) 2)) ;(:alt foo) -> num-attr = 1
                   (alt-text (plist-get attr :alt)))
              ;; (message "[org-hugo-link DBG] path: %s" path)
              ;; (message "[org-hugo-link DBG] inline image? %s" inline-image)
              ;; (message "[org-hugo-link DBG] attr: %s num of attr: %d"
              ;;          attr (length attr))
              ;; (message "[org-hugo-link DBG] parent-type: %s" parent-type)
              ;; (message "[org-hugo-link DBG] useful-parent-type: %s"
              ;;          (org-element-type useful-parent))
              (cond
               (;; Use the Markdown image syntax if the image is inline and
                ;; there are no HTML attributes for the image, or just one
                ;; attribute, the `alt-text'.
                (and inline-image
                     (or (= 0 num-attr)
                         (and alt-text
                              (= 1 num-attr))))
                (let ((alt-text (if alt-text
                                    alt-text
                                  "")))
                  (format "![%s](%s)" alt-text source)))
               (;; Else if the image is inline (with non-alt-text
                ;; attributes), use HTML <img> tag syntax.
                inline-image
                ;; The "target" and "rel" attributes would be meant for <a>
                ;; tags. So do not pass them to the <img> tag.
                (plist-put attr :target nil)
                (plist-put attr :rel nil)
                (org-html--format-image source attr info))
               (t ;Else use the Hugo `figure' shortcode.
                ;; Hugo `figure' shortcode named parameters.
                ;; https://gohugo.io/content-management/shortcodes/#figure
                (let ((figure-params `((src . ,source)
                                       (alt . ,alt-text)
                                       (caption . ,(when (org-string-nw-p caption)
                                                     (replace-regexp-in-string "\"" "\\\\\\&" caption))) ;Escape the double-quotes, if any.
                                       (link . ,(plist-get attr :link))
                                       (title . ,(plist-get attr :title))
                                       (class . ,(plist-get attr :class))
                                       (attr . ,(plist-get attr :attr))
                                       (attrlink . ,(plist-get attr :attrlink))
                                       (width . ,(plist-get attr :width))
                                       (height . ,(plist-get attr :height))
                                       ;; While the `target' and `rel'
                                       ;; attributes are not supported by
                                       ;; the inbuilt Hugo `figure'
                                       ;; shortcode, they can be used as
                                       ;; intended if a user has a custom
                                       ;; `figure' shortcode with the
                                       ;; support added for those.
                                       (target . ,(plist-get attr :target))
                                       (rel . ,(plist-get attr :rel))))
                      (figure-param-str ""))
                  (dolist (param figure-params)
                    (let ((name (car param))
                          (val (cdr param)))
                      (when val
                        (setq figure-param-str (concat figure-param-str
                                                       (format "%s=\"%s\" "
                                                               name val))))))
                  ;; (message "[org-hugo-link DBG] figure params: %s" figure-param-str)
                  (format "{{< figure %s >}}" (org-trim figure-param-str)))))))))
       ((string= type "coderef")
        (let* ((ref-label (org-element-property :path link))
               (ref-info (org-hugo-link--resolve-coderef ref-label info))
               (desc (format (org-export-get-coderef-format ref-label desc)
                             (plist-get ref-info :ref))))
          ;; (message "[org-hugo-link DBG] coderef ref label: %s" ref-label)
          ;; (message "[org-hugo-link DBG] coderef ref str: %s" (plist-get ref-info :ref))
          ;; (message "[org-hugo-link DBG] coderef anchor prefix: %s" (plist-get ref-info :anchor-prefix))
          ;; (message "[org-hugo-link DBG] coderef line num: %s" (plist-get ref-info :line-num))
          ;; (message "[org-hugo-link DBG] coderef desc: %s" desc)
          (format "[%s](#%s-%s)"
                  desc
                  (plist-get ref-info :anchor-prefix)
                  (plist-get ref-info :line-num))))
       ((string= type "radio")
        (let ((destination (org-export-resolve-radio-link link info)))
          (format "[%s](#%s%s)"
                  desc
                  (org-blackfriday--get-ref-prefix 'radio)
                  (org-blackfriday--valid-html-anchor-name
                   (org-element-property :value destination)))))
       (t ;[[file:foo.png]], [[file:foo.org::* Heading]], [[file:foo.org::#custom-id]], link type: file
        (let* ((link-param-str "")
               (path (cond
                      (link-is-url
                       ;; Taken from ox-html.el -- Extract attributes
                       ;; from parent's paragraph.  HACK: Only do this
                       ;; for the first link in parent (inner image link
                       ;; for inline images).  This is needed as long as
                       ;; attributes cannot be set on a per link basis.
                       (let* ((attr
                               (let ((parent (org-export-get-parent-element link)))
                                 (and (eq (org-element-map parent 'link #'identity info :first-match) link)
                                      (org-export-read-attribute :attr_html parent))))
                              ;; https://www.w3schools.com/tags/tag_link.asp
                              (link-params `((title . ,(plist-get attr :title))
                                             (style . ,(plist-get attr :style))
                                             (referrerpolicy . ,(plist-get attr :referrerpolicy))
                                             (media . ,(plist-get attr :media))
                                             (target . ,(plist-get attr :target))
                                             (rel . ,(plist-get attr :rel))
                                             (sizes . ,(plist-get attr :sizes))
                                             (type . ,(plist-get attr :type)))))
                         (dolist (param link-params)
                           (let ((name (car param))
                                 (val (cdr param)))
                             (when val
                               (setq link-param-str (concat link-param-str
                                                            (format "%s=\"%s\" "
                                                                    name val))))))
                         ;; (message "[org-hugo-link DBG] link params: %s" link-param-str)
                         )
                       (concat type ":" raw-path))
                      (;; Remove the "file://" prefix.
                       (string= type "file")
                       ;; (message "[org-hugo-link DBG] raw-path: %s" raw-path)
                       (let* ((path1 (replace-regexp-in-string "\\`file://" "" raw-path))
                              (path-lc (downcase path1)))
                         (cond
                          (;; foo.org, foo.org::* Heading, foo.org::#custom_id
                           (string= ".org" (file-name-extension path-lc "."))
                           (let ((ref "")
                                 (anchor ""))
                             (if (string-suffix-p org-hugo--preprocessed-buffer-dummy-file-suffix path-lc)
                                 (progn
                                   (setq ref (string-remove-suffix
                                              org-hugo--preprocessed-buffer-dummy-file-suffix
                                              (file-name-nondirectory path1)))
                                   ;; Dummy Org file paths created in
                                   ;; `org-hugo--get-pre-processed-buffer'
                                   ;; For dummy Org file paths, we are
                                   ;; limiting to only "#" style search
                                   ;; strings.
                                   (when (string-match ".*\\.org::\\(#.*\\)" raw-link)
                                     (setq anchor (match-string-no-properties 1 raw-link))))
                               ;; Regular Org file paths.
                               (setq ref (file-name-sans-extension (file-name-nondirectory path1)))
                               (let ((link-search-str
                                      ;; If raw-link is "./foo.org::#bar",
                                      ;; set `link-search-str' to
                                      ;; "#bar".
                                      (when (string-match ".*\\.org::\\(.*\\)" raw-link)
                                        (match-string-no-properties 1 raw-link))))
                                 ;; (message "[org-hugo-link DBG] link-search-str: %s" link-search-str)
                                 (when link-search-str
                                   (setq anchor (org-hugo--search-and-get-anchor raw-path link-search-str info)))))
                             ;; (message "[org-hugo-link file.org::*Heading DBG] ref    = %s" ref)
                             ;; (message "[org-hugo-link file.org::*Heading DBG] anchor = %s" anchor)
                             (cond
                              ;; Link to a post subtree.  In this case,
                              ;; the "anchor" is actually the post's
                              ;; slug.
                              ((and (org-string-nw-p anchor) (not (string-prefix-p "#" anchor)))
                               (format "{{< relref \"%s\" >}}" anchor))
                              ;; Link to a non-post subtree, like a subheading in a post.
                              ((or (org-string-nw-p ref) (org-string-nw-p anchor))
                               (format "{{< relref \"%s%s\" >}}" ref anchor))
                              (t
                               ""))))
                          (t ;; attachments like foo.png
                           (org-hugo--attachment-rewrite-maybe path1 info)))))
                      (t
                       raw-path)))
               (link-param-str (org-string-nw-p (org-trim link-param-str))))
          ;; (message "[org-hugo-link DBG] desc=%s path=%s" desc path)
          ;; (message "[org-hugo-link DBG] link-param-str=%s" link-param-str)
          (cond
           ;; Link description is a `figure' shortcode but does not
           ;; already have the `link' parameter set.
           ((and desc
                 (string-match-p "\\`{{<\\s-*figure\\s-+" desc)
                 (not (string-match-p "\\`{{<\\s-*figure\\s-+.*link=" desc)))
            (replace-regexp-in-string "\\s-*>}}\\'"
                                      (format " link=\"%s\"\\&" path)
                                      desc))
           ;; Both link description and link attributes are present.
           ((and desc
                 link-param-str)
            (format "<a href=\"%s\" %s>%s</a>"
                    (org-html-encode-plain-text path)
                    link-param-str
                    (org-link-unescape desc)))
           ;; Only link description, but no link attributes.
           (desc
            (let* ((path-has-space (and
                                    (not (string-prefix-p "{{< relref " path))
                                    (string-match-p "\\s-" path)))
                   (path (if path-has-space
                             ;; https://github.com/kaushalmodi/ox-hugo/issues/376
                             ;; https://github.com/gohugoio/hugo/issues/6742#issuecomment-573924706
                             (format "<%s>" path)
                           path)))
              (format "[%s](%s)" desc path)))
           ;; Only link attributes, but no link description.
           (link-param-str
            (let ((path (org-html-encode-plain-text path)))
              (format "<a href=\"%s\" %s>%s</a>"
                      path
                      link-param-str
                      ;; Below trick is to prevent Hugo from
                      ;; auto-hyperlinking the link in the
                      ;; description. Idea from
                      ;; https://stackoverflow.com/q/25706012/1219634.
                      (replace-regexp-in-string ":" "&colon;" (org-link-unescape path)))))
           ;; Neither link description, nor link attributes.
           (t
            (if (string-prefix-p "{{< relref " path)
                (format "[%s](%s)" path path)
              (format "<%s>" path)))))))))
  (advice-add 'org-hugo-link :override #'kb/org-hugo-link))

;;;; Magic keyword management
(with-eval-after-load 'ox-hugo
  (defun kb/find-blog-files-org ()
    "Return a list of org files which are within the blog subdirectory
of `kb/notes-dir'."
    (directory-files-recursively kb/blog-dir ""))

  (defun kb/org-hugo--add-tag-maybe ()
    "Add a FILETAGS value if necessary. Right now I only need the
draft tag for nodes with a value of true for hugo_draft."
    (when (and (not (active-minibuffer-window))
               (member buffer-file-name (kb/find-blog-files-org))
               (assoc "TITLE" (org-collect-keywords '("title"))))
      (save-excursion
        (beginning-of-buffer)
        (let* ((keywords '("filetags" "hugo_draft"))
               (collected-keywords (org-collect-keywords keywords))
               (hugo_draft_value (cadr (assoc "HUGO_DRAFT" collected-keywords)))
               (filetags_value (cadr (assoc "FILETAGS" collected-keywords))))
          (pcase hugo_draft_value
            ("false"
             (when (stringp filetags_value)
               (cond
                ((featurep 'denote)
                 (let ((tags-list (split-string filetags_value ":" 'omit-nulls)))
                   (if (< 0 (length tags-list))
                       (denote--rewrite-keywords (buffer-file-name)
                                                 (denote-keywords-sort
                                                  (remove "draft" tags-list))
                                                 'org)
                     (delete-region (re-search-forward (denote--keywords-key-regexp 'org) nil t 1) (pos-eol)))))
                ((featurep 'org-roam)
                 (org-roam-tag-remove '("draft"))))))
            ("true"
             (cond
              ((featurep 'denote)
               (denote--rewrite-keywords (buffer-file-name)
                                         (denote-keywords-sort
                                          (delete-dups
                                           (append (list "draft")
                                                   (split-string filetags_value ":" 'omit-nulls))))
                                         'org))
              ((featurep 'org-roam)
               (org-roam-tag-add '("draft"))))))))))
  ;; FIXME 2022-06-01: Point isn't preserved if added to `before-save-hook'
  (add-hook 'before-save-hook #'kb/org-hugo--add-tag-maybe)

  ;; Set the value of the hugo_bundle keyword (for blog post org files) if it is
  ;; empty. Inspired by `vulpea-project-update-tag'
  (defun kb/org-hugo--add-hugo-metadata-maybe ()
    "Update the export_file_name and hugo_draft file properties in
the current hugo buffer if they do not exist."
    (when (and (not (active-minibuffer-window))
               (member buffer-file-name (kb/find-blog-files-org))
               (assoc "TITLE" (org-collect-keywords '("title"))))
      (save-excursion
        (let* ((keywords '("export_file_name" "hugo_draft"))
               (keywords (mapcar #'(lambda (elt) (upcase elt)) keywords))
               (collected-keywords (org-collect-keywords keywords))
               (non-existent-keywords (cl-remove-if
                                       (lambda (keyword) (assoc keyword collected-keywords))
                                       keywords))
               (empty-keywords (cl-remove-if-not
                                (lambda (keyword) (string= (cadr (assoc keyword collected-keywords)) ""))
                                keywords))
               (default-export-file-name "index")
               (default-hugo-draft "true")
               (new-value))
          (dolist (keyword (append non-existent-keywords empty-keywords))
            (setq new-value (pcase keyword
                              ("EXPORT_FILE_NAME" default-export-file-name)
                              ("HUGO_DRAFT" default-hugo-draft)))
            (kb/org-set-keyword keyword new-value))))))

  ;; Org-export all files in an org-roam subdirectory. Modified from
  ;; https://sidhartharya.me/exporting-org-roam-notes-to-hugo/
  (defun kb/org-hugo-export-all ()
    "Export all org-roam files to Hugo in my blogging directory."
    (interactive)
    (when (featurep 'org-roam)
      (require 'org-roam)
      (org-roam-update-org-id-locations)) ; Necessary for id's to be recognized for exports
    ;; First delete all old posts; only works if `kb/org-hugo-bundle-workflow'
    ;; is non-nil. Useful for if I renamed a node.
    (when-let ((kb/org-hugo-bundle-workflow)
               (subdirs (cdr (ffap-all-subdirs (file-name-concat org-hugo-base-dir "content/" org-hugo-section) 1))))
      (dolist (post-dir subdirs "Deleted old posts")
        (delete-directory post-dir t t)))
    (let ((post-count 0))
      (dolist (file (cl-remove-if-not
                     (lambda (file)
                       ;; Don't look at files without the title,
                       ;; hugo_publishdate, or hugo_draft keywords or with an
                       ;; empty value. The criterion of having a
                       ;; hugo_publishdate is ignored if the value of hugo_draft
                       ;; is true. The criteria of a hugo_publishdate and
                       ;; hugo_draft are ignored if there is a "series" filetag.
                       (with-temp-buffer
                         (delay-mode-hooks (org-mode))
                         (when file (insert-file-contents file))
                         (let* ((keywords '("title" "filetags" "hugo_publishdate" "hugo_draft" "hugo_section"))
                                (collected-keywords (org-collect-keywords keywords))
                                (title-p (assoc "TITLE" collected-keywords))
                                (taxonomy-p (assoc "HUGO_SECTION" collected-keywords)))
                           ;; If hugo_draft is false, then the hugo_publishdate
                           ;; should exist and have a value. If hugo_draft doesn't
                           ;; exist, then it'll return nil.
                           (cond
                            ((and title-p taxonomy-p))
                            (title-p
                             (pcase (cadr (assoc "HUGO_DRAFT" collected-keywords))
                               ("false"
                                (let* ((publish-pair (assoc "HUGO_PUBLISHDATE" collected-keywords))
                                       (date (cadr publish-pair)))
                                  (and (stringp date)
                                       (not (string= date "")))))
                               ("true" t)))))))
                     (kb/find-blog-files-org)))
        (let ((inhibit-message t))          ; Don't show the messages in Echo area
          (with-current-buffer (find-file-noselect file)
            (read-only-mode -1)
            (kb/org-hugo--add-hugo-metadata-maybe)
            (kb/org-hugo--add-tag-maybe)
            (kb/format-buffer-indentation)
            (org-hugo-export-wim-to-md)
            (setq post-count (1+ post-count))

            (unless (member (get-buffer (buffer-name)) (buffer-list)) ; Kill buffer unless it already exists
              (kill-buffer)))))
      (message "Done - Exported %s blog notes!" post-count))))

;;;; Fix TOC including tags
(with-eval-after-load 'ox-hugo
  (defun kb/org-hugo--build-toc (info &optional n scope local)
    "Return table of contents as a string.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is a positive integer
specifying the depth of the table.

When optional argument SCOPE is non-nil, build a table of
contents according to the specified element.

When optional argument LOCAL is non-nil, build a table of
contents according to the current heading.

My version of this function has tags processed through
`org-hugo-tag-processing-functions' before they are added to the
TOC."
    (let* ((toc-heading
            (unless local
              (format "\n<div class=\"heading\">%s</div>\n"
                      (org-html--translate "Table of Contents" info))))
           (current-level nil)
           (toc-items
            (mapconcat
             (lambda (heading)
               (let* ((level-raw (org-export-get-relative-level heading info))
                      (level (if scope
                                 (let* ((current-level-inner
                                         (progn
                                           (unless current-level
                                             (setq current-level level-raw))
                                           current-level))
                                        (relative-level
                                         (1+ (- level-raw current-level-inner))))
                                   ;; (message (concat "[ox-hugo build-toc DBG] "
                                   ;;                  "current-level-inner:%d relative-level:%d")
                                   ;;          current-level-inner relative-level)
                                   relative-level)
                               level-raw))
                      (indentation (make-string (* 4 (1- level)) ?\s))
                      (todo (and (org-hugo--plist-get-true-p info :with-todo-keywords)
                                 (org-element-property :todo-keyword heading)))
                      (todo-str (if todo
                                    (concat (org-hugo--todo todo info) " ")
                                  ""))
                      (heading-num-list (org-export-get-headline-number heading info))
                      (number (if heading-num-list
                                  ;; (message "[ox-hugo TOC DBG] heading-num-list: %S" heading-num-list)
                                  (org-hugo--get-heading-number heading info :toc)
                                ""))
                      (toc-entry
                       (format "[%s%s](#%s)"
                               todo-str
                               (org-export-data-with-backend
                                (org-export-get-alt-title heading info)
                                (org-export-toc-entry-backend 'hugo)
                                info)
                               (org-hugo--get-anchor heading info)))
                      (tags (and (plist-get info :with-tags)
                                 (not (eq 'not-in-toc (plist-get info :with-tags)))
                                 (let ((tags (org-export-get-tags heading info)))
                                   ;; I added these two lines to have tags
                                   ;; properly processed
                                   (dolist (fn org-hugo-tag-processing-functions tags)
                                     (setq tags (funcall fn tags info)))
                                   (and tags
                                        (format ":%s:"
                                                (mapconcat #'identity tags ":")))))))
                 ;; (message "[ox-hugo build-toc DBG] level:%d, number:%s" level number)
                 ;; (message "[ox-hugo build-toc DBG] indentation: %S" indentation)
                 ;; (message "[ox-hugo build-toc DBG] todo: %s | %s" todo todo-str)
                 (concat indentation "- " number toc-entry tags)))
             (org-export-collect-headlines info n scope)
             "\n"))                       ;Newline between TOC items
           ;; Remove blank lines from in-between TOC items, which can
           ;; get introduced when using the "UNNUMBERED: t" heading
           ;; property.
           (toc-items (org-string-nw-p
                       (replace-regexp-in-string "\n\\{2,\\}" "\n" toc-items))))
      ;; (message "[ox-hugo build-toc DBG] toc-items:%s" toc-items)
      (when toc-items
        (let ((toc-classes '("toc" "ox-hugo-toc"))
              ;; `has-section-numbers' is non-nil if section numbers are
              ;; present for even one heading.
              (has-section-numbers (string-match-p "^\\s-*\\-\\s-<span class=\"section\\-num\"" toc-items)))
          (when has-section-numbers
            (push "has-section-numbers" toc-classes))
          (when local
            (push "local" toc-classes))
          (concat (format "<div class=\"%s\">\n" (string-join (reverse toc-classes) " "))
                  (unless (org-hugo--plist-get-true-p info :hugo-goldmark)
                    "<div></div>\n") ;This is a nasty workaround till Hugo/Blackfriday support
                  toc-heading    ;wrapping Markdown in HTML div's.
                  "\n"
                  toc-items ;https://github.com/kaushalmodi/ox-hugo/issues/93
                  "\n\n"
                  "</div>\n"
                  ;; Special comment that can be use to filter out the TOC
                  ;; from .Summary in Hugo templates.
                  ;;
                  ;;     {{ $summary_splits := split .Summary "<!--endtoc-->" }}
                  ;;     {{ if eq (len $summary_splits) 2 }}
                  ;;         <!-- If that endtoc special comment is present, output only the part after that comment as Summary. -->
                  ;;         {{ index $summary_splits 1 | safeHTML }}
                  ;;     {{ else }}
                  ;;         <!-- Print the whole Summary if endtoc special comment is not found. -->
                  ;;         {{ .Summary }}
                  ;;     {{ end }}
                  "<!--endtoc-->\n")))))
  (advice-add 'org-hugo--build-toc :override #'kb/org-hugo--build-toc))

;;; org-blogging-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-blogging-rcp)
