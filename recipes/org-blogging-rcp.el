;;; org-blogging-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Everything necessary for creating static websites using org-mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Ox-hugo
;; Using the Hugo static cite generator as an option for exporting files
(use-package ox-hugo
  :defer 7
  :commands kb/org-hugo-org-roam-sync-all
  :ensure-system-package (hugo go)
  :custom
  (org-hugo-base-dir (concat org-directory "hugo/"))
  (org-hugo-section "posts")
  ;; `nil' if you don't want to export to the static directory. This is
  ;; desirable if, for instance, you leverage page bundles for each post.
  (org-hugo-default-static-subdirectory-for-externals nil)
  (org-hugo-auto-set-lastmod nil)       ; Use lastmod?
  (org-hugo-suppress-lastmod-period 604800) ; Only use lastmod if modified at least a week later
  :init
  (defvar kb/org-hugo-exclude-tags '("project" "ATTACH" "draft")
    "Tags to exclude. Look at `kb/org-hugo--tag-processing-fn-ignore-tags-maybe'.")
  :config
  (defun kb/org-hugo--add-tag-maybe ()
    "Add a FILETAGS value if necessary. Right now I only need the
draft tag for nodes with a value of true for hugo_draft."
    (when (and (not (active-minibuffer-window))
               (member buffer-file-name (kb/find-blog-files-org))
               (assoc "TITLE" (org-collect-keywords '("title"))))
      (save-excursion
        (let* ((keywords '("filetags" "hugo_draft"))
               (collected-keywords (org-collect-keywords keywords))
               (hugo_draft_value (cadr (assoc "HUGO_DRAFT" collected-keywords)))
               (filetags_value (cadr (assoc "FILETAGS" collected-keywords))))
          (pcase hugo_draft_value
            ("false"
             (when (stringp filetags_value)
               (org-roam-tag-remove '("draft"))))
            ("true"
             (org-roam-tag-add '("draft")))
            )))))
  ;; FIXME 2022-06-01: Point isn't preserved if added to `before-save-hook'
  (add-hook 'before-save-hook #'kb/org-hugo--add-tag-maybe)

  ;; Set the value of the hugo_bundle keyword (for blog post org files) if it is
  ;; empty. Inspired by `vulpea-project-update-tag'
  (defun kb/org-hugo--add-hugo-metadata-maybe ()
    "Update the hugo_bundle, export_file_name, and hugo_draft file
properties in the current buffer hugo buffer if they do not
exist."
    (when (and (not (active-minibuffer-window))
               (member buffer-file-name (kb/find-blog-files-org))
               (assoc "TITLE" (org-collect-keywords '("title"))))
      (save-excursion
        (let* ((keywords '("hugo_bundle" "export_file_name" "hugo_draft"))
               (keywords (mapcar #'(lambda (elt) (upcase elt)) keywords))
               (collected-keywords (org-collect-keywords keywords))
               (non-existent-keywords (cl-remove-if
                                       (lambda (keyword) (assoc keyword collected-keywords))
                                       keywords))
               (empty-keywords (cl-remove-if-not
                                (lambda (keyword) (string= (cadr (assoc keyword collected-keywords)) ""))
                                keywords))
               (default-bundle (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
               (default-export-file-name "index")
               (default-hugo-draft "true")
               (new-value))
          (dolist (keyword (append non-existent-keywords empty-keywords))
            (setq new-value (pcase keyword
                              ("HUGO_BUNDLE" default-bundle)
                              ("EXPORT_FILE_NAME" default-export-file-name)
                              ("HUGO_DRAFT" default-hugo-draft)))
            (org-roam-set-keyword keyword new-value))))))

  ;; Org-export all files in an org-roam subdirectory. Modified from
  ;; https://sidhartharya.me/exporting-org-roam-notes-to-hugo/
  (defun kb/org-hugo-org-roam-sync-all ()
    "Export all org-roam files to Hugo in my blogging directory."
    (interactive)
    (require 'org-roam)
    (org-roam-update-org-id-locations) ; Necessary for id's to be recognized for exports
    (dolist (file (cl-remove-if-not
                   (lambda (file)
                     ;; Don't look at files without the title, hugo_publishdate,
                     ;; or hugo_draft keywords or with an empty value. The
                     ;; criterion of having a hugo_publishdate is ignored if the
                     ;; value of hugo_draft is true
                     (org-roam-with-temp-buffer file
                       (let* ((keywords '("title" "hugo_publishdate" "hugo_draft"))
                              (collected-keywords (org-collect-keywords keywords)))
                         (and
                          (assoc "TITLE" collected-keywords)      ; Has title
                          ;; If hugo_draft is false, then the hugo_publishdate
                          ;; should exist and have a value. If hugo_draft
                          ;; doesn't exist, then it'll return nil.
                          (pcase (cadr (assoc "HUGO_DRAFT" collected-keywords))
                            ("false"
                             (let* ((publish-pair (assoc "HUGO_PUBLISHDATE" collected-keywords))
                                    (date (cadr publish-pair)))
                               (and (stringp date)
                                    (not (string= date "")))))
                            ("true" t)))
                         )))
                   (kb/find-blog-files-org)))
      (with-current-buffer (find-file-noselect file)
        (read-only-mode -1)
        (kb/org-hugo--add-tag-maybe)
        (kb/org-hugo--add-hugo-metadata-maybe)
        (org-hugo-export-wim-to-md)
        (unless (member (get-buffer (buffer-name)) (buffer-list)) ; Kill buffer unless it already exists
          (kill-buffer)))))

  (defun kb/org-hugo--tag-processing-fn-ignore-tags-maybe (tag-list info)
    "Ignore tags which match a string found in `kb/org-hugo-exclude-tags'."
    (cl-set-difference tag-list kb/org-hugo-exclude-tags :test #'equal))
  (add-to-list 'org-hugo-tag-processing-functions #'kb/org-hugo--tag-processing-fn-ignore-tags-maybe)

  ;; NOTE 2022-03-12: This is a janky way to use page bundles with ox-hugo.
  ;; Requires the bundle name of each post to match the post's file name.
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
                               (if (plist-get info :hugo-bundle)
                                   (concat (file-name-sans-extension destination) "/index.md")
                                 (concat (file-name-sans-extension destination) ".md"))
                             destination))))
               ;; (message "[org-hugo-link DBG] plain-text path: %s" path)
               (if (org-id-find-id-file raw-path)
                   (let* ((anchor (org-hugo-link--heading-anchor-maybe link info))
                          (anchor-str (if (org-string-nw-p anchor)
                                          (concat "#" anchor)
                                        "")))
                     ;; (message "[org-hugo-link DBG] plain-text org-id anchor: %S" anchor)
                     ;; (message "[org-hugo-link DBG] plain-text org-id anchor-str: %S" anchor-str)
                     (if desc
                         (format "[%s]({{< relref \"%s%s\" >}})" desc path anchor-str)
                       (format "[%s]({{< relref \"%s%s\" >}})" path path anchor-str)))
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
       (t ;[[file:foo.png]], [[file:foo.org::* Heading]], [[file:foo.org::#custom-id]],
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
                             ;; (message "[org-hugo-link DBG] link search anchor: %S" anchor)
                             (format "{{< relref \"%s%s\" >}}" ref anchor)))
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

;;; org-blogging-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-blogging-rcp)
