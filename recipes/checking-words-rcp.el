;;; checking-words-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages relevant to the dictionaries and thesauruses.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Online
;;;; Define-word
;; See definitions of words from an online dictionary.
(use-package dictionary
  :gfhook 'hide-mode-line-mode
  :custom
  (dictionary-use-single-buffer t))     ; Reuse dictionary buffers

;;;; Powerthesaurus
;; Search for synonyms using an online thesaurus.
(use-package powerthesaurus)

;;; Offline
;;;; Wordnut
;; Offline dictionary
(use-package wordnut
  :after define-word
  ;; TODO 2021-08-20: Have this changed depending on Linux distribution
  ;; Make sure the install a dictnary, in case it is a separate package
  :ensure-system-package (wn . wordnet-cli))

;;;; Synosaurus
;; Offline thesaurus
(use-package synosaurus
  :after powerthesaurus
  ;; TODO 2021-08-20: Have this changed depending on Linux distribution
  :ensure-system-package (wn . wordnet) ; Make sure English dictionary is also installed
  :custom
  (synosaurus-backend 'synosaurus-backend-wordnet) ; Offline thesaurus that relies on `wordnet'
  (synosaurus-choose-method 'default))

;;;; kb/{dictionary,thesaurus}-at-point kb/{dictionary,thesaurus}-lookup
;; Change which packages are used depending on internet connection
(defun kb/internet-up-p (&optional host)
  "Return `t' if the device has internet access, and `nil'
otherwise. Credit to https://emacs.stackexchange.com/a/18515"
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                     (if host host "www.google.com"))))

;; TODO 2022-06-04: Better integrate bindings with other packages
(defun kb/dictionary-at-point ()
  "Call a dictionary command or `helpful-at-point'.

If the major-mode is derived from text-mode, then call a
dictionary command for the word at point depending on the
internet connection.

If not (i.e. in a prog-mode derived major mode), then call
`helpful-at-point' instead. If the sexp at point does not
exist,then throw an error message."
  (interactive)
  (let ((dict-function (if (kb/internet-up-p)
                           'dictionary-lookup-definition
                         'wordnut-lookup-current-word))
        (in-comment-p (nth 4 (syntax-ppss)))
        (in-string-p (nth 3 (syntax-ppss)))
        (valid-symbol-p (or (fboundp (symbol-at-point))
                            (boundp (symbol-at-point)))))
    (cond
     ((derived-mode-p 'text-mode)
      (funcall dict-function))
     (valid-symbol-p
      (helpful-at-point))
     ((or in-comment-p in-string-p)
      (funcall dict-function))
     (t
      (error "Not a valid symbol!")))))

(defun kb/thesaurus-at-point ()
  "Use `powerthesaurus' if online, and `synosaurus' if offline."
  (interactive)
  (when (or (derived-mode-p 'prog-mode) (nth 4 (syntax-ppss)))
    (if (kb/internet-up-p)
        (powerthesaurus-lookup-synonyms-dwim)
      (synosaurus-choose-and-replace))))

(defun kb/dictionary-lookup ()
  "Use `dictionary' if online, and `wordnet' if offline."
  (interactive)
  (if (kb/internet-up-p)
      (funcall 'dictionary-search)
    (wordnut-search)))

(defun kb/thesuarus-lookup ()
  "Use `powerthesaurus' if online, and `synosaurus' if offline."
  (interactive)
  (if (kb/internet-up-p)
      (powerthesaurus-lookup-dwim)
    (synosaurus-choose-and-insert)))

;; TODO 2022-06-04: Integrate devdocs
(general-define-key
 (general-chord "jj") 'kb/dictionary-at-point
 (general-chord "JJ") 'kb/dictionary-lookup
 (general-chord "kk") 'kb/thesaurus-at-point
 (general-chord "KK") 'kb/thesuarus-lookup)

;;; checking-words-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-words-rcp)
