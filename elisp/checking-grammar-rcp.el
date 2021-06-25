;;; checking-grammar-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages relevant to checking grammar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Writegood-mode
;; Catch self-defined weasel words
(use-package writegood-mode
  :disabled t ; Not necessary anymore with proselint syntax checker in flycheck
  :hook (text-mode . writegood-mode)
  )

;;;; Langtool
;; Use langtool program to check grammar of current buffer
(use-package langtool
  ;; ensure-system-package (languagetool)
  :custom
  (langtool-default-language "en-US")
  (langtool-autoshow-message-function 'langtool-autoshow-detail-popup)
  ;; (langtool-java-classpath "~/Downloads/LanguageTool-5.3-stable")

  (langtool-bin "~/Downloads/LanguageTool-5.3-stable/languagetool.jar")
  (langtool-language-tool-jar "~/Downloads/LanguageTool-5.3-stable/languagetool-commandline.jar")
  (langtool-language-tool-server-jar "~/Downloads/LanguageTool-5.3-stable/languagetool-server.jar")
  (langtool-java-bin "/usr/bin/java")

  (langtool-server-user-arguments '("-p" "8082"))
  (langtool-http-server-host 'nil)
  (langtool-http-server-port 8082)
  (langtool-http-server-stream-type 'tls)
  (langtool-default-language "en-US")

  :preface
  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  :config
  (pretty-hydra-define hydra:langtool
    (:color green :hint t :foreign-keys run :quit-key "q" :exit t)
    ("Correct grammar"
     (("b" #'langtool-check-buffer "Check")
      ("c" #'langtool-correct-buffer "Correct")
      ("d" #'langtool-check-done "Done"))
     ))

  (general-define-key
   "C-c g" '(hydra:langtool/body :which-key "Langtool")
   )
  )

;;; checking-grammar-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-grammar-rcp)
