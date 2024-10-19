;;; Exec-path-from-shell
;; Ensure Emacs' and system shell have same path
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH" "BROWSER"
     ;; Language paths
     "GOPATH"
     ;; `ssh-agent' environment variables. See
     ;; https://wiki.archlinux.org/title/SSH_keys#Start_ssh-agent_with_systemd_user
     "SSH_AGENT_PID" "SSH_AUTH_SOCK"
     ;; For LSP-mode. See
     ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
     "LSP_USE_PLISTS"))
  :config
  (exec-path-from-shell-initialize)

  ;; For npm. Found here:
  ;; https://www.reddit.com/r/emacs/comments/s6zkb6/comment/ht794j7/?utm_source=share&utm_medium=web2x&context=3
  (when (eq (length (getenv "NODE_PATH")) 0)
    (setenv "NODE_PATH" "/usr/local/lib/node_modules")))

;;; Provide
(provide 'krisb-system-env)
