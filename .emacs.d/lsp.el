
(up lsp-mode
    :config (add-hook 'clojure-mode-hook 'lsp))

(up lsp-treemacs)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      lsp-clojure-custom-server-command '("bash" "-c" "/usr/local/bin/clojure-lsp")
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )
