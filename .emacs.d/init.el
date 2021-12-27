;;; init.el --- Initialization code ;;; -*- lexical-binding: t;-*-
;;; Commentary:

;;; Code:


(require 'org)
(require 'misc)

(defun load-local (file-name)
  (load (concat user-emacs-directory
                file-name)))

(load-local "bootstrap.el")
(load-local "util.el")

(emacs-core-keybindings
 '(("C--" . undo)
   ("C-c l" . just-no-space)
   ("C-c u" . delete-indentation)
   ("\C-x\C-b" . buffer-menu)
   ("C-c i" . indent-region)
   ("<right>" . forward-to-word)
   ("<left>" . backward-to-word)
   ("<up>" . backward-paragraph)
   ("<down>" . forward-paragraph)
   ("M-a" . zap-up-to-char)
   ("M-p" . hippie-expand)
   ("<f9>" . global-linum-mode)))

(use-package guaranteed-emacs
  :ensure t
  :straight (:host github :repo "Guaranteed-Rate/guaranteed-emacs")
  :config
  (set-common-vars))


(up smex
  :ensure t
  :bind ("M-x" . smex))

(up magit
  :bind   (("C-x g" . magit-status)
           ("C-c g" . magit-file-dispatch))
  :custom (magit-log-section-commit-count 40))

(up dumb-jump
  :ensure t
  :custom (dumb-jump-prefer-searcher 'rg)
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(up expand-region
  :bind ("C-o" . er/expand-region))

(up paredit
  :hook   ((cider-repl-mode
            clojure-mode
            emacs-lisp-mode
            lisp-mode
            slime-repl-mode
            scheme-mode) . paredit-mode)
  :bind   (("<f7>" . paredit-wrap-square)
           ("<f8>" . paredit-wrap-curly)))

(up clojure-mode
  :defer t
  :bind ("<f3>" . clojure-thread-first-all))

(up rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(up flycheck
    :ensure t
    :hook   ((clojure-mode web-mode) . flycheck-mode))

(up flycheck-clj-kondo
  :after clojure-mode)

(up deadgrep
  :bind   ("C-c d" . deadgrep))

(up git-link
  :defer t)

(up ace-window
    :bind   ("M-o" . ace-window))

(up company
  :hook  ((prog-mode cider-repl-mode) . company-mode))

(up multiple-cursors
  :bind   (("<f1>" . mc/mark-next-like-this)
           ("<f2>" . mc/mark-all-like-this)))

(up which-key
  :hook (prog-mode . which-key-mode))

(up cider
  :defer t
  :bind (:map cider-repl-mode-map
              ("<up>" . #'cider-repl-backward-input)
              ("<down>" . #'cider-repl-forward-input)))

(up crux
    :ensure t
    :defer t
    :bind (("C-x 4 t" . crux-transpose-windows)
           ("C-c I" . crux-find-user-init-file)))

(up slime
  :defer t)

(up restclient
  :defer   t)

(up dockerfile-mode
    :defer   t)

(up treemacs
    :defer t)


(straight-use-package 'clj-refactor)

(straight-use-package 'projectile)
(projectile-mode)
(global-set-key (kbd "C-c s") 'projectile-switch-project)
(straight-use-package 'helm)
(straight-use-package 'helm-projectile)
;; (require 'helm-projectile)
(helm-projectile-on)
(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
(global-set-key (kbd "C-c p h") 'helm-projectile)
(global-set-key (kbd "M-x") 'helm-M-x)


(load-local "typescript.el")

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-treemacs)
(add-hook 'clojure-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )

(setq lsp-clojure-custom-server-command '("bash" "-c" "/usr/local/bin/clojure-lsp"))

;; beginning of assignment
;; setq-default will apply to all buffers
(setq-default indent-tabs-mode nil)

;;cljr-warn-on-eval  will create ASTs for all the namespaces at REPL start up if this is set to nil
(nilf cljr-warn-on-eval make-backup-files org-html-postamble)

(setq inhibit-splash-screen t
      initial-scratch-message ""
      inferior-lisp-program "clisp")

;; end of assignment

;; modes
(global-auto-revert-mode)
(menu-bar-mode -1)

;; ;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)


;; ;; Misc
(global-prettify-symbols-mode 1)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Stop customize from writing to this file
 
(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((cider-preferred-build-tool . lein)
     (eval when
           (require 'rainbow-mode nil t)
           (rainbow-mode 1))
     (eval setenv "DEV_QUIET_REPL" "1")
     (cider-figwheel-main-default-options . "dev")
     (cider-default-cljs-repl . figwheel-main)))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
