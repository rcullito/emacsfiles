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
   ("\M-z" . zap-up-to-char)
   ("<f9>" . global-linum-mode)))


(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package magit
  :bind   (("C-x g" . magit-status)
           ("C-c g" . magit-file-dispatch))
  :custom (magit-log-section-commit-count 40))

(use-package dumb-jump
  :ensure t
  :custom (dumb-jump-prefer-searcher 'rg)
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package expand-region
  :bind ("C-o" . er/expand-region))

(use-package paredit
  :hook   ((cider-repl-mode
            clojure-mode
            emacs-lisp-mode
            lisp-mode
            slime-repl-mode) . paredit-mode)
  :bind   (("<f7>" . paredit-wrap-square)
           ("<f8>" . paredit-wrap-curly)))

(use-package clojure-mode
  :defer t
  :bind ("<f3>" . clojure-thread-first-all))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck
  :hook   (clojure-mode . flycheck-mode))

(use-package flycheck-clj-kondo
  :after clojure-mode)

(use-package deadgrep
  :bind   ("C-c d" . deadgrep))

(use-package git-link
  :defer t)

(use-package ace-window
  :bind   ("M-o" . ace-window))

(use-package company
  :hook  ((prog-mode cider-repl-mode) . company-mode))

(use-package multiple-cursors
  :bind   (("<f1>" . mc/mark-next-like-this)
           ("<f2>" . mc/mark-all-like-this)))

(use-package which-key
  :hook (prog-mode . which-key-mode))

(use-package cider
  :defer t
  :bind (:map cider-repl-mode-map
              ("<up>" . #'cider-repl-backward-input)
              ("<down>" . #'cider-repl-forward-input)))

(use-package crux
  :ensure t
  :defer t
  :bind (("C-x 4 t" . crux-transpose-windows)
         ("C-c I" . crux-find-user-init-file)))

(use-package slime
  :defer t)

(use-package restclient
  :defer   t)

(use-package dockerfile-mode
  :defer   t)

(straight-use-package 'clj-refactor)

(setq-default indent-tabs-mode nil)
;; (setq org-html-postamble nil)

;;cljr-warn-on-eval  will create ASTs for all the namespaces at REPL start up if this is set to nil
(nilf cljr-warn-on-eval make-backup-files)

;; modes
(global-auto-revert-mode)

(menu-bar-mode -1)

;; ;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; fn keys for emacs core

;; ;; Misc
(global-prettify-symbols-mode 1)
(put 'narrow-to-region 'disabled nil)

(setq inhibit-splash-screen t
      initial-scratch-message "")



(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq inferior-lisp-program "clisp")


;; Stop customize from writing to this file
 
(provide 'init)

;;; init.el ends here
