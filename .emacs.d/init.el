;;; init.el --- Initialization code ;;; -*- lexical-binding: t;-*-
;;; Commentary:

;;; Code:

(defvar straight-vc-git-default-protocol)
(setq straight-vc-git-default-protocol 'ssh)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

(setq user-full-name "Rob Culliton")
(setq user-mail-address "rob.culliton@gmail.com")


;; bind, hook, mode, all imply a defer


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
  :hook   ((cider-repl-mode clojure-mode emacs-lisp-mode) . paredit-mode)
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
(setq cljr-warn-on-eval nil) ;; will create ASTs for all the namespaces at REPL start up if this is set to nil

;; modes
(global-auto-revert-mode)
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)

;; ;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; fn keys for emacs core

(global-set-key (kbd "<f9>") 'display-line-numbers-mode)

;; ;; Misc
(global-prettify-symbols-mode 1)
(put 'narrow-to-region 'disabled nil)


(defun just-no-space ()
  (interactive)
  (setq current-prefix-arg '(0)) ; C-u
  (call-interactively 'just-one-space))


(require 'misc)
(global-set-key (kbd "<right>") 'forward-to-word)
(global-set-key (kbd "<left>") 'backward-to-word)
(global-set-key (kbd "<up>") 'backward-paragraph)
(global-set-key (kbd "<down>") 'forward-paragraph)
(global-set-key "\M-z" 'zap-up-to-char)

(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-c l") 'just-no-space)
(global-set-key (kbd "C-c u") 'delete-indentation)
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key (kbd "C-c i") 'indent-region)

(setq inhibit-splash-screen t
      initial-scratch-message "")
(setq make-backup-files nil)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(defvar next-buffer-count)
(setq next-buffer-count 2)

(defun earmuff (var-name)
  (concat "*" var-name "*"))

(defun new-scratch ()
  (interactive)
  (let* ((buffer-num (number-to-string next-buffer-count))
         (buffer-name (earmuff (concat "scratch-" buffer-num))))
    (progn
      (setq next-buffer-count (+ next-buffer-count 1))
      (switch-to-buffer buffer-name))))


;; Stop customize from writing to this file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
 
(provide 'init)

;;; init.el ends here
