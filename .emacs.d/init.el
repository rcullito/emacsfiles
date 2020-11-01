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

(use-package vterm
  :custom (vterm-always-compile-module t)
  :ensure t
  :bind ("<f6>" . vterm))

; (use-package guaranteed-emacs
;  :straight (:host github :repo "Guaranteed-Rate/guaranteed-emacs")
;  :config (set-common-vars) (setenv "PROCESS_QUEUES" "true"))

;; bind, hook, mode, all imply a defer
(use-package magit
  :bind   (("C-x g" . magit-status)
           ("C-c g" . magit-file-dispatch))
  :custom (magit-log-section-commit-count 40))


(use-package dumb-jump
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom (dumb-jump-prefer-searcher 'rg))

(use-package expand-region
  :bind ("C-o" . er/expand-region))

(use-package paredit
  :hook   ((cider-repl-mode clojure-mode emacs-lisp-mode) . paredit-mode)
  :bind   (("<f7>" . paredit-wrap-square)
           ("<f8>" . paredit-wrap-curly)))

(use-package clojure-mode
  :defer t
  :bind ("<f3>" . clojure-thread-first-all))

(use-package cider
  :defer t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck
  :hook   (clojure-mode . flycheck-mode))

(use-package flycheck-clj-kondo
  :after clojure-mode)


(use-package smooth-scrolling
  :hook   (clojure-mode . smooth-scrolling-mode))

(use-package deadgrep
  :bind   ("C-c d" . deadgrep))

(use-package treemacs
  :defer   t)

(use-package nyan-mode
  :defer   t)

(use-package ace-window
  :bind   ("M-o" . ace-window))

(use-package company
  :hook  ((prog-mode cider-repl-mode) . company-mode))

(use-package multiple-cursors
  :bind   (("<f1>" . mc/mark-next-like-this)
           ("<f2>" . mc/mark-all-like-this)))


(use-package which-key
  :config (which-key-mode))

(use-package helm
  :bind ("M-x" . helm-M-x))

(use-package projectile
  :bind ("C-c s" . projectile-switch-project))

(use-package helm-projectile
  :after (helm projectile)
  :bind ("C-c p f" . helm-projectile-find-file))


;; (straight-use-package 'terraform-mode)
;; (straight-use-package 'yaml-mode)

;; (straight-use-package 'slime)
;; (require 'helm-projectile)
;; (helm-projectile-on)
;; (straight-use-package 'jenkinsfile-mode)
;; (projectile-mode)

;; modes
(global-auto-revert-mode)
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(which-key-mode)


;; ;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)


;; fn keys for emacs core

(global-set-key (kbd "<f4>") 'backward-paragraph)
(global-set-key (kbd "<f5>") 'forward-paragraph)
(global-set-key (kbd "<f9>") 'display-line-numbers-mode)

;; ;; Misc
(global-prettify-symbols-mode 1)
(put 'narrow-to-region 'disabled nil)


(defun just-no-space ()
  (interactive)
  (setq current-prefix-arg '(0)) ; C-u
  (call-interactively 'just-one-space))

(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-c l") 'just-no-space)
(global-set-key (kbd "C-c u") 'delete-indentation)
(global-set-key "\C-x\C-b" 'buffer-menu)

(setq inhibit-splash-screen t
      initial-scratch-message "")
(setq make-backup-files nil)

;; Stop customize from writing to this file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
 
(provide 'init)

;;; init.el ends here
