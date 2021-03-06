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
  :bind ("<f6>" . vterm))

(when (getenv "WORK_LAPTOP")
  (use-package guaranteed-emacs
   :ensure t
   :straight (:host github :repo "Guaranteed-Rate/guaranteed-emacs")
   :config
   (set-common-vars)
   (setenv "PROCESS_QUEUES" "true")
   (setenv "LOAD_SINGLE_LOAN" "1798d26b-0758-4965-a350-925c6e5dcda1")))

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
  :hook (prog-mode . which-key-mode))

(use-package cider
  :defer t
  :bind (:map cider-repl-mode-map
              ("<up>" . #'cider-repl-backward-input)
              ("<down>" . #'cider-repl-forward-input)))

(use-package crux
  :ensure t
  :bind ("C-x 4 t" . crux-transpose-windows))


;; beginning of helm madness

(straight-use-package 'projectile)
(projectile-mode)
(global-set-key (kbd "C-c s") 'projectile-switch-project)
(straight-use-package 'helm)
(straight-use-package 'helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)
(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
(global-set-key (kbd "C-c p h") 'helm-projectile)
(global-set-key (kbd "M-x") 'helm-M-x)
;; TODO potentially model these modes after which-key mode, prog mode dotted pair
;; (use-package projectile
;;   :bind ("C-c s" . projectile-switch-project))

;; (use-package helm
;;   :bind ("M-x" . helm-M-x))

;; (use-package helm-projectile
;;   :bind ("C-c p f" . helm-projectile-find-file))

;; end of helm madness


(straight-use-package 'terraform-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'slime)
(straight-use-package 'jenkinsfile-mode)

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
(global-set-key (kbd "C-c i") 'indent-region)

(setq inhibit-splash-screen t
      initial-scratch-message "")
(setq make-backup-files nil)

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

(defun mount ()
  (interactive)
  (goto-char (point-max))
  (insert "(mount.core/start)")
  (cider-repl-return))

(defun frontend ()
  (interactive)
  (goto-char (point-max))
  (insert "(do (require 'figwheel.main) (figwheel.main/start :dev))")
  (cider-repl-return))

;; Stop customize from writing to this file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
 
(provide 'init)

;;; init.el ends here
