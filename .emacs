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
(setq straight-use-package-by-default t)

(setq user-full-name "Rob Culliton")
(setq user-mail-address "rob.culliton@gmail.com")

(straight-use-package 'helm)
(straight-use-package 'projectile)
(straight-use-package 'ht)
(straight-use-package 'browse-kill-ring)
(straight-use-package 'terraform-mode)
(straight-use-package 'helm-rg)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-clj-kondo)
(straight-use-package 'deadgrep)
(straight-use-package 'yaml-mode)
(straight-use-package 'which-key)
(straight-use-package 'smooth-scrolling)
(straight-use-package 'slime)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'neotree)
(straight-use-package 'magit)
(straight-use-package 'highlight-parentheses)
(straight-use-package 'helm-projectile)
(straight-use-package 'clojure-mode)
(straight-use-package 'clojure-mode-extra-font-locking)
(straight-use-package 'clj-refactor)
(straight-use-package 'ace-window)
(straight-use-package 'cider)
(straight-use-package 'multiple-cursors)
(straight-use-package 'paredit) 

(straight-use-package
 '(el-patch :type git :host github :repo "Guaranteed-Rate/guaranteed-emacs"))
(require 'guaranteed-emacs)
(set-common-vars)
(setenv "PROCESS_QUEUES" "true")

(menu-bar-mode -1)

;; modes
(which-key-mode)
(projectile-mode)
(global-auto-revert-mode)

(setq-default indent-tabs-mode nil)
(add-hook 'after-init-hook
	  'global-company-mode)


(require 'flycheck-clj-kondo)
(require 'smooth-scrolling)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (idle-highlight-mode t)
  (smooth-scrolling-mode 1)
  (paredit-mode t))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'clojure-mode-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

;; emacs.d stuff
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; fn keys
(global-set-key (kbd "<f1>") 'mc/mark-next-like-this)
(global-set-key (kbd "<f2>") 'mc/mark-all-like-this)
(global-set-key (kbd "<f3>") 'clojure-thread-first-all)
(global-set-key (kbd "<f4>") 'backward-paragraph)
(global-set-key (kbd "<f5>") 'forward-paragraph)
(global-set-key (kbd "<f6>") #'deadgrep)
(global-set-key (kbd "<f7>") #'paredit-wrap-square)
(global-set-key (kbd "<f8>") #'paredit-wrap-curly)
(global-set-key (kbd "<f9>") 'global-linum-mode)

;; Misc
(global-prettify-symbols-mode 1)
(put 'narrow-to-region 'disabled nil)

;; 11/8/2018
(require 'helm-projectile)
(helm-projectile-on)

;; other global keys
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-c s") 'projectile-switch-project)
(global-set-key (kbd "C-c d") 'helm-projectile-rg)
(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
(global-set-key (kbd "C-c p h") 'helm-projectile)
(global-set-key (kbd "C-c l") 'just-no-space)
(global-set-key (kbd "C-c u") 'delete-indentation)
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key "\M-/" 'hippie-expand)

(defun just-no-space ()
  (interactive)
  (setq current-prefix-arg '(0)) ; C-u
  (call-interactively 'just-one-space))

(setq inhibit-splash-screen t
      initial-scratch-message "")
(setq make-backup-files nil)


