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
(straight-use-package 'nyan-mode)
(straight-use-package 'terraform-mode)
(straight-use-package 'helm-rg)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-clj-kondo)
(straight-use-package 'deadgrep)
(straight-use-package 'kotlin-mode)
(straight-use-package 'pyenv-mode)
(straight-use-package 'elpy)
(straight-use-package 'request-deferred)
(straight-use-package 'request)
(straight-use-package 'yaml-mode)
(straight-use-package 'which-key)
(straight-use-package 'solidity-mode)
(straight-use-package 'smooth-scrolling)
(straight-use-package 'smex)
(straight-use-package 'slime)
(straight-use-package 'slim-mode)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'qml-mode)
(straight-use-package 'neotree)
(straight-use-package 'markdown-preview-mode)
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


;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
(setq inhibit-splash-screen t
      initial-scratch-message "")
(setq make-backup-files nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; magit custom code
(defun my-magit-hook ()
  (define-key magit-mode-map
    (kbd "Q")
    'quick-commit-and-push))

(add-hook 'magit-mode-hook 'my-magit-hook)

;; this helper fn will properly handle
;; will prompt to stage automatically if anything is still unstaged
;; will let you know if there have been no changes at all
(defun quick-commit-and-push (commit-message)
  (interactive "sEnter your commit message: ")
  (cond 
   ((magit-anything-unstaged-p)
    (when (y-or-n-p "Stage and commit all unstaged changes? ")
      (progn (magit-run-git "add" "-u" ".")
             (quick-commit-and-push commit-message))))
   ((magit-anything-staged-p)
    (when (magit-git-success "commit" "-m" commit-message)
      (progn (message "commmited!")
             ;;(magit-refresh-buffer)
             (magit-push-current-to-upstream nil))))
   (t
    (user-error "No changes since last commit"))))
