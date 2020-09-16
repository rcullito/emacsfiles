(require 'package)
(setq byte-compile-warnings '(cl-functions))
(require 'cl)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq user-full-name "Rob Culliton")
(setq user-mail-address "rob.culliton@gmail.com")

(defvar rcullito/packages '(neotree
                            qml-mode
                            flycheck
                            flycheck-clj-kondo
                            deadgrep
 helm-rg
 ag
 magit
 less-css-mode
 ace-window
 solidity-mode
 slim-mode slime
 scala-mode
 yaml-mode
 cider
 cmake-mode
 clj-refactor
 markdown-mode+
 markdown-preview-mode
 markdown-mode
 which-key
 multiple-cursors
 highlight-parentheses
 clojure-mode-extra-font-locking
 smex clojure-mode
 paredit
 helm-projectile
 company
 rainbow-delimiters
 projectile
 helm)
  "Default packages")

;; taken from http://aaronbedra.com/emacs.d/
(defun rcullito/packages-installed-p ()
  (loop for pkg in rcullito/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (rcullito/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg rcullito/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(terraform-mode helm-rg flycheck-clj-kondo flycheck deadgrep kotlin-mode pyenv-mode elpy request-deferred request restclient yaml-mode which-key solidity-mode smooth-scrolling smex slime slim-mode rainbow-delimiters qml-mode neotree markdown-preview-mode markdown-mode+ magit less-css-mode highlight-parentheses helm-projectile cmake-mode clojure-mode-extra-font-locking clj-refactor ag ace-window))
 '(safe-local-variable-values
   '((cider-figwheel-main-default-options . "dev")
     (cider-default-cljs-repl . figwheel-main))))

;; modes
(which-key-mode)
(projectile-mode)
(global-auto-revert-mode)

(setq-default indent-tabs-mode nil)
(add-hook 'after-init-hook
	  'global-company-mode)



(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-clj-kondo)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1))


(add-hook 'clojure-mode-hook 'enable-paredit-mode #'my-clojure-mode-hook)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

;; emacs.d stuff
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; fn keys
(global-set-key (kbd "<f1>") 'mc/mark-next-like-this)
(global-set-key (kbd "<f2>") 'mc/mark-all-like-this)
(global-set-key (kbd "<f3>") 'clojure-thread-first-all)
(global-set-key (kbd "<f4>") 'clojure-thread-last-all)
(global-set-key (kbd "<f5>") 'global-linum-mode)
(global-set-key (kbd "<f6>") #'deadgrep)
(global-set-key (kbd "<f7>") #'paredit-wrap-square)
(global-set-key (kbd "<f9>") 'cider-scratch)
(global-set-key (kbd "<f10>") 'transpose-sexps)

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
(global-set-key (kbd "C-c M-y") 'start-mount)
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

(defun start-mount ()
  (interactive)
  (goto-char (point-max))
  (insert "(mount.core/start)")
  (cider-repl-return))

