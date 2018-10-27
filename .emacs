(require 'package) 
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar rcullito/packages '(neotree
 qml-mode
 helm-ag
 ag
 x
 magit
 less-css-mode
 ace-window
 solidity-mode
 slim-mode slime
 ensime
 scala-mode
 clj-refactor
 yaml-mode
 markdown-mode+
 markdown-preview-mode
 markdown-mode
 which-key
 multiple-cursors
 highlight-parentheses
 clojure-mode-extra-font-locking
 smex clojure-mode
 paredit
 ido-ubiquitous
 helm-projectile
 company
 rainbow-delimiters
 projectile
 helm)
  "Default packages")

;; forked from http://aaronbedra.com/emacs.d/
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
   rcullito/packages))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; modes
(which-key-mode)
(projectile-mode)

(add-hook 'after-init-hook
	  'global-company-mode)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1))

(add-hook 'clojure-mode-hook 'enable-paredit-mode #'my-clojure-mode-hook)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

;; emacs.d stuff
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/customizations")
(load-theme 'zenburn t)
(load "ui.el")

;; fn keys
(global-set-key (kbd "<f1>") 'mc/mark-next-like-this)
(global-set-key (kbd "<f2>") 'mc/mark-all-like-this)
(global-set-key (kbd "<f3>") 'clojure-thread-first-all)
(global-set-key (kbd "<f4>") 'clojure-thread-last-all)
(global-set-key (kbd "<f5>") 'global-linum-mode)
(global-set-key (kbd "<f6>") 'find-grep-dired)
(global-set-key (kbd "<f7>") #'paredit-wrap-square)
(global-set-key (kbd "<f8>") #'paredit-wrap-curly)
(global-set-key (kbd "<f9>") 'cider-scratch)
(global-set-key (kbd "<f10>") 'transpose-sexps)

;; Misc
(global-prettify-symbols-mode 1)
(require 'helm-config)
(add-to-list 'load-path "~/rob/cider")
(require 'cider)
(put 'narrow-to-region 'disabled nil)

;; other global keys
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-c p s a") 'helm-projectile-ag)
(global-set-key (kbd "C-c M-h") 'custom-cider-jack-in)
(global-set-key (kbd "C-c M-y") 'start-figwheel-cljs-repl)
(global-set-key (kbd "C-c l") 'just-no-space)
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key (kbd "M-x") 'helm-M-x)


;; rcullito fns
(defun custom-cider-jack-in ()
  (interactive)
  (let ((status-desktop-params "with-profile +figwheel repl"))
    (set-variable 'cider-lein-parameters status-desktop-params)
    (message "setting 'cider-lein-parameters")
    (cider-jack-in)))

(defun start-figwheel-cljs-repl ()
  (interactive)
  (set-buffer "*cider-repl status-react*")
  (goto-char (point-max))
  (insert "(do (use 'figwheel-api)
           (start [:desktop])
           (start-cljs-repl))")
  (cider-repl-return))

(defun just-no-space ()
  (interactive)
  (setq current-prefix-arg '(0)) ; C-u
  (call-interactively 'just-one-space))

