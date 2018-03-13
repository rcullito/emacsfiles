(require 'package) 
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-window solidity-mode ## slim-mode slime exwm ensime scala-mode clj-refactor yaml-mode markdown-mode+ markdown-preview-mode markdown-mode which-key multiple-cursors highlight-parentheses cider clojure-mode-extra-font-locking smex clojure-mode paredit ido-ubiquitous helm-projectile company rainbow-delimiters projectile helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; modes
(which-key-mode)
(projectile-mode)

;; writers workshop
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 110)

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

;; global keys
(global-set-key (kbd "<f1>") 'mc/mark-next-like-this)
(global-set-key (kbd "<f2>") 'mc/mark-all-like-this)
(global-set-key (kbd "<f3>") 'clojure-thread-first-all)
(global-set-key (kbd "<f4>") 'clojure-thread-last-all)
(global-set-key (kbd "<f5>") 'global-linum-mode)
(global-set-key (kbd "<f6>") 'find-grep-dired)
(global-set-key (kbd "<f7>") #'paredit-wrap-square)
(global-set-key (kbd "<f8>") #'paredit-wrap-curly)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Misc
(global-prettify-symbols-mode 1)
(require 'helm-config)

;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/local/Cellar/sbcl/1.4.3/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(setq cider-cljs-lein-repl
    "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")

(global-set-key (kbd "M-o") 'ace-window)
