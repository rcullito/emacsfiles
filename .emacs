(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(origami s dash lsp-mode helm-rg company company-mode helm-projectile async helm multiple-cursors ace-window expand-region projectile paredit magit cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)


(require 'misc)


(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-install 'async)
(package-install 'helm)
(package-install 'helm-projectile)
(package-install 'helm-rg)
(package-install 'lsp-mode)
(package-install 'origami)


(defun emacs-core-keybindings (bindings)
  (mapc
   (lambda (x)
     (global-set-key (kbd (car x)) (cdr x)))
   bindings))

(defun user-clj ()
  (interactive)
  (find-file "~/rob/price_sheet/dev-resources/user.clj"))

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
   ("<f9>" . global-display-line-numbers-mode)
   ("C-o" . er/expand-region)
   ("<f7>" . paredit-wrap-square)
   ("<f8>" . paredit-wrap-curly)
   ("M-o" . ace-window)
   ("<f1>" . mc/mark-next-like-this)
   ("<f2>" . mc/mark-all-like-this)
   ("C-c j" . user-clj)))

(projectile-add-known-project "~/rob/price_sheet")

(helm-projectile-on)
(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
(global-set-key (kbd "C-c p h") 'helm-projectile)
(global-set-key (kbd "C-c p s r") 'helm-projectile-rg)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c s") 'projectile-switch-project)

(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(global-auto-revert-mode)

(add-hook 'clojure-mode-hook 'lsp)

(add-hook 'origami-mode-hook
          (lambda ()
	    (define-key origami-mode-map (kbd "M-p") #'origami-toggle-node)
	    (define-key origami-mode-map (kbd "M-l") #'origami-close-all-nodes)))

(add-hook 'cider-mode-hook
          (lambda ()
	    (define-key cider-repl-mode-map  (kbd "<up>") #'cider-repl-backward-input)
	    (define-key cider-repl-mode-map  (kbd "<down>") #'cider-repl-forward-input)))
