(require 'flycheck)

(use-package typescript-mode
  :ensure t)


(use-package add-node-modules-path)

(use-package web-mode
    :init (progn
            (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
            (setq typescript-indent-level 2))
    :ensure t
    :bind (:map web-mode-map ("TAB" . #'typescript-indent-line))
    :config (electric-indent-mode -1))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck eldoc)
  :hook ((typescript-mode . tide-setup)
         (web-mode . tide-setup)
         (web-mode . add-node-modules-path)
         (typescript-mode . tide-hl-identifier-mode))
  :config (flycheck-add-next-checker 'tsx-tide 'javascript-eslint))



(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (apheleia-global-mode t))

;; (add-hook 'web-mode-hook #'add-node-modules-path)
(flycheck-add-mode 'typescript-tslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)


