(require 'flycheck)

(defun typescript-indentation ()
  (setq typescript-indent-level 2)
  (electric-indent-mode -1))

(use-package typescript-mode
  :ensure t)

(use-package add-node-modules-path
  :hook (typescript-mode web-mode))

(use-package web-mode
    :init 
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (typescript-indentation)
    :bind (:map web-mode-map ("TAB" . #'typescript-indent-line))
    :config 
    (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package tide
  :init (typescript-indentation)
  :after (typescript-mode company flycheck eldoc)
  :hook ((typescript-mode . tide-setup)
         (web-mode . tide-setup))
  :config 
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint))

(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (apheleia-global-mode t))
