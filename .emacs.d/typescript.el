(require 'flycheck)

(use-package typescript-mode
  :ensure t)

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
         (typescript-mode . tide-hl-identifier-mode)))

(flycheck-add-mode 'typescript-tslint 'web-mode)

(defun eslint-disable ()
  "disable eslint for the current file"
  (interactive)
  (goto-char 0)
  (insert "/* eslint-disable */")
  (paredit-newline)
  (save-buffer))
