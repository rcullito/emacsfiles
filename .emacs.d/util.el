(defvar next-buffer-count 2)

(defun earmuff (var-name)
  (concat "*" var-name "*"))

(defun new-scratch ()
  (interactive)
  (let* ((buffer-num (number-to-string next-buffer-count))
         (buffer-name (earmuff (concat "scratch-" buffer-num))))
    (progn
      (setq next-buffer-count (+ next-buffer-count 1))
      (switch-to-buffer buffer-name))))


(defun just-no-space ()
  (interactive)
  (setq current-prefix-arg '(0)) ; C-u
  (call-interactively 'just-one-space))


(defmacro emacs-core-keybindings (bindings)
  (let ((g (cl-gensym)))
    `(mapc
      (lambda (,g)
        (global-set-key (kbd (car ,g)) (cdr ,g)))
      ,bindings)))
