(setq lexical-binding t)

(let ((next-buffer-count 2))
  (defun new-scratch ()
   (interactive)
   (let ((buffer-name (format "*scratch-%d*" next-buffer-count)))
     (cl-incf next-buffer-count)
     (switch-to-buffer buffer-name))))

(defun just-no-space ()
  (interactive)
  (setq current-prefix-arg '(0)) ; C-u
  (call-interactively 'just-one-space))


(defun emacs-core-keybindings (bindings)
  (mapc
   (lambda (x)
     (global-set-key (kbd (car x)) (cdr x)))
   bindings))

;; On Lisp, Paul Graham pg 169, Generalized Variables
;; adapted slightly for emacs lisp and without additional with-genysms macro
(defmacro allf (val &rest args)
  (let ((gval (cl-gensym)))
    `(let ((,gval ,val))
       (setf ,@(cl-mapcan #'(lambda (a) (list a gval))
                          args)))))

(defmacro nilf (&rest args) `(allf nil ,@args))

