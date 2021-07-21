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



(setq global-key-pairs
  '(("C--" . 'undo)
    ("C-c l" . 'just-no-space)
    ("C-c u" . 'delete-indentation)
    ("\C-x\C-b" . 'buffer-menu)
    ("C-c i" . 'indent-region)))


(defmacro set-key-pairs ()
  `(progn ,@(mapcar
             (lambda (pair)
               `(global-set-key (kbd ,(car pair)) ,(cdr pair)))
             global-key-pairs)))

(set-key-pairs)
