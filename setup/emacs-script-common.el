(pop argv)

(setq debug-on-error t)

(defun print-stdout (fmt-string &rest args)
  (princ (apply #'format (cons (concat fmt-string "\n") args))))

(defun load-file-link (file)
  (add-to-list 'load-path (expand-file-name (file-name-directory file))))
