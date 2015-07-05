(pop argv)

(setq debug-on-error t)

(defun print-stdout (fmt-string &rest args)
  (princ (apply #'format (cons (concat fmt-string "\n") args))))

(defun load-file-link (file)
  (add-to-list 'load-path (expand-file-name (file-name-directory file))))

(defun format-links-in-file (arg &rest files)
  "Formats links in a given html file according to a perl script."
  (apply
   #'call-process
   (append
    (list
     (concat (file-name-directory load-file-name) "transform-file-links.pl")
     nil nil nil arg)
    files)))
