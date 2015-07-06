(pop argv)

(setq debug-on-error t)

(defun print-stdout (fmt-string &rest args)
  (princ (apply #'format (cons (concat fmt-string "\n") args))))

(defun load-file-link (file)
  (add-to-list 'load-path (expand-file-name (file-name-directory file))))

(defvar transform-file-links-binary
  (concat (file-name-directory load-file-name) "transform-file-links.pl"))

(defun format-links-in-region (arg beg end &optional filename)
  "Formats links in a given buffer according to a perl script."
  (apply
   #'call-process-region
   (append
    (list beg end transform-file-links-binary t t nil arg)
    (if filename (list filename) nil))))
