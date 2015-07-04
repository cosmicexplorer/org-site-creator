#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp -*-
(pop argv)
(setq debug-on-error t)

(defun htmlize-this-file (file)
  (let ((outfile (concat file ".html")))
    (message "%s => %s" file outfile)
    (htmlize-file file outfile)))

(let ((htmlize-link (car argv))
      (input-files (cdr argv)))
  (load (if (file-name-absolute-p htmlize-link) htmlize-link
          (expand-file-name (concat default-directory htmlize-link)))
        nil t)
  (mapcar #'htmlize-this-file input-files))
(kill-emacs 0)
