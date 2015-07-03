#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp -*-
(pop argv)
(setq debug-on-error t)

(defvar this-dir (concat (expand-file-name
                          (file-name-directory load-file-name))))

(defvar org-proj-name "org")

(require 'ox-publish)
(let ((input-dir (concat this-dir (car argv)))
      (output-dir (cadr argv))
      (single-file (let (res) (car (cddr argv))
                        (and res (expand-file-name res)))))
  (setq org-publish-project-alist
        (list
         (list
          org-proj-name
          ;; read org source from "org/" subdirectory
          :base-directory input-dir
          ;; only read org files
          :base-extension "org"
          ;; publish html to given directory
          :publishing-directory output-dir
          :publishing-function #'org-html-publish-to-html)))
  (if (and single-file (file-exists-p output-dir))
      (progn
        (org-html-publish-to-html
         (car org-publish-project-alist) single-file
         (file-name-directory
	   (concat output-dir
		   (and (string-match-p (regexp-quote input-dir) single-file)
			(substring single-file (match-end 0))))))
        (org-publish-update-timestamp
         single-file output-dir #'org-html-publish-to-html input-dir))
    (org-publish-project org-proj-name t)))

(message "%s" "Done. Goodbye!")
(kill-emacs 0)
