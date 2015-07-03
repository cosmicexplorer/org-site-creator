#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp -*-
(pop argv)
(setq debug-on-error t)

(defvar org-proj-name "org")

(require 'ox-publish)
(let ((htmlize-link (car argv))
      (input-dir (cadr argv))
      (output-dir (car (cddr argv))))
  (load-file
   (expand-file-name (concat default-directory htmlize-link)))
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
  (org-publish-project org-proj-name)
  (message "%s" "Completed project build!"))
(kill-emacs 0)
