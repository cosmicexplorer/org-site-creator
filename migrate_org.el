#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp -*-
(pop argv)
(setq debug-on-error t)

(defvar this-dir (expand-file-name (file-name-directory load-file-name)))
(defvar org-proj-name "org")

(require 'ox-publish)
;;; we don't want org to use its cache here since we're using make, but these
;;; functions are buggy and throw errors anyway, even when we tell org not to
;;; use its cache
(defadvice org-publish-needed-p (around publish-always-needed activate)
  (setq ad-return-value t))
(defadvice org-publish-cache-set (around no-publish-cache-set activate)
  (setq ad-return-value nil))
(defadvice org-publish-cache-get (around no-publish-cache-get activate)
  (setq ad-return-value nil))
(defadvice org-publish-write-cache-file (around no-write-cache activate)
  (setq ad-return-value nil))

(defun publish-org-file-no-cache (file)
  (setq file (expand-file-name file))
  (org-publish-file file (car org-publish-project-alist) t))

(let* ((input-dir (expand-file-name (car argv)))
       (output-dir (expand-file-name (cadr argv)))
       (file-list (mapcar #'expand-file-name (cddr argv))))
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
  ;; if no output dir, publish whole project
  (if (not (file-exists-p output-dir))
      (progn
        (org-publish-project org-proj-name t)
        (message "%s" "Completed project build!"))
    (mapcar #'publish-org-file-no-cache file-list)))
