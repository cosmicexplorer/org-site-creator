#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp -*-
(pop argv)
(setq debug-on-error t)

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
(defadvice org-publish-cache-file-needs-publishing (around pub activate)
  (setq ad-return-value))
(defun org-font-lock-ensure () (font-lock-fontify-buffer))

(defvar my-org-timestamp-format "%Y-%m-%d@%H:%M:%S")

(defun obscure-email-format-string (str class)
  (concat "<a class=\"" class "\">" str "</a>"))

(defvar emacs-url "http://gnu.org/software/emacs/")
(defun emacs-version-string ()
  (concat "<a href=\"" emacs-url "\">Emacs</a> " emacs-version))
(defvar org-url "http://orgmode.org")
(defun org-version-string ()
  (concat "<a href=\"" org-url "\">Org</a> " (org-version)))

(defadvice org-html-format-spec (around add-my-format-chars activate)
  (let* ((info (ad-get-arg 0))
         (res ad-do-it))
    (setq ad-return-value
          (append res
                  `((?D . ,(let ((file (plist-get info :input-file)))
                             (format-time-string
                              my-org-timestamp-format
                              (if file (nth 5 (file-attributes file))
                                (current-time)))))
                    (?E . ,(obscure-email-format-string
                            (plist-get info :email)
                            "format_eval"))
                    (?f . ,(let ((file (plist-get info :input-file)))
                             (file-name-sans-extension
                              (file-name-nondirectory file))))
                    (?F . ,(plist-get info :input-file))
                    (?o . ,(org-version-string))
                    (?V . ,(emacs-version-string)))))))

(setq org-html-postamble-format
      (list
       (list
        "en"
        (concat
         "<table><tr>"
         "<td class=\"author\">%a</td>"
         "<td class=\"source\"><a href=\"%f.org.html\">"
         "See Org Source</a></td>"
         "<td class=\"date\">Date: %D</td>"
         "</tr><tr>"
         "<td class=\"email\">%E</td>"
         "<td></td>"
         "<td class=\"creator\">%V, %o</td>"
         "</tr></table>"))))

(defun publish-org-file-no-cache (file)
  (if (string-match-p "\\(.*/\\)?\\.?#" file) nil
    (let* ((output-file
            (file-relative-name
             (concat
              output-dir
              (replace-regexp-in-string
               "\\.org$" ".html"
               (replace-regexp-in-string
                (concat "^" (regexp-quote (expand-file-name input-dir)))
                ""
                (expand-file-name file))))))
           (out-tilde-file (concat output-file "~")))
      (setq org-notes-plist
            (list
             "org-notes"
             ;; read org source from "org/" subdirectory
             :base-directory input-dir
             ;; only read org files
             :base-extension "org"
             ;; publish html to given directory
             :publishing-directory output-dir
             :publishing-function #'org-html-publish-to-html)

            org-static-plist
            (list
             "org-static"
             :base-directory input-dir
             :base-extension "css\\|js"
             :publishing-directory output-dir
             :publishing-function #'org-publish-attachment))
      (if (not (file-exists-p file))
          (progn
            (when (file-exists-p output-file) (delete-file output-file))
            (message "deleted [%s => %s]" file output-file))
        (org-publish-file
         (expand-file-name file)
         (if (string-match-p "\\.org" file) org-notes-plist org-static-plist)
         t)
        (message "%s => %s" file output-file))
      (when (file-exists-p out-tilde-file) (delete-file out-tilde-file)))))

(let ((htmlize-link (car argv))
      (input-dir (cadr argv))
      (output-dir (car (cddr argv)))
      (file-list (nthcdr 3 argv)))
  ;; this doesn't come by default
  (load (expand-file-name (concat default-directory htmlize-link)) nil t)
  (mapcar #'publish-org-file-no-cache file-list))
(kill-emacs 0)
