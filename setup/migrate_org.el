#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp -*-
(load (concat (file-name-directory load-file-name)
              "emacs-script-common.el") nil t)

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
    (let ((file-buf (find-file (expand-file-name file))))
      (with-current-buffer file-buf
        (let* ((output-file
                (expand-file-name
                 (concat
                  output-dir "/"
                  (replace-regexp-in-string
                   (regexp-quote
                    (let ((common-root output-dir))
                      (while (not
                              (string-match-p (regexp-quote common-root) file))
                        (setq common-root (file-name-directory common-root)))
                      common-root))
                   "" file))))
               (out-tilde-file (concat output-file "~")))
          (if (not (file-exists-p file))
              (progn
                (when (file-exists-p output-file) (delete-file output-file))
                (print-stdout "%s => %s" (file-relative-name file input-dir)
                              (file-relative-name output-file input-dir)))
            (setq org-publish-project-alist
                  (list
                   (list
                    "org"
                    ;; read org source from "org/" subdirectory
                    :base-directory (file-name-directory file)
                    ;; only read org files
                    :base-extension "org"
                    :recursive t
                    ;; publish html to given directory
                    :publishing-directory
                    (file-name-directory output-file)
                    :publishing-function #'org-html-publish-to-html)))
            (org-publish-current-file t)
            (print-stdout "%s => %s" (file-relative-name file input-dir)
                          (file-relative-name output-file input-dir)))
          (when (file-exists-p out-tilde-file) (delete-file out-tilde-file))))
      (kill-buffer file-buf))))

(let ((htmlize-link (car argv))
      (input-dir (expand-file-name (car (cdr argv))))
      (output-dir (expand-file-name (car (cddr argv))))
      (file-list (mapcar #'expand-file-name (nthcdr 3 argv))))
  (load-file-link htmlize-link)
  (require 'htmlize)
  (mapcar #'publish-org-file-no-cache file-list))
(kill-emacs 0)
