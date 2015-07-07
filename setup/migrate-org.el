#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp -*-
(load (concat (file-name-directory load-file-name)
              "emacs-script-common.el") nil t)

(setq build-dir default-directory)

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
                    (?E . ,(let ((email-str (plist-get info :email)))
                             (cond ((eq do-export-email 'y)
                                    (concat
                                     "<a href=\"mailto:" email-str "\">"
                                     email-str "</a>"))
                                   ((eq do-export-email 'f)
                                    (obscure-email-format-string
                                     email-str "format_eval"))
                                   (t ""))))
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
        ;; couldn't figure out how to do this with divs. sue me.
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

(defvar highlightjs-init "require('highlight.js').initHighlightingOnLoad();")

(defun hl-css () (eq do-highlight-css 'y))

(defun do-org-info-string () (eq do-org-info 'y))

(defun html-head-format-string ()
  (concat
   (if (hl-css)
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />\n"
     "%s")
   (if (do-org-info-string)
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />\n"
     "%s")
   "<script type=\"text/javascript\" src=\"%s\"></script>\n"
   "<script type=\"text/javascript\" src=\"%s\"></script>\n"
   "<script type=\"text/javascript\">%s</script>\n"))

(defvar infojs-opts-format-string
  (concat "path:%s toc:t ltoc:above view:info mouse:underline buttons:t "
          "up:%s home:%s"))

(defvar build-dir (file-name-nondirectory load-file-name))
(defun org-info-js-dir ()
  (expand-file-name (concat output-dir "/org-info-js/")))
(defun org-info-js-js ()
  (expand-file-name (concat (org-info-js-dir) "org-info-mini.js")))
(defun org-info-js-css ()
  (expand-file-name (concat (org-info-js-dir) "stylesheet-mini.css")))

(defun bundle-js ()
  (expand-file-name (concat output-dir "/scripts/bundle.js")))

(defun hljs-css ()
  (expand-file-name (concat output-dir "/styles/tomorrow.css")))

(defvar sitemap-filename "sitemap.org")

(defun re-seq (regexp string &optional num)
  "list of all regexp matches in a string"
  (unless num (setq num 0))
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string num string) matches)
        (setq pos (match-end num)))
      matches)))

;;; requiring cl may be silly for a single thing, but this script is only called
;;; once per make, so it's not too much of an issue
(require 'cl)
(defadvice org-publish-get-base-files (around match-whole-regexp activate)
  (let ((res ad-do-it))
    (remove-if
     (lambda (file) (string-match-p (ad-get-arg 1) (expand-file-name file)))
     res)))

(defun setup-org-proj-alist (&optional buf-file-name)
  (setq org-publish-project-alist
        (list
         (list
          "org"
          ;; read org source from "org/" subdirectory
          :base-directory input-dir
          ;; only read org files
          :base-extension "org"
          :recursive t
          ;; publish html to given directory
          :publishing-directory output-dir
          :publishing-function #'org-html-publish-to-html
          :html-preamble t
          :html-postamble t
          :html-html5-fancy t
          :tex t
          ;; let's not include the org files in this repo
          ;; this is a hack but i was unsuccessful in trying to narrow it down
          ;; more, so if you have a subfolder named org-info-js, you're out of
          ;; luck
          :exclude "org\\-info\\-js"
          :html-scripts t
          :html-style t
          :html-head
          (let* ((htmlhead-regexp "^#\\+HTML_HEAD:\\( .*\\)")
                 (htmlhead-additions
                  (mapconcat
                   #'identity (re-seq htmlhead-regexp
                                      (buffer-string) 1) " ")))
            (goto-char (point-min))
            (while (re-search-forward htmlhead-regexp nil t)
              (beginning-of-line)
              (delete-region (line-beginning-position)
                             (line-end-position)))
            (concat htmlhead-additions " "
                    (format (html-head-format-string)
                            (if (hl-css)
                                (file-relative-name
                                 (hljs-css)
                                 (file-name-directory output-file))
                              "")
                            (if (do-org-info-string)
                                (file-relative-name
                                 (org-info-js-css)
                                 (file-name-directory output-file))
                              "")
                            (file-relative-name
                             (expand-file-name
                              (concat
                               output-dir "/scripts/out.js"))
                             (file-name-directory output-file))
                            ;; add and initialize highlightjs
                            (file-relative-name
                             (bundle-js)
                             (file-name-directory output-file))
                            highlightjs-init)))
          :infojs-opt
          (let ((indexfile (expand-file-name
                            (concat input-dir "/"
                                    "index.org")))
                (index-htmlfile
                 (expand-file-name (concat output-dir "/"
                                           "index.html")))
                (infojs-opt-additions
                 (mapconcat
                  #'identity
                  (re-seq "^#\\+INFOJS_OPTS:\\( .*\\)"
                          (buffer-string) 1) " ")))
            (concat
             ;; add #+INFOJS_OPTS: to preempt given options
             infojs-opt-additions " "
             (format infojs-opts-format-string
                     (file-relative-name
                      (org-info-js-js)
                      (file-name-directory output-file))
                     (if (file-exists-p indexfile)
                         (file-relative-name
                          index-htmlfile
                          (file-name-directory output-file))
                       (file-name-nondirectory (or (buffer-file-name)
                                                   buf-file-name)))
                     (let ((sitemap-out
                            (concat
                             (file-name-sans-extension sitemap-filename)
                             ".html")))
                       (if (file-exists-p sitemap-filename)
                           (file-relative-name
                            sitemap-out (file-name-directory output-file))
                         (file-name-nondirectory
                          (or
                           (buffer-file-name)
                           buf-file-name)))))))
          :html-container "div"
          :html-doctype "xhtml-strict"
          :auto-sitemap t
          :sitemap-filename sitemap-filename
          :sitemap-title "Site Map"))))

(defun publish-org-file-no-cache (file)
  (let ((file-buf (find-file (expand-file-name file))))
    (with-current-buffer file-buf
      (let* ((output-file
              (expand-file-name
               (concat
                (file-name-sans-extension
                 (concat
                  output-dir "/"
                  (replace-regexp-in-string
                   (regexp-quote input-dir)
                   "" file)))
                ".html")))
             (out-tilde-file (concat output-file "~")))
        (if (or (file-newer-than-file-p file output-file)
                (file-newer-than-file-p load-file-name file)
                (file-newer-than-file-p transform-file-links-binary file))
            (progn

              (goto-char (point-min))
              (while (re-search-forward "^#\\+SETUPFILE: \\(.+\\)" nil t)
                (let ((insert-str (match-string 1)))
                  (beginning-of-line)
                  (delete-region (point) (line-end-position))
                  (insert
                   (let* ((buf (find-file insert-str))
                          (str
                           (with-current-buffer buf (buffer-string))))
                     (kill-buffer buf)
                     str))))

              (setup-org-proj-alist)

              ;; TODO: make better highlight css that actually works with the
              ;; theme we've got going

              (org-publish-current-file t)  ; goes to output-file
              (kill-buffer
               (with-current-buffer (find-file output-file)
                 (format-links-in-region "y" (point-min) (point-max)
                                         output-file)
                 (save-buffer)
                 (current-buffer)))
              (print-stdout "%s => %s"
                            (file-relative-name file build-dir)
                            (file-relative-name output-file build-dir)))
          (call-process "touch" nil nil nil output-file))
        (when (file-exists-p out-tilde-file) (delete-file out-tilde-file))))
    (kill-buffer file-buf)))

(defun or-fun (a b)
  (or a b))

(let ((htmlize-link (car argv))
      (input-dir (expand-file-name (car (cdr argv))))
      (output-dir (expand-file-name (car (cddr argv))))
      (do-export-email (intern (car (nthcdr 3 argv))))
      (do-highlight-css (intern (car (nthcdr 4 argv))))
      (do-org-info (intern (car (nthcdr 5 argv))))
      (file-list (mapcar #'expand-file-name (nthcdr 6 argv))))
  (load-file-link htmlize-link)
  (require 'htmlize)
  (let ((sitemap-in
         (expand-file-name (concat input-dir "/" sitemap-filename)))
        (sitemap-out
         (expand-file-name
          (concat
           (file-name-sans-extension
            (concat output-dir "/" sitemap-filename))
           ".html"))))
    (mapcar #'publish-org-file-no-cache file-list)
    (print-stdout "generated => %s"
                  (file-relative-name sitemap-in build-dir))
    (let* ((output-file
            (expand-file-name
             (concat
              (file-name-sans-extension
               (concat
                output-dir "/"
                (replace-regexp-in-string
                 (regexp-quote input-dir)
                 "" file)))
              ".html")))
           (out-tilde-file (concat output-file "~"))
           (file sitemap-in))
      (setup-org-proj-alist sitemap-in)
      (org-publish-org-sitemap (car org-publish-project-alist)))
    (let ((sitemap-tilde (concat sitemap-in "~")))
      (when (file-exists-p sitemap-tilde)
        (delete-file sitemap-tilde)))
    (publish-org-file-no-cache sitemap-in)))
(kill-emacs 0)
