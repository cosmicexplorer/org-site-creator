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
                             (cond ((eq do-export-email 'y) email-str)
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

(defun html-head-format-string ()
  (concat
   (if (hl-css)
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />\n"
     "%s")
   "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />\n"
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
  (expand-file-name (concat output-dir "/styles/tomorrow-night-bright.css")))

(defvar sitemap-filename "sitemap.org")

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
                      :html5-fancy t
                      :tex t
                      :html-scripts t
                      :html-style t
                      :html-head (format (html-head-format-string)
                                         (if (hl-css)
                                             (file-relative-name
                                              (hljs-css)
                                              (file-name-directory output-file))
                                           "")
                                         (file-relative-name
                                          (org-info-js-css)
                                          (file-name-directory output-file))
                                         (file-relative-name
                                          (expand-file-name
                                           (concat
                                            output-dir "/scripts/out.js"))
                                          (file-name-directory output-file))
                                         ;; add and initialize highlightjs
                                         (file-relative-name
                                          (bundle-js)
                                          (file-name-directory output-file))
                                         highlightjs-init)
                      :infojs-opt
                      (format infojs-opts-format-string
                              (file-relative-name
                               (org-info-js-js)
                               (file-name-directory output-file))
                              (file-name-nondirectory output-file)
                              (file-name-nondirectory output-file))
                      :html-container "div"
                      :html-doctype "xhtml-strict"
                      :auto-sitemap t
                      :sitemap-filename sitemap-filename
                      ;; TODO: make up point to site map
                      :sitemap-title "Site Map")))
              (org-publish-current-file t)  ; goes to output-file
              (let ((sitemap-in
                     (expand-file-name (concat input-dir "/" sitemap-filename)))
                    (sitemap-out
                     (expand-file-name
                      (concat
                       (file-name-sans-extension
                        (concat output-dir "/" sitemap-filename))
                       ".html"))))
                (when (or (not (file-exists-p sitemap-out))
                          (file-newer-than-file-p file sitemap-out))
                  (print-stdout "generated => %s"
                                (file-relative-name sitemap-in build-dir))
                  (org-publish-org-sitemap (car org-publish-project-alist))
                  (let ((sitemap-tilde (concat sitemap-in "~")))
                    (when (file-exists-p sitemap-tilde)
                      (delete-file sitemap-tilde)))
                  (print-stdout "%s => %s"
                                (file-relative-name sitemap-in build-dir)
                                (file-relative-name sitemap-out build-dir))
                  (org-publish-file sitemap-in)
                  (delete-file sitemap-in)))
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

(let ((htmlize-link (car argv))
      (input-dir (expand-file-name (car (cdr argv))))
      (output-dir (expand-file-name (car (cddr argv))))
      (do-export-email (intern (car (nthcdr 3 argv))))
      (do-highlight-css (intern (car (nthcdr 4 argv))))
      (file-list (mapcar #'expand-file-name (nthcdr 5 argv))))
  (load-file-link htmlize-link)
  (require 'htmlize)
  (mapcar #'publish-org-file-no-cache file-list))
(kill-emacs 0)
