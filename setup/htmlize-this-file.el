(load (concat (file-name-directory load-file-name)
              "emacs-script-common.el") nil t)

(setq enable-local-variables nil)

(defun add-header ()
  (goto-char (point-min))
  (insert
   (format
    "%s %%%% %s %s %%%% %s\n"
    (or (and (boundp 'comment-start) comment-start) "")
    "This file was generated from"
    (propertize
     (file-name-nondirectory (buffer-file-name))
     'htmlize-link
     (list :uri (file-name-nondirectory (buffer-file-name))))
    (or (and (boundp 'comment-end) comment-end) ""))))

(defvar this-dir (file-name-directory load-file-name))

;;; TODO: remove this hack
(defun makefile-p (file)
  (string-equal (file-name-sans-extension (file-name-nondirectory file))
                "Makefile"))

(defun send-message (msg)
  (with-temp-buffer
    (insert msg "\n")
    (append-to-file (point-min) (point-max) output-file)))

(defun make-html-file-fun ()
  (kill-buffer
       (let ((buf
              (with-current-buffer (find-file file)
                (normal-mode)
                (add-header)
                (current-buffer))))
         (with-current-buffer (htmlize-buffer buf)
           (format-links-in-region "y" (point-min) (point-max) file)
           (write-file outfile)
           (current-buffer)))))

(defun make-nonhtml-file-fun ()
  (copy-file file outfile-nonhtml t t t t))

(defun htmlize-this-file (file)
  (setq file (expand-file-name file))
  (let* ((outfile-nonhtml
          (expand-file-name
           (concat
            output-dir "/"
            (replace-regexp-in-string
             (regexp-quote input-dir) "" file))))
         (outfile (concat outfile-nonhtml ".html"))
         (outfile-dir (file-name-directory outfile)))
    (unless (file-exists-p outfile-dir)
      (make-directory outfile-dir t))
    (let ((make-html-file
           (or (file-newer-than-file-p file outfile)
               (file-newer-than-file-p load-file-name file)
               (file-newer-than-file-p transform-file-links-binary file)))
          (make-nonhtml-file
           (and (not (makefile-p file))
                (not (string-equal (expand-file-name file)
                                   (expand-file-name outfile-nonhtml)))
                (or (file-newer-than-file-p file outfile-nonhtml)
                    (file-newer-than-file-p load-file-name file)
                    (file-newer-than-file-p transform-file-links-binary
                                            file)))))
      (cond ((and make-html-file make-nonhtml-file)
             (send-message
              (format "%s => [ %s %s ]"
                      (file-relative-name
                       file (expand-file-name (concat this-dir "/..")))
                      (file-relative-name
                       outfile-nonhtml (expand-file-name
                                        (concat this-dir "/..")))
                      (file-relative-name
                       outfile (expand-file-name (concat this-dir "/..")))))
             (make-html-file-fun)
             (make-nonhtml-file-fun))
            (make-html-file
             (send-message
              (format "%s => %s"
                      (file-relative-name
                       file (expand-file-name (concat this-dir "/..")))
                      (file-relative-name
                       outfile (expand-file-name (concat this-dir "/..")))))
             (make-html-file-fun))
            (make-nonhtml-file
             (send-message
              (format "%s => %s"
                      (file-relative-name
                       file (expand-file-name (concat this-dir "/..")))
                      (file-relative-name
                       outfile-nonhtml (expand-file-name
                                        (concat this-dir "/..")))))
             (make-nonhtml-file-fun))))))

(defadvice org-publish-get-project-from-filename (around ew activate)
  (setq ad-return-value (car org-publish-project-alist)))

(let* ((tmpbuf (find-file (concat this-dir "tmpfile")))
       (argv
        (split-string
         (with-current-buffer tmpbuf (buffer-string)))))
  (kill-buffer tmpbuf)
  (let ((output-file (car argv))
        (htmlize-link (cadr argv))
        (input-dir (expand-file-name (car (cddr argv))))
        (output-dir (expand-file-name (car (nthcdr 3 argv))))
        (input-files (nthcdr 4 argv)))
    (load-file-link htmlize-link)
    (require 'htmlize)
    ;; disgusting, disgusting hack
    (let ((sitemap-file (expand-file-name (concat input-dir "/" "sitemap.org"))))
      (htmlize-this-file sitemap-file))
    (mapcar #'htmlize-this-file input-files))
  (kill-emacs 0))
