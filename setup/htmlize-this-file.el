(load (concat (file-name-directory load-file-name)
              "emacs-script-common.el") nil t)

(setq enable-local-variables nil)

(defun add-header ()
  (goto-char (point-min))
  (insert
   (format
    "%s %%%% %s %s %%%%%s\n"
    (or (and (boundp 'comment-start) comment-start) "")
    "This file was generated from"
    (propertize
     (file-name-nondirectory (buffer-file-name))
     'htmlize-link
     (list :uri (format "file:%s"
                        (file-name-nondirectory (buffer-file-name)))))
    (or (and (boundp 'comment-end) comment-end) ""))))

(defvar this-dir (file-name-directory load-file-name))

;;; TODO: remove this hack
(defun makefile-p (file)
  (string-equal (file-name-sans-extension (file-name-nondirectory file))
                "Makefile"))

(let* ((tmpbuf (find-file (concat this-dir "tmpfile")))
       (argv
        (split-string
         (with-current-buffer tmpbuf (buffer-string)))))
  (kill-buffer tmpbuf)
  (defun htmlize-this-file (file)
    (setq file (expand-file-name file))
    (let ((common-root (expand-file-name output-dir)))
      (while (not (string-match-p (regexp-quote common-root) file))
        (setq common-root (expand-file-name
                           (concat common-root "/.."))))
      (let* ((outfile-nonhtml
              (expand-file-name
               (concat
                output-dir "/"
                (replace-regexp-in-string
                 (regexp-quote common-root) "" file))))
             (outfile (concat outfile-nonhtml ".html"))
             (outfile-dir (file-name-directory outfile)))
        (unless (file-exists-p outfile-dir)
          (make-directory outfile-dir))
        (with-temp-buffer
          (insert (format
                   (concat "%s => %s"
                           (cond ((or (and (file-newer-than-file-p file outfile)
                                           (file-newer-than-file-p
                                            file outfile-nonhtml))
                                      (file-newer-than-file-p
                                       load-file-name file))
                                  "{,.html}")
                                 ((file-newer-than-file-p file outfile) ".html")
                                 ((file-newer-than-file-p file outfile-nonhtml)
                                  "")
                                 (t " (already exists)"))
                           "\n")
                   (file-relative-name
                    file (expand-file-name (concat this-dir "/..")))
                   (file-relative-name
                    outfile-nonhtml
                    (expand-file-name (concat this-dir "/..")))))
          (append-to-file (point-min) (point-max) output-file))
        (when (or (file-newer-than-file-p file outfile)
                  (file-newer-than-file-p load-file-name file))
          (kill-buffer
           (let ((buf
                  (with-current-buffer (find-file file)
                    (add-header)
                    (current-buffer))))
             (with-current-buffer (htmlize-buffer buf)
               (write-file outfile)
               (current-buffer))))
          (format-links-in-file "n" outfile))
        (when (and (not (makefile-p file))
                   (or (file-newer-than-file-p file outfile-nonhtml)
                       (file-newer-than-file-p load-file-name file)))
          (copy-file file outfile-nonhtml t t t t))
        (call-process "touch" nil nil nil outfile outfile-nonhtml))))

  (defadvice org-publish-get-project-from-filename (around ew activate)
    (setq ad-return-value (car org-publish-project-alist)))

  (let ((output-file (car argv))
        (htmlize-link (cadr argv))
        (input-dir (expand-file-name (car (cddr argv))))
        (output-dir (expand-file-name (car (nthcdr 3 argv))))
        (input-files (nthcdr 4 argv)))
    (load-file-link htmlize-link)
    (require 'htmlize)
    (mapcar #'htmlize-this-file input-files))
  (kill-emacs 0))
