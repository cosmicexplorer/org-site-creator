(load (concat (file-name-directory load-file-name)
              "emacs-script-common.el") nil t)

(setq enable-local-variables nil)

(defun makefile-p (file)
  (string-equal (file-name-sans-extension (file-name-nondirectory file))
                "Makefile"))

(let* ((tmpbuf (find-file (concat default-directory "tmpfile")))
       (output-file (concat default-directory "output-file"))
       (argv
        (split-string
         (with-current-buffer tmpbuf (buffer-string)))))
  (kill-buffer tmpbuf)
  (defun htmlize-this-file (file)
    (setq file (expand-file-name file))
    (let ((common-root output-dir))
      (while (not (string-match-p (regexp-quote common-root) file))
        (setq common-root (file-name-directory common-root)))
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
                           (if (makefile-p file) ".html" "{,.html}") "\n")
                   (file-relative-name file)
                   (file-relative-name outfile-nonhtml)))
          (append-to-file (point-min) (point-max) output-file))
        (htmlize-file (expand-file-name file) outfile)
        (unless (makefile-p file)
          (copy-file (expand-file-name file) outfile-nonhtml t t t t)))))

  (defadvice org-publish-get-project-from-filename (around ew activate)
    (setq ad-return-value (car org-publish-project-alist)))

  (let ((htmlize-link (car argv))
        (org-mode-link (cadr argv))
        (color-theme-link (car (cddr argv)))
        (color-theme-me-link (car (nthcdr 3 argv)))
        (input-dir (expand-file-name (car (nthcdr 4 argv))))
        (output-dir (expand-file-name (car (nthcdr 5 argv))))
        (input-files (nthcdr 6 argv)))
    (load-file-link org-mode-link)
    (require 'org)
    (load-file-link htmlize-link)
    (require 'htmlize)
    (load-file-link color-theme-link)
    (require 'color-theme)
    (load-file-link color-theme-me-link)
    (require 'color-theme-danny)
    (color-theme-danny)
    (mapcar #'htmlize-this-file input-files))
  (kill-emacs 0))
