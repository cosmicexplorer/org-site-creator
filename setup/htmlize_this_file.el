(load (concat (file-name-directory load-file-name)
              "emacs-script-common.el") nil t)

(setq enable-local-variables nil)

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
      (let* ((outfile
              (expand-file-name
               (concat
                output-dir "/"
                (replace-regexp-in-string
                 (regexp-quote common-root) "" file)
                ".html")))
             (outfile-dir (file-name-directory outfile)))
        (unless (file-exists-p outfile-dir)
          (make-directory outfile-dir))
        (with-temp-buffer
          (insert (format "%s => %s\n" (file-relative-name file)
                          (file-relative-name outfile)))
          (append-to-file (point-min) (point-max) output-file))
        (let ((file-buf (find-file file)))
          (kill-buffer
           (with-current-buffer
               (with-current-buffer file-buf
                 (font-lock-fontify-buffer)
                 (htmlize-buffer (current-buffer)))
             (write-file outfile)))
          (kill-buffer file-buf)))))

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
