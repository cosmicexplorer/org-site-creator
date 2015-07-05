(load (concat (file-name-directory load-file-name)
              "emacs-script-common.el") nil t)

(setq enable-local-variables nil)

(defun makefile-p (file)
  (string-equal (file-name-sans-extension (file-name-nondirectory file))
                "Makefile"))

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

(defun remove-file-from-a-href (str)
  "Removes any \"file:\" designations from a given anchor tag of the form
<a href=\"... Meant to work on the result of `htmlize-format-link'."
  (if (string-match-p (concat "\\`" (regexp-quote "<a href=\"")) str)
      (let ((uri
             (if (string-match "[^\\]\"\\([^\"]+\\(\\\\\\)*\\)\"" str)
                 (match-string 1 str) nil)))
        (if uri
            (format
             "%s%s%s"
             (substring str 0 (match-beginning 1))
             (replace-regexp-in-string "\\`file:" "" uri)
             (substring str (match-end 1)))
          str))
    str))

(defadvice htmlize-format-link (around transform-file-links activate)
  (setq ad-return-value (remove-file-from-a-href ad-do-it)))

(defvar this-dir (file-name-directory load-file-name))

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
                           (if (makefile-p file) ".html" "{,.html}") "\n")
                   (file-relative-name
                    file (expand-file-name (concat this-dir "/..")))
                   (file-relative-name
                    outfile-nonhtml
                    (expand-file-name (concat this-dir "/..")))))
          (append-to-file (point-min) (point-max) output-file))
        (kill-buffer
         (let ((buf
                (with-current-buffer (find-file file)
                  (add-header)
                  (current-buffer))))
           (with-current-buffer (htmlize-buffer buf)
             (write-file outfile)
             (current-buffer))))
        (unless (makefile-p file)
          (copy-file file outfile-nonhtml t t t t)))))

  (defadvice org-publish-get-project-from-filename (around ew activate)
    (setq ad-return-value (car org-publish-project-alist)))

  (let ((output-file (car argv))
        (org-mode-link (cadr argv))
        (htmlize-link (car (cddr argv)))
        (color-theme-link (car (nthcdr 3 argv)))
        (color-theme-me-link (car (nthcdr 4 argv)))
        (input-dir (expand-file-name (car (nthcdr 5 argv))))
        (output-dir (expand-file-name (car (nthcdr 6 argv))))
        (input-files (nthcdr 7 argv)))
    (load-file-link htmlize-link)
    (require 'htmlize)
    (mapcar #'htmlize-this-file input-files))
  (kill-emacs 0))
