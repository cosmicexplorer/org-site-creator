#!/bin/bash
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp -*-
(load (concat (file-name-directory load-file-name)
              "emacs-script-common.el") nil t)
(let ((org-dir (car argv))) (byte-recompile-directory org-dir))
(kill-emacs 0)
