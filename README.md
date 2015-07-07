org-site-creator
================

A truly disgusting hack to put together a site from a bunch of org files. To use:

1. Add this repo as a git submodule (you can potentially just do it in a "docs" branch of your repo if you only use this to provide a website and not to install documentation).
2. Modify the site.config provided. The paths specified for `indir` and `outdir` are relative to where you cloned this repo.
  - `indir`: the root of your org files. The tree specified here will be transferred exactly to `outdir`, so if you use your project root as a base for `indir` but your org files are in a subdirectory, that entire subdirectory structure will be cloned.
  - `outdir`: the root of your html output.
  - `xvfb_disp`: X display to use for running emacs headless under Xvfb. If you don't have Xvfb installed, or don't care about running emacs in graphical mode, you can turn this to `nw` to have it use nonwindowed emacs.
  - `highlight_css`: [y/n]. use if you want to enable the default css for highlightjs usage (you don't have to, other options are provided + you can use your own).
  - `export_email`: [y/f/n]. format to export email addresses at bottom of org pages: y for standard, f to allow the use of an emacs lisp `format` command instead of a standard email address (see [this example repo](http://cosmicexplorer.github.io) for an example).
  - `org_info`: use org_info.js and associated css. this is kinda the point of using this repo, so there's no real reason not to have it on.
  - `html_prefix`: prefix for links to bare files. in addition to publishing your org files, this also copies and htmlizes every file in the specified input directory and copies it to your output directory. The htmlized files have links to their non-htmlized selves. If you leave this blank, it will just link to the version of the file that was copied over during the build. I like to add something like "http://cosmicexplorer.github.io/blob/master/" so it directly links to github. Whatever floats your boat.

Some gotchas include:

1. It makes three directories in your specified output directory; `styles`, `scripts`, and `org-info-js`. Don't put anything in these, since they'll be deleted whenever you remake.
2. `indir` and `outdir` can be literally anything you want, including the same thing, including each other, etc.
3. It takes like a minute to run which is kind of annoying.
4. Sometimes it doesn't work, for a variety of reasons. Typically, run `make rebuild` once or twice will solve the issue. If not, run `make distclean`, then `make` again, which might help. Uncommenting the `1>&2 2>/dev/null` in the `$(OUT_PAGES)` and `$(HTMLIZE_OUT)` is typically what I do when those fail to build (everything usually builds pretty reliably).
