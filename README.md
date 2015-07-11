org-site-creator
================

A truly disgusting hack to put together a site from a bunch of org (and other) files. Example repo is [my trashy attempt at a website](https://github.com/cosmicexplorer/cosmicexplorer.github.io) (make sure to checkout the `site` branch after cloning, master only has the output and no build system).

# Dependencies

- [Xvfb](http://www.x.org/archive/X11R7.6/doc/man/man1/Xvfb.1.xhtml) isn't required, but makes the htmlized output look more like your actual environment if you use emacs in graphical mode. If you keep emacs in `nw` mode, then just just set `xvfb_disp` to `nw` and you should be fine. You can usually get it through your distribution's package manager.
- [node/npm](https://nodejs.org) for some helper scripts.
- [emacs](https://gnu.org/software/emacs), ideally version 24.5 or above, I haven't tested with earlier versions. If it yells at you about undefined functions, just build the [development version](http://savannah.gnu.org/projects/emacs/), I don't really care about trying to support older emacsen since I use the development version. If you actually are trying to use this code and don't want to have to hack it yourself, contact me and I'll make it work for earlier versions.
- [perl](https://perl.org)
- [make](https://gnu.org/software/make), [grep](https://gnu.org/software/grep/), [findutils](https://gnu.org/software/findutils/), and [coreutils](https://gnu.org/software/coreutils).
- [git](https://git-scm.com/) for the submodules.
- [texinfo](http://www.gnu.org/software/texinfo/) for `makeinfo`, to build the [org-mode](https://org-mode.org) submodule. This could probably be removed, but it's not a difficult dependency to satisfy.

# Directions

1. Add this repo as a git submodule (you can potentially just do it in a "docs" branch of your repo if you only use this to provide a website and not to install documentation).
2. Modify the site.config provided. The paths specified for `indir` and `outdir` are relative to where you cloned this repo (so `.` refers to this repo, and `..` to the folder containing it).
  - `indir`: the root of your org files. The tree specified here will be transferred exactly to `outdir`, so if you use your project root as a base for `indir` but your org files are in a subdirectory, that entire subdirectory structure will be cloned.
  - `outdir`: the root of your html output.
  - `xvfb_disp`: X display to use for running emacs headless under Xvfb. If you don't have Xvfb installed, or don't care about running emacs in graphical mode, you can turn this to `nw` to have it use nonwindowed emacs. If the htmlize step is failing, you can set this to `:0` or another valid X screen to see why (emacs will open in your normal display).
  - `highlight_css`: [y/n]. use if you want to enable the default css for highlightjs usage (you don't have to, other options are provided + you can use your own).
  - `export_email`: [y/f/n]. format to export email addresses at bottom of org pages: y for standard, f to allow the use of an emacs lisp `format` command instead of a standard email address (see [this example repo](https://cosmicexplorer.github.io) for an example).
  - `org_info`: use org_info.js and associated css. this is kinda the point of using this repo, so there's no real reason not to have it on.
  - `html_prefix`: prefix for links to bare files. in addition to publishing your org files, this also copies and htmlizes every file in the specified input directory and copies it to your output directory. The htmlized files have links to their non-htmlized selves. If you leave this blank, it will just link to the version of the file that was copied over during the build. I like to add something like "http://cosmicexplorer.github.io/blob/master/" so it directly links to github. Whatever floats your boat.

# Gotchas

1. I haven't fixed the make dependencies so that `make` will always detect when changes need to be made, so you'll have to run `make rebuild` instead of `make` in pretty much any situation. Due to the possibility that the input and output directories can contain each other, it's conceivable that that might not even be possible (?). I doubt it — the better answer is that I'm just too lazy to fix it. This is in large part because the htmlization and html generation from org files is done in batch mode; again, this could still be fixed in the Makefile, but it's not really a priority right now.
2. It takes like 30 seconds to generate stuff. Sorry about that.
3. It makes three directories in your specified output directory; `styles`, `scripts`, and `org-info-js`. Don't put anything in these, since they'll be deleted whenever you remake.
4. `indir` and `outdir` can be literally anything you want, including the same thing, including each other, etc.
5. Sometimes it doesn't work, for a variety of reasons. Typically, run `make rebuild` once or twice will solve the issue. If not, run `make distclean`, then `make` again, which might help. Uncommenting the `1>&2 2>/dev/null` in the `$(OUT_PAGES)` and `$(HTMLIZE_OUT)` is typically what I do when those fail to build (everything usually builds pretty reliably).
6. htmlized output looks prettier if you run it on your own personal box instead of a build server or whatever, since it represents the file as emacs would represent it to you, so a bare emacs will look pretty flat.
7. Installing from CPAN automatically only works sometimes. Feel free to run make a few more times, or make as a root user or under sudo, until it works.
8. htmlize.el transcribes quite literally the contents of the file *as it is visible in emacs* to html; this means if any folding occurs (such as when using hs-minor-mode, or when using org-mode or markdown-mode's folding abilities), parts of the file will NOT be visible. to avoid this in org files, you can preface them with `#+STARTUP: showeverything`. For other filetypes, you may have to add additional hooks or advices.
