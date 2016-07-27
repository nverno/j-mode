emacs ?= emacs

auto ?= j-mode-autoloads.el
el = $(filter-out $(auto),$(wildcard *.el))
elc = $(el:.el=.elc)

loadpath ?= .
elpa_dir ?=~/.emacs.d/elpa
auto_flags ?= \
	--eval "(let ((generated-autoload-file \
                      (expand-file-name (unmsys--file-name \"$@\"))) \
                      (wd (expand-file-name default-directory)) \
                      (backup-inhibited t) \
                      (default-directory (expand-file-name \"$(elpa_dir)\"))) \
                   (normal-top-level-add-subdirs-to-load-path) \
                   (update-directory-autoloads wd))"

.PHONY: $(auto) clean
all: compile $(auto) README.md

compile: $(elc)
%.elc : %.el
	$(emacs) -batch -L $(loadpath) -f batch-byte-compile $<

$(auto):
	$(emacs) -batch $(auto_flags)

clean:
	$(RM) *~ *.elc *loaddefs.el *autoloads.el
