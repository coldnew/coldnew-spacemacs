EMACS ?= emacs
CASK ?= cask

all: compile

test: clean
	${MAKE} all
clean:
	$(RM) config.el
	$(RM) *.elc

compile:
	${CASK} exec ${EMACS} -Q -batch -l init.el \
		--eval '(setq byte-compile-error-on-warn t)' \
	        --eval '(byte-recompile-directory (expand-file-name (getenv "PWD")) 0)'

.PHONY: all test unit compile
