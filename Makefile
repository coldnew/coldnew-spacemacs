EMACS ?= emacs
CASK ?= cask

all: compile

test: clean
	${MAKE} all

bootstrap:
	${MAKE} clean
	${MAKE} init.el
	${CASK} install

Cask:
	${RM} Cask
	${MAKE} init.el

clean:
	$(RM) init.el Cask
	$(RM) *.elc
	$(RM) */*.elc

# cask exec emacs -Q -batch -l "ob-tangle" -eval "(org-babel-tangle-file \"init.org\")"
init.el:
	$(RM) init.el
	${EMACS} -Q -batch \
		--eval "(require 'org)" \
		--eval '(setq org-confirm-babel-evaluate nil)' \
		--eval '(setq org-confirm-execute-src-block nil)' \
		--eval '(org-babel-tangle-file "init.org")'

compile: init.el
	${CASK} exec ${EMACS} -Q -batch \
		--eval '(setq byte-compile-error-on-warn nil)' \
		--eval '(byte-recompile-directory (expand-file-name (getenv "PWD")) 0)'

.PHONY: all bootstrap init.el test unit compile
