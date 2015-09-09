EMACS ?= emacs
CASK ?= cask

all: compile

test: clean
	${MAKE} all

bootstrap:
	${CASK} install
	${MAKE} init.el

clean:
	$(RM) init.el
	$(RM) *.elc
	$(RM) */*.elc

init.el:
	$(RM) init.el
	${CASK} exec ${EMACS} -Q -batch \
		--eval "(require 'org)" \
		--eval '(setq org-confirm-babel-evaluate nil)' \
		--eval '(setq user-emacs-directory (file-name-directory (getenv "PWD")))' \
		--eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))" \
		--eval '(org-babel-load-file "init.org")'

compile: init.el
	${CASK} exec ${EMACS} -Q -batch \
		--eval '(setq byte-compile-error-on-warn nil)' \
		--eval '(byte-recompile-directory (expand-file-name (getenv "PWD")) 0)'

.PHONY: all bootstrap init.el test unit compile
