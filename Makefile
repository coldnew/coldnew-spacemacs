EMACS ?= emacs
CASK ?= cask

all: compile

test: clean
	${MAKE} all

bootstrap: init.el
	${CASK} install

clean:
	$(RM) init.el
	$(RM) *.elc
	$(RM) */*.elc

init.el:
	${CASK} exec ${EMACS} -Q -batch \
		--eval "(require 'org)" \
		--eval '(setq org-confirm-babel-evaluate nil)' \
		--eval '(setq user-emacs-directory (file-name-directory (getenv "PWD")))' \
		--eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))" \
		--eval '(org-babel-load-file "init.org")'

compile: init.el
	${CASK} exec ${EMACS} -Q -batch \
		--eval '(setq byte-compile-error-on-warn nil)' \
		--eval '(byte-compile-file "init.el")'
#		--eval '(byte-recompile-directory "spacemacs" 0)'
#	        --eval '(byte-recompile-directory (expand-file-name (getenv "PWD")) 0)'

.PHONY: all init.el test unit compile
