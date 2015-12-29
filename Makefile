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

# Use emacs to generate init.el from init.org
# we first tangle the org-mode file to init.el~ the rename it.
# this can make use use async task to create another init.el after save.
init.el:
	${EMACS} -Q -batch \
		--eval "(require 'org)" \
		--eval '(setq org-confirm-babel-evaluate nil)' \
		--eval '(setq org-confirm-execute-src-block nil)' \
		--eval '(delete-file "init.el~" nil)' \
		--eval '(org-babel-tangle-file "init.org" "init.el~")' \
		--eval '(rename-file "init.el~" "init.el" t)'

compile: init.el
	${CASK} exec ${EMACS} -Q -batch \
		--eval '(setq byte-compile-error-on-warn nil)' \
		--eval '(byte-recompile-directory (expand-file-name (getenv "PWD")) 0)'

.PHONY: all bootstrap init.el test unit compile
