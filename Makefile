EMACS=emacs -Q

all: compile README.org

README.org: ellit-org.el
	$(EMACS) -batch -L . -l ellit-org --eval '(ellit-org-file "ellit-org.el" "README.org")'

compile: ellit-org.elc

ellit-org.elc: ellit-org.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile ellit-org.el

test: test.el ellit-org.elc
	$(EMACS) -batch -L . -l test -f ert-run-tests-batch-and-exit

clean:
	@rm -vf ellit-org.elc

.PHONY: clean test
