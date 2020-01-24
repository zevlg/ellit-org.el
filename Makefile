EMACS=emacs -Q

all: compile ellit-org-logo.svg README.org

README.org: ellit-org.el
	$(EMACS) -batch -L . -l ellit-org --eval '(let ((ellit-org-template-unknown-warn nil)) (ellit-org-file "ellit-org.el" "README.org"))'

compile: ellit-org.elc

ellit-org.elc: ellit-org.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile ellit-org.el

test: test.el ellit-org.elc
	$(EMACS) -batch -L . -l test -f ert-run-tests-batch-and-exit

ellit-org-logo.svg: ellit-org.el
	$(EMACS) -batch -L . -l ellit-org --eval '(ellit-org--save-logo "ellit-org-logo.svg" 64)'

clean:
	@rm -vf ellit-org.elc

.PHONY: clean test
