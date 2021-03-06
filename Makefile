EMACS=emacs
EMACS_BATCH=$(EMACS) -Q -batch

all: compile ellit-org-logo.svg README.org

README.org: ellit-org.el
	$(EMACS_BATCH) -L . -l ellit-org.el --eval '(let ((debug-on-error t)) (ellit-org-export "ellit-org.el" "README.org"))'

compile: ellit-org.elc

ellit-org.elc: ellit-org.el
	$(EMACS_BATCH) -f batch-byte-compile ellit-org.el

test: test.el ellit-org.elc
	$(EMACS_BATCH) -L . -l test -f ert-run-tests-batch-and-exit

ellit-org-logo.svg: ellit-org.el
	$(EMACS_BATCH) -L . -l ellit-org --eval '(ellit-org--save-logo "ellit-org-logo.svg" 512)'

clean:
	@rm -vf ellit-org.elc

.PHONY: clean test
