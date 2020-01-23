EMACS=emacs -Q

all: README.org

README.org: ellit-org.el
	$(EMACS) -batch -L . -l ellit-org.el --eval '(ellit-org-file "ellit-org.el" "README.org")'
