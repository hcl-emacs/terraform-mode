.PHONY : test

EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .
LOAD_HELPER = -l test/test-helper.el

ELPA_DIR = \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

test: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/test-highlighting.el -l test/test-indentation.el \
		-l test/test-command.el \
		-f ert-run-tests-batch-and-exit

test-highlighting: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/test-highlighting.el \
		-f ert-run-tests-batch-and-exi

test-indentation: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/test-indentation.el \
		-f ert-run-tests-batch-and-exit

test-command: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/test-command.el \
		-f ert-run-tests-batch-and-exit

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
