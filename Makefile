.PHONY: test

test:
	dune exec test/test.exe test $(TESTS)
