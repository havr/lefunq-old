.PHONY: test

test:
	dune exec test/test.exe test $(TESTS)

install:
	dune build bin/lefunq.exe
	cp _build/default/bin/lefunq.exe /usr/local/bin/lefunq
