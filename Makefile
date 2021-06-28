.PHONY: test

test:
	dune exec test/test.exe test $(TESTS)

build:
	dune build bin/lefunq.exe 
	cp _build/default/bin/lefunq.exe ./lefunq
	
install:
	dune build bin/lefunq.exe
	rm -f /usr/local/bin/lefunq
	cp _build/default/bin/lefunq.exe /usr/local/bin/lefunq
