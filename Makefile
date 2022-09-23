.PHONY: build install uninstall clean test

build:
	dune build 

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean