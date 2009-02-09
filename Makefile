.PHONY: all install clean

all:
	ocamlbuild -lib unix lpcsprg.native

install:
	cp lpcsprg.native ~/bin/lpcsprg

clean:
	ocamlbuild -clean
