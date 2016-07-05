OCAMLBUILDFLAGS=-classic-display -no-hygiene
SRC=$(wildcard *.ml) parser.mly lexer.mll
LIBS=unix,dynlink

.PHONY: all clean
all: main.byte

main.byte: $(SRC)
	ocamlbuild $(OCAMLBUILDFLAGS) -libs $(LIBS) -cflag -dtypes -cflag -g -lflag -g main.byte

main.native: $(SRC)
	ocamlbuild $(OCAMLBUILDFLAGS) -libs $(LIBS) main.native

clean:
	ocamlbuild $(OCAMLBUILDFLAGS) -clean
