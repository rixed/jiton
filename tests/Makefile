OCAMLC   = OCAMLPATH=../.. ocamlfind ocamlc
OCAMLOPT = OCAMLPATH=../.. ocamlfind ocamlopt
OCAMLDEP = OCAMLPATH=../.. ocamlfind ocamldep
OCAMLOPTFLAGS = -S -w Ael -g
OCAMLFLAGS    = -w Ael -g

REQUIRES = jiton

.PHONY: all clean

PROGRAMS = virtual_test.byte

all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

# Clean up
clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.s *.o *.byte *.opt .depend

# Dependencies
.depend: *.ml
	$(OCAMLDEP) -package "$(REQUIRES)" -I .. $^ > $@

include ../make.common
include .depend
