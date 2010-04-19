OCAMLC   = OCAMLPATH=../.. ocamlfind ocamlc
OCAMLOPT = OCAMLPATH=../.. ocamlfind ocamlopt
OCAMLDEP = OCAMLPATH=../.. ocamlfind ocamldep
override OCAMLOPTFLAGS += -ccopt -L.. -w Ael -g -S
override OCAMLFLAGS    += -ccopt -L.. -w Ael -g
override CFLAGS        += -ggdb -std=c99 -D_GNU_SOURCE $(HAVE_CACHECTL)
override CPPFLAGS      += -I $(shell ocamlfind printconf stdlib)

CSOURCES = bench_c.c
CLIB     = libbench_c.a

REQUIRES = jiton

.PHONY: all clean

PROGRAMS = lighten.byte

all: byte opt
byte: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

$(CLIB): $(CSOURCES:.c=.o)
	$(AR) r $@ $?

# Clean up
clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.s *.o *.byte *.opt .depend $(CLIB)

# Dependencies
.depend: *.ml
	$(OCAMLDEP) -package "$(REQUIRES)" -I .. $^ > $@

lighten.byte: $(CLIB)
lighten.opt: $(CLIB)

include ../make.common
include .depend
