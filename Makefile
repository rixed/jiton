NAME = jiton

OCAMLC     = ocamlfind ocamlc
OCAMLOPT   = ocamlfind ocamlopt
OCAMLDEP   = ocamlfind ocamldep
INCS       =
OCAMLOPTFLAGS = $(INCS) -w Ae -g
OCAMLFLAGS    = $(INCS) -w Ae -g
CPPFLAGS   = -I $(shell ocamlfind printconf stdlib)

SOURCES  = $(wildcard *.ml)
OBJECTS  = $(SOURCES:.ml=.cmo)
XOBJECTS = $(OBJECTS:.cmo=.cmx)
CSOURCES = codebuf.c wrap_codebuf.c
CLIB     = libjithelper.a

ARCHIVE  = $(NAME).cma
XARCHIVE = $(ARCHIVE:.cma=.cmxa)

REQUIRES   = num

.PHONY: clean all

all: $(ARCHIVE)
opt: $(XARCHIVE)

$(CLIB): $(CSOURCES:.c=.o)
	$(AR) r $@ $?

$(ARCHIVE): $(OBJECTS) $(CLIB)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) -custom $(OBJECTS) -cclib -ljithelper

$(XARCHIVE): $(XOBJECTS) $(CLIB)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(OBJECTS) -cclib -ljithelper

install: all
	if test -f $(XARCHIVE) ; then extra="$(XARCHIVE) "`basename $(XARCHIVE) .cmxa`.a ; fi ; \
	ocamlfind install $(NAME) *.cmi $(ARCHIVE) META geom.ml cnt.ml $$extra

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

check: $(ARCHIVE) $(XARCHIVE)
	make -C tests all opt && tests/virtual_test.byte

# Clean up
clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.o *.s .depend

# Dependencies
.depend: $(wildcard *.ml)
	$(OCAMLDEP) -package "$(REQUIRES)" $^ > $@

include make.common
include .depend
