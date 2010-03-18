NAME = jiton

OCAMLC     = ocamlfind ocamlc
OCAMLOPT   = ocamlfind ocamlopt
OCAMLDEP   = ocamlfind ocamldep
INCS       =
HAVE_CACHECTL = $(shell if test -r /usr/include/asm/cachectl.h ; then echo '-DHAVE_CACHECTL' ; fi)
override OCAMLOPTFLAGS += $(INCS) -w Ae -g
override OCAMLFLAGS    += $(INCS) -w Ae -g
override CFLAGS        += -ggdb -std=c99 -D_GNU_SOURCE $(HAVE_CACHECTL)
override CPPFLAGS      += -I $(shell ocamlfind printconf stdlib)

SOURCES  = jiton.ml codebuffer_impl.ml compiler_impl.ml $(wildcard impl_*.ml)
OBJECTS  = $(SOURCES:.ml=.cmo)
XOBJECTS = $(OBJECTS:.cmo=.cmx)
CSOURCES = codebuf.c wrap_codebuf.c
CLIB     = libjithelper.a

ARCHIVE  = $(NAME).cma
XARCHIVE = $(ARCHIVE:.cma=.cmxa)

REQUIRES   = num bigarray

.PHONY: clean all dump

all: $(ARCHIVE)
opt: $(XARCHIVE)

$(CLIB): $(CSOURCES:.c=.o)
	$(AR) r $@ $?

$(ARCHIVE): $(OBJECTS) $(CLIB)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) -custom $(OBJECTS) -cclib -ljithelper

$(XARCHIVE): $(XOBJECTS) $(CLIB)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(XOBJECTS) -cclib -ljithelper

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

# Just to remember the cmd line
dump:
	objdump -m mips:loongson_2f -b binary -M no-aliases,gpr-names=n32 -D /tmp/test.code

# Dependencies
.depend: $(SOURCES)
	$(OCAMLDEP) -package "$(REQUIRES)" $^ > $@

include make.common
include .depend
