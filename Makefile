.PHONY: all clean
.SECONDARY: main-build

# If you have opam, this is the simplest way.
obin = $(shell opam var bin)
xmllibdir = $(shell opam var xml-light:lib)
# Add the following line for ocaml>=4.09
graphicslibdir = $(shell opam var graphics:lib)

# If you don't have opam, you must set by hand the above variables
#obin =/usr/bin
#xmllibdir = /usr/lib/ocaml/xml-light
# For ocaml >=4.09, the graphics library is no longer in the standard distrib so
#graphicslibdir = /usr/lib/ocaml/graphics

mlsatlibdir = ./MLSAT

# For ocaml <4.09
#O_INCLUDES = -I $(xmllibdir)  -I $(mlsatlibdir)
# For ocaml >=4.09
O_INCLUDES = -I $(xmllibdir)  -I $(mlsatlibdir) -I $(graphicslibdir)

O_OBJS = p3m
O_LIBS = picosat xml-light graphics str unix

O_INTERFACES = 

O_SRC =  $(O_OBJS:=.ml)
O_COBJS = $(O_OBJS:=.cmo)
O_OOBJS = $(O_OBJS:=.cmx)

O_CLIBS = $(O_LIBS:=.cma)
O_OLIBS = $(O_LIBS:=.cmxa)


OCAMLOPT = $(obin)/ocamlopt
OCAMLC = $(obin)/ocamlc
OCAMLF =  $(O_INCLUDES)

%.cmi: %.mli
	$(OCAMLC) $(OCAMLF) -c $<

%.cmx : %.ml
	$(OCAMLOPT) $(OCAMLF) -c $<

%.cmo : %.ml
	$(OCAMLC) $(OCAMLF) -c $<


all: mlsat .depend ml-build

mlsat:
	(cd MLSAT;make)

.depend: $(O_INTERFACES) $(O_SRC)
	ocamldep  $<  >  $@

ml-build:
	@$(MAKE) --no-print-directory p3m

p3m : $(O_OOBJS)
	$(OCAMLOPT) $(OCAMLF) $(O_OLIBS) $< -o $@ -ccopt -L$(mlsatlibdir) 


clean:
	(\rm -f p3m *.o *.cmi *.cmx .depend *.cnf *~ P3M-Manual/*.aux P3M-Manual/*.log P3M-Manual/*~;cd MLSAT;make clean)

-include .depend
