example: example.ml picosat.cmxa picosat.cmi
	ocamlopt -o example picosat.cmxa example.ml -ccopt -L.

picosat.cmxa: libmlpicosat.a picosat.cmx
	ocamlopt  picosat.cmx -a -o picosat.cmxa -cclib -lmlpicosat -cclib -lpicosat

picosat.cma: libmlpicosat.a picosat.cmo
	ocamlc  picosat.cmo -a -o picosat.cma -cclib -lmlpicosat -cclib -lpicosat

libmlpicosat.a: mlpicosat.o
	ar r libmlpicosat.a mlpicosat.o

mlpicosat.o: mlpicosat.c
	ocamlopt -c mlpicosat.c

picosat.cmi: picosat.mli
	ocamlopt -c picosat.mli

picosat.cmx: picosat.cmi picosat.ml
	ocamlopt -c picosat.ml

picosat.cmo: picosat.cmi picosat.ml
	ocamlc -c picosat.ml


clean:
	\rm -f picosat.a libmlpicosat.a picosat.cma picosat.cmxa *.cmi *.o *.cmx *.cmo example *~

