The program compiles easily under linux and cygwin. It should also work under MacOS.

Under debian required packages are:
picosat
libxml-light-ocaml-dev


If ocaml is installed with opam, libxml-light-ocaml-dev is no longer required but the opam package xml-light is.

With the latest versions of ocaml (>=4.09), the graphics library
is no longer included in the standard distribution and has to be added
(graphics package in opam).

There are more explanations in the Makefile.

There are many examples in this directory. Some of them even include initialization files for variables (see UserManual).

./p3m -filename atmchk -query +apoptose\(5\) -grounding 5
./p3m -filename lac -query +Glucose\(5\) -grounding 5
./p3m -filename lac_r -orig -grounding 3 -query \+Glucose\(3\) -graph
./p3m -filename erk -query +ERK_p\(5\) -grounding 5 -graph
