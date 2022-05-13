
# P3M, A framework for modelling Molecular Interaction Maps

You need to install the picosat solver (package picosat in debian/ubuntu) and the opam packages xml-light and graphics.

There are many examples in this directory. Some of them even include initialization files for variables (see UserManual.pdf in the P3M-Manual repertory for an explanation of the graphical interface and the command line switches).

This is how to launch 4 of them:

     ./p3m -filename atmchk -query +apoptose\(5\) -grounding 5
     ./p3m -filename lac -query +Glucose\(5\) -grounding 5
     ./p3m -filename lac_r -orig -grounding 3 -query \+Glucose\(3\) -graph
     ./p3m -filename erk -query +ERK_p\(5\) -grounding 5 -graph

More explanations can be found in our paper [A framework for modelling Molecular Interaction Maps](https://arxiv.org/abs/2008.09546)
