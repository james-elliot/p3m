/* Need packet picosat in debian */
#include "picosat/picosat.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
/* #include <stdio.h> */

#ifdef PICOSAT_REENTRANT_API
static PicoSAT *ps;

void init()
{
  ps=picosat_init ();
}

void reset()
{
  picosat_reset(ps);
}

int add(value a)
{
  /*  printf("%d ",Int_val(a)); */
  return(Val_int(picosat_add(ps,Int_val(a))));
}

void assume(value a)
{
  picosat_assume(ps,Int_val(a));
}

int sat()
{
  switch(picosat_sat(ps,-1))
    {
    case PICOSAT_UNKNOWN : return(Val_int(0));
    case PICOSAT_SATISFIABLE : return(Val_int(1));
    case PICOSAT_UNSATISFIABLE : return(Val_int(2));
    }
  return(Val_int(0));
}

int inc_max_var()
{
  return(Val_int(picosat_inc_max_var(ps)));
}

void push()
{
  picosat_push(ps);
}

void pop()
{
  picosat_pop(ps);
}

int deref(value a)
{
  return Val_int(picosat_deref(ps,Int_val(a))+1);
}

int variables()
{
  return Val_int(picosat_variables(ps));
}

int added_original_clauses()
{
  return Val_int(picosat_added_original_clauses(ps));
}
#else

void init()
{
  picosat_init ();
}

void reset()
{
  picosat_reset();
}

int add(value a)
{
  return Val_int(picosat_add(Int_val(a)));
}

void assume(value a)
{
  picosat_assume(Int_val(a));
}

int sat()
{
  switch(picosat_sat(-1))
    {
    case PICOSAT_UNKNOWN : return(Val_int(0));
    case PICOSAT_SATISFIABLE : return(Val_int(1));
    case PICOSAT_UNSATISFIABLE : return(Val_int(2));
    }
  return(Val_int(0));
}

int inc_max_var()
{
  return(Val_int(picosat_inc_max_var()));
}

void push()
{
  picosat_push();
}

void pop()
{
  picosat_pop();
}

int deref(value a)
{
  return Val_int(picosat_deref(Int_val(a))+1);
}

#endif
