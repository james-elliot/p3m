(*
  Initialize the solver with solver_init
  Add litterals to a clause with add_lit and end the clause by add_lit(0)
  Solve with solve
  Cleanup memory with solver_reset
*)

type satisfiability =
  | Unknown
  | Satisfiable
  | Unsatisfiable;;

type assignment =
  | False
  | Not_assigned
  | True;;

external init : unit -> unit = "init"
external reset : unit -> unit = "reset"
external add : int -> int = "add"
external assume : int -> unit = "assume"
external sat : unit -> satisfiability = "sat"
external inc_max_var : unit -> int = "inc_max_var"
external push : unit -> unit = "push"
external pop : unit -> unit = "pop"
external deref : int -> assignment = "deref"
external variables : unit -> int = "variables"
external added_original_clauses : unit -> int = "added_original_clauses"        
