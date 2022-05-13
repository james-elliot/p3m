open Picosat;;

let _ =
  init();
  ignore(add(+1));ignore(add(-2));ignore(add(0));
  ignore(add(-3));ignore(add(0));
  ignore(add(-3));ignore(add(-4));ignore(add(+2));ignore(add(0));
  ignore(add(+3));ignore(add(-5));ignore(add(+4));ignore(add(0));
  ignore(add(-6));ignore(add(-2));ignore(add(0));
  ignore(add(+6));ignore(add(-2));ignore(add(0));
  ignore(add(+7));ignore(add(-2));ignore(add(0));
  ignore(add(-5));ignore(add(0));
  ignore(add(+5));ignore(add(-4));ignore(add(+2));ignore(add(0));
  ignore(add(+4));ignore(add(-2));ignore(add(0));
  let res = sat () in
  match res with
  | Satisfiable ->
     Printf.printf "Satisfiable\n";
     for i = 1 to 7 do
       let v =
         match deref i with
         | False -> "false"
         | Not_assigned -> "not assigned"
         | True -> "true" in
       Printf.printf "%d %s\n" i v;
     done;
  | Unsatisfiable -> Printf.printf "Unsatisfiable\n"
  | Unknown -> Printf.printf "Unknown\n";;
    
