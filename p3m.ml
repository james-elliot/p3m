let auto_image = ref false;;
let max_clauses=10000000;;
let max_vars=128;;
let get_cpu_time () =
  let {Unix.tms_utime=tms_utime;Unix.tms_stime=tms_stimes} = Unix.times () in
  tms_utime+.tms_stimes;;

module OrderedSB = struct type t = string*bool let compare = compare end;;
module SetSB = Set.Make(OrderedSB);;
module OrderedSBSB = struct type t = string*bool*bool let compare = compare end;;
module SetSBSB = Set.Make(OrderedSBSB);;
module OrderedSetSB = struct type t = SetSB.t let compare = SetSB.compare end;;
module SetSetSB = Set.Make(OrderedSetSB);;

let pdebug debug = if debug then Printf.fprintf else Printf.ifprintf;;

type arrow =
  | Conversion
  | Stimulation
  | Binding
  | Inhibition;;

let arrow_to_string a = 
  match a with
  | Conversion -> "Conversion"
  | Inhibition -> "Inhibition"
  | Binding -> "Binding"
  | Stimulation -> "Stimulation";;

type graphic_object_val = 
    Atom of string
  | Arrow of (int Lazy.t*int Lazy.t*arrow);; (* id1 id2 arrow *)

let print_graphic_object_val debug fp v =
  match v with
  | Atom s -> pdebug debug fp "%s" s
  | Arrow(id1,id2,arr) ->
     pdebug debug fp "(%d %d %s)"
       (Lazy.force id1) (Lazy.force id2) (arrow_to_string arr);;
  
type graphic_object =
  {
    id : int;
    v : graphic_object_val;
  };;

let print_graphic_object debug fp o =
  pdebug debug fp "%d: " o.id;
  print_graphic_object_val debug fp o.v;
  pdebug debug fp "\n";;
  
module OrderedGraph = struct type t = graphic_object let compare = compare end;;
module GraphSet = Set.Make(OrderedGraph);;
module OrderedString = struct type t = string let compare = compare end;;
module StringMap = Map.Make(OrderedString);;
module OrderedInt = struct type t = int let compare = compare end;;
module IntMap = Map.Make(OrderedInt);;
module IntSet = Set.Make(OrderedInt);;
module OrderedIntInt = struct type t = int*int let compare = compare end;;
module IntIntSet = Set.Make(OrderedIntInt);;
module StringSet = Set.Make(OrderedString);;
module OrderedStringSet = struct type t = StringSet.t let compare = StringSet.compare end;;
module StringSetMap = Map.Make(OrderedStringSet);;
module StringSetSet = Set.Make(OrderedStringSet);;
module OrderedStringList = struct type t = string list let compare = compare end;;
module StringListSet = Set.Make(OrderedStringList);;

let stringSet_of_list l  =  List.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty l;;

exception Name of string;;
let write_ppm base =
  Printf.printf "Writing image";print_newline();
  let name =
    try
      for i=1 to 9999 do
	let name =base^(Printf.sprintf "%04d" i)^".png" in
	if not (Sys.file_exists name) then raise (Name name);
      done;
      failwith "No filename"
    with (Name name) -> name in
  let width = Graphics.size_x () and height = Graphics.size_y () in
  let tab = Graphics.dump_image (Graphics.get_image 0 0 width height) in
  let oc = Unix.open_process_out ("/usr/bin/convert - "^name) in
  Printf.fprintf oc "P6\n%d %d\n255\n" width height;
  for y = 0 to height-1 do
    for x = 0 to width-1 do
      let c = tab.(y).(x) in
      let r = (c land 0xff0000) lsr 16
      and g = (c land 0xff00) lsr 8
      and b = (c land 0xff) in
      output_char oc (char_of_int r);
      output_char oc (char_of_int g);
      output_char oc (char_of_int b);
    done;
  done;
  output_char oc '\n';
  close_out oc;
  Printf.printf "Image written";print_newline();;



let save_vals exos endos assumed filename =
  Printf.printf "Saving ini file";print_newline();
  let name = filename^".ini" in
  let fp=open_out name in
  StringSet.iter (fun x-> Printf.fprintf fp "%s " x) exos;
  Printf.fprintf fp "\n";
  StringSet.iter (fun x-> Printf.fprintf fp "%s " x) endos;
  Printf.fprintf fp "\n";
  SetSetSB.iter
    (fun s ->
      if (SetSB.cardinal s)<> 1 then
	failwith "Only one litteral in assumed clause";
      let (e,b) = SetSB.choose s in
      if b then 
	Printf.fprintf fp "+%s" e
      else Printf.fprintf fp "~%s" e)
    assumed;
  Printf.fprintf fp "\n";
  close_out fp;
  Printf.printf "Done";print_newline();;

let font_normal () =  Graphics.set_font "6x13";;

let font_bold () =  Graphics.set_font "6x13";;

let init_graph width height =
  let disp = try Sys.getenv("DISPLAY")
             with Not_found -> failwith "DISPLAY not set" in
  let (width,height)=(int_of_float width,int_of_float height) in
  (*  let str = ("unix:0 "^(string_of_int width)^"x"^(string_of_int height)) in *)
  let str = (disp^" "^(string_of_int width)^"x"^(string_of_int height)) in
  Graphics.open_graph str;
  Graphics.set_color Graphics.black;
  font_normal();;
  (*  Graphics.set_text_size 36;;*)

let find_from_id idf objects = 
  let s = GraphSet.filter (fun {id=id} -> (id=idf)) objects in
  match GraphSet.cardinal s with
  | 0 -> raise Not_found
  | 1 -> GraphSet.choose s
  | _ -> failwith "Too many in find_from_id";;

exception Sortie of bool;;
let pi = acos (-1.0);;
let display_graph objects positions width height exos endos assumed new_arrs filename =
  if !auto_image then Printf.printf "Auto_image2\n";
  let (width,height)=(int_of_float width,int_of_float height) in
  let draw_line x1 y1 x2 y2 arr =
    let d = sqrt ((x2-.x1)*.(x2-.x1)+.(y2-.y1)*.(y2-.y1)) in
    let pd = 1.-. 10.0/.d in
    let x2=x1+.(x2-.x1)*.pd and y2=y1+.(y2-.y1)*.pd in
    let (ix1,iy1,ix2,iy2) =
      (int_of_float x1,int_of_float y1,int_of_float x2,int_of_float y2) in
    let (iy1,iy2)=(height-iy1,height-iy2) in
    Graphics.draw_segments [|(ix1,iy1,ix2,iy2)|];
    let alpha = atan2 (y1-.y2) (x1-.x2) in
    match arr with
    | Inhibition ->
       let inc = 90. *. pi/.180. in
       let (x3,y3) = (x2+.10.*.(cos (alpha+.inc)),y2+.10.*.(sin (alpha+.inc)))
       and (x4,y4) = (x2+.10.*.(cos (alpha-.inc)),y2+.10.*.(sin (alpha-.inc))) in
       let (ix3,iy3,ix4,iy4) =
	 (int_of_float x3,int_of_float y3,int_of_float x4,int_of_float y4) in
       let (iy3,iy4)=(height-iy3,height-iy4) in
       Graphics.draw_segments [|(ix2,iy2,ix3,iy3);(ix2,iy2,ix4,iy4)|];
    | Binding -> ()
    | Conversion | Stimulation ->
       let inc = 30. *. pi/.180. in
       let (x3,y3) = (x2+.10.*.(cos (alpha+.inc)),y2+.10.*.(sin (alpha+.inc)))
       and (x4,y4) = (x2+.10.*.(cos (alpha-.inc)),y2+.10.*.(sin (alpha-.inc))) in
       let (ix3,iy3,ix4,iy4) =
	 (int_of_float x3,int_of_float y3,int_of_float x4,int_of_float y4) in
       let (iy3,iy4)=(height-iy3,height-iy4) in
       if arr=Conversion then
	 Graphics.fill_poly[|(ix2,iy2);(ix3,iy3);(ix4,iy4)|]
       else
	 Graphics.draw_poly[|(ix2,iy2);(ix3,iy3);(ix4,iy4)|]
  in
  let draw_box x y w h str =
    let (x,y,w,h)=
      (int_of_float x,int_of_float y,int_of_float w,int_of_float h) in
    let y = height-y in
    Graphics.draw_rect (x-w/2) (y-h/2) w h;
    Graphics.set_color Graphics.black;
    let (wt,ht)=Graphics.text_size str in
    Graphics.moveto (x-wt/2) (y-ht/2);
    let col =
      try 
	SetSetSB.iter
	  (fun s ->
	    if (SetSB.cardinal s)<> 1 then
	      failwith "Only one litteral in assumed clause";
	    let (e,b) = SetSB.choose s in
	    if e=str then raise (Sortie b))
	  assumed;
	Graphics.black
      with Sortie b -> if b then Graphics.green else Graphics.red
    in
    Graphics.set_color col;
    if (StringSet.mem str exos) then font_bold();
    Graphics.draw_string str;
    font_normal();
    Graphics.set_color Graphics.black;
  in
  Graphics.auto_synchronize false;
  Graphics.clear_graph();
  GraphSet.iter
    (function {id=id;v=v} ->
      match v with
      | Atom s ->
	 let (x,y,w,h) = IntMap.find id positions in
	 (*	 Printf.printf "%d: %s %f %f %f %f\n" id s x y w h; *)
	 draw_box x y w h s;
	 ()
      | Arrow(id1,id2,arr) ->
	 let (x1,y1,x2,y2) = IntMap.find id positions in
	 (*
	 Printf.printf
	   "%d: %s FROM %f %f TO %f %f\n" id (arrow_to_string arr) x1 y1 x2 y2;
	 *)
	 Graphics.set_color
	   (if IntSet.mem id new_arrs then Graphics.red else Graphics.black);
	   draw_line x1 y1 x2 y2 arr;
	 Graphics.set_color Graphics.black;
	 ())
    objects;
  Graphics.auto_synchronize true;
  if !auto_image then Printf.printf "Auto_image\n";
  if !auto_image then write_ppm filename;;

let find_box ex ey objects positions =
  let height=Graphics.size_y () in
  let res = GraphSet.filter
    (function {id=id;v=v} ->
      match v with
      | Atom s ->
	 let (x,y,w,h) = IntMap.find id positions in
	 let (x,y,w,h) =
	   (int_of_float x,int_of_float y,int_of_float w,int_of_float h) in
	 let y = height -y in
	 ((abs (x-ex))<w/2) && ((abs (y-ey))<h/2)
      | _ -> false)
    objects in
  if (GraphSet.cardinal res)>1 then failwith "There should be at most one box";
  if (GraphSet.cardinal res)=0 then None
  else
    let {id=id;v=v} = GraphSet.choose res in
    match v with
    | Atom s -> Some s
    | _ -> failwith "Pattern matching failure in find_box";;
  
let graph_vars objects positions width height exos endos filename =
  let exos = ref exos and endos = ref endos in
  let flip s =
    if StringSet.mem s !exos then (
      exos := StringSet.remove s !exos;
      endos := StringSet.add s !endos)
    else (
      exos := StringSet.add s !exos;
      endos := StringSet.remove s !endos;
    )
  in
  display_graph
    objects positions width height !exos !endos SetSetSB.empty IntSet.empty filename;
  try 
    while true do
      let status = Graphics.wait_next_event
	[Graphics.Button_down;Graphics.Key_pressed] in
      if status.Graphics.button then
	let ex = status.Graphics.mouse_x and ey = status.Graphics.mouse_y in
	let res = find_box ex ey objects positions in
	(match res with
	| None ->()
	| Some s -> flip s;
	  display_graph
	    objects positions width height !exos !endos SetSetSB.empty IntSet.empty filename)
      else
	match status.Graphics.key with
	| 'g' -> raise Exit;
	| 'i' -> write_ppm filename
	| _ -> ()
    done;
    failwith "Never here in graph_vars"
  with Exit -> (!exos,!endos);;

exception Sorties of SetSB.t*bool;;
let graph_values objects positions width height exos endos assumed filename =
  let assumed = ref assumed in
  let flip str =
    try
      SetSetSB.iter
	(fun s ->
	  if (SetSB.cardinal s)<> 1 then
	    failwith "Only one litteral in assumed clause";
	  let (e,b) = SetSB.choose s in
	  if e=str then raise (Sorties (s,b)))
	!assumed;
      assumed := SetSetSB.add (SetSB.singleton (str,true)) !assumed
    with Sorties (s,b) ->
	  assumed := SetSetSB.remove s !assumed;
	  if b then
	    assumed := SetSetSB.add (SetSB.singleton (str,false)) !assumed
  in
  try 
    display_graph
      objects positions width height exos endos !assumed IntSet.empty filename;
    while true do
      let status = Graphics.wait_next_event
	[Graphics.Button_down;Graphics.Key_pressed] in
      if status.Graphics.button then
	let ex = status.Graphics.mouse_x and ey = status.Graphics.mouse_y in
	let res = find_box ex ey objects positions in
	(match res with
	| None ->()
	| Some s -> flip s;
	  display_graph
	    objects positions width height exos endos !assumed IntSet.empty filename)
      else
	match status.Graphics.key with
	| 'g' -> raise Exit;
	| 'i' -> write_ppm filename
	| 's' -> save_vals exos endos !assumed filename
	| _ -> ()
    done;
    failwith "Never here in graph_values"
  with Exit -> !assumed;;

let graph_sols objects positions width height exos endos assumed nres new_arrs filename =
  let tab = Array.of_list nres
  and i = ref 0
  and old_i = ref (-1) in
  try 
    while true do
      if !i<0 then i:=(Array.length tab)-1;
      if !i=(Array.length tab) then i:=0;
      let nassumed =
	if (List.length nres) >0 then
	  List.fold_left
	    (fun a (e,b) -> SetSetSB.add (SetSB.singleton (e,b)) a)
	    assumed tab.(!i)
	else assumed in
      if !old_i <> !i then display_graph
	objects positions width height exos endos nassumed new_arrs filename;
      let status = Graphics.wait_next_event
	[Graphics.Key_pressed] in
      match status.Graphics.key with
      | 'n' -> incr i;
      | 'p' -> decr i
      | 'i' -> write_ppm filename
      | 'g' -> raise Exit
      | 's' -> save_vals exos endos nassumed filename
      | _ -> ()
    done;
    failwith "Never here in graph_vars"
  with Exit -> ();;

let graph_arrow
    objects positions width height exos endos assumed nres new_id =
  let {id=_;v=v} = find_from_id new_id objects in
  match v with
  | Arrow(id1,id2,arr) -> 
     let id1 = Lazy.force id1 and id2 = Lazy.force id2 in
     let (x1,y1,w1,h1) = IntMap.find id1 positions in
     let {id=_;v=v2} = find_from_id id2 objects in
     let np = match v2 with
       | Atom _ ->
          let (x2,y2,w2,h2) = IntMap.find id2 positions in
          (x1,y1,x2,y2)
       | Arrow _ ->
          let (x2,y2,x3,y3) = IntMap.find id2 positions in
          (x1,y1,(x2+.x3)/.2.,(y2+.y3)/.2.) in
     Printf.printf "Printing graph\n";print_newline();
     Printf.printf "Hit s to save values of variables\n";
     Printf.printf "Hit i to save an image of the current graph\n";
     Printf.printf "Hit n to go to the next set of variables that are solutions\n";
     Printf.printf "Hit p to go to the previous set of variables that are solutions\n";
     Printf.printf "Hit g to go to the next possible graph\n";
     print_newline();
     graph_sols
       objects (IntMap.add new_id np positions) width height
       exos endos assumed nres (IntSet.singleton new_id)
  | _ -> failwith "Pattern matching in graph_arrow";;

let check_name name =
  try
    let _ = String.rindex name ',' in
    failwith (name^" : No comma (,) in variables names");
  with
    Not_found ->
    try
      let _ = String.rindex name '$' in
      failwith (name^" : No dollar sign ($) in variables names");
    with Not_found -> 
      try
        let _ = String.rindex name ' ' in
        failwith (name^" : No space ( ) in variables names");
      with Not_found -> ();;

let parse_pathways lx = 
  let objects = ref GraphSet.empty in
  let positions = ref IntMap.empty in
  let count = ref 0 and boardwidth = ref 0. and boardheight = ref 0.0 in
  let map_id = ref StringMap.empty in
  let smf gref = try StringMap.find gref !map_id with Not_found -> -1 in
  let parse_pathway x =
    let parse_interaction x = 
      match x with
      | Xml.Element (tag,_,children) -> 
	 (match tag with
	  | "Graphics" -> 
	     incr count;
	     let (id1,id2,arr,orig,dest) = 
	       List.fold_left
	         (fun (id1,id2,arr,orig,dest) c ->
		   match c with
		   | Xml.Element (tag,attrs,_) -> 
		      (match tag with 
		       | "Point" -> 
		          (let gref =
			     try Xml.attrib c  "GraphRef"
			     with x -> ""
			   in
			   let x = Xml.attrib c "X" and y = Xml.attrib c "Y" in
			   try
			     let arrow = Xml.attrib c "ArrowHead" in
			     let arr =
			       match arrow with
			       | "mim-conversion" -> Conversion
			       | "mim-necessary-stimulation" -> Stimulation
			       | "mim-inhibition" -> Inhibition
                               | "mim-binding" -> Binding
			       | _ -> failwith ("Invalid arrow type "^arrow)  in
			     (id1,Some (lazy (smf gref)),Some arr,orig,Some (x,y))
			   with (Xml.No_attribute s) ->
			     (Some (lazy (smf gref)),id2,arr,Some(x,y),dest))
		       | "Anchor" -> 
		          (try
			     let gid = Xml.attrib c "GraphId" in
			     map_id := StringMap.add gid !count !map_id;
			     (id1,id2,arr,orig,dest)
			   with _ -> (id1,id2,arr,orig,dest))
		       | _ -> (id1,id2,arr,orig,dest))
		   | _ -> (id1,id2,arr,orig,dest))
	         (None,None,None,None,None) children in
	     (match (id1,id2,arr,orig,dest) with
	      | (Some id1,Some id2,Some arr,orig,dest) ->
	         objects := GraphSet.add {id= !count;v=Arrow(id1,id2,arr)} !objects;
	         (match (orig,dest) with
	          | (Some(x1,y1),Some(x2,y2)) ->
		     positions := IntMap.add !count
		                    (float_of_string x1,float_of_string y1,float_of_string x2,float_of_string y2)
		                    !positions
	          | _ -> () (* We do not store the positions of anchors *)
	         )
	      | _ ->
	         failwith "Should not happen");
	  | _ -> ())
      | _ -> () in
    let parse_datanode d id =
      match d with
      | Xml.Element (tag,attrs,children) ->
	 (match tag with
	 | "Graphics" ->
	    let x = Xml.attrib d "CenterX" and y = Xml.attrib d "CenterY"
	    and w = Xml.attrib d "Width" and h = Xml.attrib d "Height" in
	    positions := IntMap.add id (float_of_string x,float_of_string y,float_of_string w,float_of_string h) !positions
	 | _ -> ())
      | _ -> () in
    (match x with
    | Xml.Element (tag,attrs,children) -> 
       (match tag with
       | "DataNode" -> 
	  incr count;
	 let name = Xml.attrib x "TextLabel"
	 and gid = Xml.attrib x "Graphid" in
         check_name name;
	 map_id := StringMap.add gid !count !map_id;
	 objects := GraphSet.add {id= !count;v=Atom name} !objects;
	 List.iter (fun x -> parse_datanode x !count) children
       | "Interaction" -> List.iter parse_interaction children
       | "Graphics" ->
	  boardwidth := float_of_string (Xml.attrib x "BoardWidth");
	  boardheight := float_of_string (Xml.attrib x "BoardHeight");
       | _ -> ());
    | _ -> ()) in
  List.iter parse_pathway lx;
  (!objects,!count,!positions,!boardwidth,!boardheight);;

let print_graph debug fp objects =
  let l = ref [] in
  GraphSet.iter 
    (fun {id=id;v=v} ->
      pdebug debug fp "(%d,"id;
      match v with
      | Atom x -> 
	 pdebug debug fp "Atom(%s))" x
      | Arrow (id1,id2,arr) ->
	 let id1 = Lazy.force id1 and id2 = Lazy.force id2 in
	 if id1= -1 || id2= -1 then l:= (id1,id2,arrow_to_string arr):: !l;
	 pdebug debug fp "Arrow(%d,%d,%s))" id1 id2 (arrow_to_string arr))
    objects;
  pdebug debug fp "\n";
  List.iter
    (fun (i,j,a) ->
     pdebug debug fp "Arrow(%d,%d,%s))\n" i j a;
     if i<> -1 then print_graphic_object debug fp (find_from_id i objects);
     if j<> -1 then print_graphic_object debug fp (find_from_id j objects);
     ())
    !l;
  if List.length !l <> 0 then failwith "Invalid Graph";;

type connector = 
  | Or
  | And
  | Implies
  | Equivalent ;;

let connector_to_string c = 
  match c with
  | Or -> "Or"
  | And -> "And"
  | Implies -> "Implies"
  | Equivalent -> "Equivalent" ;;


type tree = 
  | Leaf of string*bool*bool (* name , bool value, consumed or not consumed *)
  | Node of connector*tree*tree
  | Not of tree;;


(* # is used before the name of the variable to indicate it is consumed *)
let rec tree_to_string t arg = 
  match t with
  | Not s -> "(Not ("^(tree_to_string s arg)^")"
  | Leaf(s,flag,cons) ->
     let s=if arg || (not cons) then s else (s^"#") in if flag then s else ("~"^s)
  | Node(c,l1,l2) -> "("^(connector_to_string c)^","^(tree_to_string l1 arg)^","^(tree_to_string l2 arg)^")";;


let setSB_of_list l  = List.fold_left (fun acc x -> SetSB.add x acc) SetSB.empty l;;

let setSetSB_of_list l  =  List.fold_left (fun acc x -> SetSetSB.add x acc) SetSetSB.empty l;;

exception Tautology;;
let rec simp_or t = 
  match t with
  | Node(Or,s1,s2) ->
     let t1 = simp_or s1 and t2 = simp_or s2 in
     let res = 
       SetSB.for_all (fun (a1,b1) -> 
	 SetSB.for_all (fun (a2,b2) -> (a1<>a2) || (b1=b2)) t2)
	 t1 in
     if res then SetSB.union t1 t2 else raise Tautology
  | Leaf (a,b,_) -> SetSB.singleton (a,b)
  | _ -> failwith "Should never happen in simp_or";;

let rec flat_tree_to_SetSetSB t = 
  match t with 
  | Node(And,s1,s2) ->
     SetSetSB.union (flat_tree_to_SetSetSB s1) (flat_tree_to_SetSetSB s2)
  | Node(Or,s1,s2) ->
     (try SetSetSB.singleton (simp_or (Node(Or,s1,s2)))
     with Tautology -> SetSetSB.empty)
  | Leaf(a,b,_) -> SetSetSB.singleton (SetSB.singleton (a,b))
  | _ -> failwith "Should not happen in flat_tree_to_SetSetSB";;

let print_SetSetSB debug fp s =
  SetSetSB.iter 
    (fun e -> 
      pdebug debug fp "[";
      SetSB.iter 
	(fun (s,b) -> 
	  if b then pdebug debug fp "%s," s else pdebug debug fp "~%s," s)
	e;
      pdebug debug fp "]")
    s;
  pdebug debug fp "\n";;

let rec neg_tree t =
  match t with
  | Not s -> s
  | Node(Implies,a,b) -> Node(Implies,neg_tree b,neg_tree a)
  | Node(Equivalent,a,b) -> Node(Or,Node(Implies,neg_tree b,neg_tree a),Node(Implies,neg_tree a,neg_tree b))
  | Node (And,a,b) -> Node(Or ,(neg_tree a),(neg_tree b))
  | Node (Or, a,b) -> Node(And,(neg_tree a),(neg_tree b))
  | Leaf (s,b,c)     -> Leaf(s,not b,c);;

let flatten_tree t = 
  let modif = ref true in
  let rec fft t =
    match t with
    | Not s ->
       modif:=true;
       fft (neg_tree s)
    | Node(Implies,a,b)->
       modif:=true;
       Node(Or,fft (Not a),fft b)
    | Node(Equivalent,a,b)->
       modif:=true;
       Node(And,fft (Node(Implies,a,b)),fft (Node(Implies,b,a)))
    | Node(Or,Node(And,a,b),Node(And,c,d)) -> 
      modif := true;
      Node(And,Node(Or,fft a,fft c),
	   Node(And,Node(Or,fft a,fft d),Node(And,Node(Or,fft b,fft c),Node(Or,fft b,fft d))))
    | Node(Or,Node(And,a,b),c) ->
      modif := true;
      Node(And,Node(Or,fft a,fft c),Node(Or,fft b,fft c))
    | Node(Or,a,Node(And,b,c)) ->
      modif := true;
      Node(And,Node(Or,fft a,fft b),Node(Or,fft a,fft c))
    | Node(Or,a,b) ->
      Node(Or,fft a,fft b)
    | Node(And,a,b) ->
      Node(And,fft a,fft b)
    | Leaf (s,b,c) -> Leaf (s,b,c)
  in
  let r = ref t in
  while !modif do
    modif:=false;
    r:= fft !r
  done;
  !r;;

let before o objects = 
  GraphSet.elements
    (GraphSet.filter
       (fun {v=vt} ->
	 match vt with 
	 | Arrow (_,id2,_) -> (o.id=(Lazy.force id2))
	 | _ -> false) 
       objects);;

let rec leaf (s,b,c) = 
  let cpt = ref 0 in
  while s.[!cpt]=' ' do incr cpt done;
  let ind = try String.index s ',' 
		  with Not_found -> (String.length s) in
  let cpt2 = ref (ind-1) in
  while s.[!cpt2]=' ' do decr cpt2 done;
  let s1 = String.sub s !cpt (!cpt2 - !cpt + 1) in
  if ind = (String.length s) then Leaf (s1,b,c)
  else
    let s2 = String.sub s (ind+1) ((String.length s)-ind-1) in
    if b then
      Node(And,Leaf(s1,b,c),leaf (s2,b,c))
    else
      Node(Or,Leaf(s1,b,c),leaf (s2,b,c));;

type activity = Activ | Inactiv;;
    
let model o act cons objects =
  let rec m_l o act cons = 
    match o.v with
    | Arrow (id1,_,_) -> 
       let a = find_from_id (Lazy.force id1) objects in
       (match a.v with
       | Atom s -> 
	  List.fold_left 
	    (fun fm o2 -> 
	      match o2.v with
	      | Arrow (_,_,arr2) ->
		 (match arr2 with
		 | Conversion ->
		    failwith "No conversion inside subtree in model"
		 | Stimulation ->
		    if act=Activ then Node(And,fm,m_l o2 Activ false) 
		    else Node(Or,fm,m_l o2 Inactiv false)
		 | Inhibition ->
		    if act=Activ then Node(And,fm,m_l o2 Inactiv false) 
		    else Node(Or,fm,m_l o2 Activ false)
                 | _ -> failwith "Should not happen in model 0")
	      | Atom _ -> failwith "Should not happen in model")
	    (leaf (s,(act=Activ),cons)) 
	    (before o objects)
       | Arrow _ -> failwith "should not happen in model (2)")
    | Atom _ -> failwith "should not happen in model (3)"
  in m_l o act cons;;

let transform objects debug =
  let mapa = ref IntMap.empty in
  let suppress = ref IntSet.empty in
  GraphSet.iter
    (fun {id=id;v=v} ->
      match v with
      | Arrow(id1,id2,Binding) ->
         suppress := IntSet.add id !suppress;
         let s = try IntMap.find (Lazy.force id2) !mapa with Not_found -> StringSet.empty in
         let o = find_from_id (Lazy.force id1) objects in
         (match o.v with
          | Atom x ->
             mapa := IntMap.add (Lazy.force id2) (StringSet.add x s) !mapa
          | _ -> failwith "Should not happen in transform")
      | _ -> ())
    objects;
  let objs =
    GraphSet.filter
      (fun {id=id;v=v} -> not (IntSet.mem id !suppress))
      objects in
  GraphSet.map
    (fun o ->
      let {id=id;v=v} = o in
      try
        let s = IntMap.find id !mapa in
        match v with
        | Atom x -> {id=id;v=Atom (StringSet.fold (fun a b -> a^","^b) s x)}
        | _ -> failwith "Should not happen in transform 2"
      with Not_found ->  o
    )
    objs;;
  

let translate_graph objects debug =
  let objects = transform objects debug in
  pdebug debug stdout "Printing graph after substitution\n";
  print_graph debug stdout objects;
  pdebug debug stdout "\n";
  let ht = Hashtbl.create 100 in
  let orgs = GraphSet.filter
    (fun {id=id;v=v} -> 
      match v with
      | Arrow _ -> false
      | Atom _ ->  
	(GraphSet.exists 
	   (fun {v=vt} ->
	     match vt with 
	     | Atom _ -> false
	     | Arrow (id1,id2,_) -> ((Lazy.force id2)=id))
	   objects)) objects in
  if (GraphSet.cardinal orgs >=1) then (
    let orgs = GraphSet.elements orgs in
    pdebug debug stdout "\nPrinting all final points in the graph (future heads of clauses)\n";
    List.iter 
      (fun {id=id;v=v} -> 
	match v with 
	| Atom x -> pdebug debug stdout "%d %s\n" id x
	| _ -> failwith "Should not happen in translate")
      orgs;
    pdebug debug stdout "\nPrinting all graphs formulas\n";
    List.iter 
      (fun o ->
	match o.v with 
	| Atom s -> 
	   List.iter 
	     (fun hd -> 
	       let (flag,cons) = match hd.v with 
		 | Arrow(_,_,Conversion) -> (true,true)
		 | Arrow(_,_,Stimulation) -> (true,false)
		 | Arrow(_,_,Inhibition) -> (false,false)
		 | _ -> failwith "Should not happen in translate (2)" in
	       let t = model hd Activ cons objects in
	       let st = tree_to_string t false in
	       pdebug debug stdout "%s %b:%s\n" s flag st; 
	       Hashtbl.add ht (s,flag)  t)
	     (before o objects);
	| _ -> failwith "Should not happen in translate (3)")
      orgs;
    ht)
  else
    ht;;

let extract_vars ht =
  let endos =
    Hashtbl.fold (fun (s,_) _ e -> StringSet.add s e) ht StringSet.empty in
  let rec traverse_tree t = 
    match t with
    | Not t1 -> (traverse_tree t1)
    | Leaf(s,flag,_) -> StringSet.singleton s
    | Node(c,l1,l2) ->
       StringSet.union (traverse_tree l1) (traverse_tree l2) in
  let right_vars =
    Hashtbl.fold
      (fun (_,_) t e ->	StringSet.union (traverse_tree t) e)
      ht StringSet.empty in
  let exos = StringSet.diff right_vars endos in
  (exos,endos);;

let build_temporal_cnf ht exos endos grounding debug =
  let ht3 = Hashtbl.create 100 in
  (* Building the minimal temporal model *)
  let rec traverse_tree2 t = 
    match t with
    | Leaf(s,flag,c) ->
       if StringSet.mem s endos then
	 (SetSBSB.singleton (s^"$",flag,c),Leaf(s^"$",flag,c))
       else
	 (SetSBSB.singleton (s,flag,c),Leaf(s,flag,c))
    | Node(c,l1,l2) ->
       let (e1,s1)  = (traverse_tree2 l1) and (e2,s2) = (traverse_tree2 l2) in
       (SetSBSB.union e1 e2,Node(c,s1,s2))
    | _ -> failwith "Should not happen in initial traverse tree in build_temporal_cnf" in
  Hashtbl.iter
    (fun (name,flag) t ->
      let (vars,tr) = traverse_tree2 t in
      (* Replace classical clause with the same one where variables have
	 been replaced with their temporal equivalent, i.e. all endogenous variables a are now a$. 
         tr is the guard of the clause and name is the endogenous variable head of the clause 
         Sometimes, endogenous variables have been redefined by the user as exogenous one. In this case,
         there is no association created in the Hashtbl for it
       *)
      if (StringSet.mem name endos) then Hashtbl.add ht3 (name^"$") (tr,true);
      (* Here we find all consumed variables in the clause and store the associated guard *)
      SetSBSB.iter
	(fun (s1,f1,c1) -> if f1 && c1 && not (StringSet.mem s1 exos) then Hashtbl.add ht3 s1 (tr,false))
	vars
    )
    ht;
  pdebug debug stdout "\nPrinting new initial tree\n";
  Hashtbl.iter (fun s (t,flag) -> pdebug debug stdout "%s: %s (%b)\n" s (tree_to_string t true) flag) ht3;
  print_newline();

  (* Formula is:  a(t+1) <=> (P_1 |...|P_n) | (a(t) & ~C_1 & ... & ~C_n )  
     where P_i are the guards for productions and C_j are the guards for consumption 
     We first aggregate in map mt indexed by endogenous variable names the P part and the C part *)
  let mt = ref StringMap.empty in
  Hashtbl.iter (fun name (t,prod) ->
      let (p,c) =
        try StringMap.find name !mt
        with Not_found -> (None,Leaf (name,true,true)) in
      let res =
        if prod then
          match p with
          | None -> (Some t,c)
          | Some tp -> (Some (Node(Or,tp,t)),c)
        else
          (p,Node(And,c,(Not t))) in
      mt:= StringMap.add name res !mt)
    ht3;
  
  let setsb2 = ref SetSetSB.empty in
  StringMap.iter
    (fun name (p,c) ->
      pdebug debug stdout "%s: " name;
      let s1 = match p with
        | None -> "" 
        | Some tp -> (tree_to_string tp true) in
      let s2= (tree_to_string c true) in
      pdebug debug stdout "P:%s C:%s " s1 s2;
      (* We build the full tree using the above formula to combine P and C part *)
      let ft1 =
        match p with
        | None -> c
        | Some tp -> Node(Or,tp,c) in
      (* We add an extra $ to the head, so now a(t+1) is a$$ et a(t) is a$ *)
      let ft = Node(Equivalent,Leaf(name^"$",true,false),ft1) in
      pdebug debug stdout "ft:%s" (tree_to_string ft true);
      (* We flatten the final formula *)
      let ft = flatten_tree ft in
      pdebug debug stdout "flat:%s\n" (tree_to_string ft true);
      (* Convert it to a CNF clause *)
      let sft = flat_tree_to_SetSetSB ft in
      pdebug debug stdout "CNF(%d): " (SetSetSB.cardinal sft);
      print_SetSetSB debug stdout sft;
      setsb2 := SetSetSB.union sft !setsb2;
    )
    !mt;
  pdebug debug stdout "\nsetsb2: ";
  print_SetSetSB debug stdout !setsb2;
  pdebug debug stdout "\n";
  (* In grounding we replace a$ with a(i) and a$$ with a(i+1) for all i *)
  let setsb3 = ref SetSetSB.empty in
  for i=0 to grounding do
    setsb3 :=
      SetSetSB.union
        !setsb3
        (SetSetSB.map
           (fun ens ->
             SetSB.map
               (fun (name,b) ->
                 let name = try
                     let ind = String.index name '$' in
                     if (String.length name) = ind+1 then Printf.sprintf "%s(%d)" (String.sub name 0 (ind)) i
                     else Printf.sprintf "%s(%d)" (String.sub name 0 (ind)) (i+1)
                   with Not_found -> name in
                 (name,b))
               ens)
           !setsb2)  
  done;
  pdebug debug stdout "setsb3: ";
  print_SetSetSB debug stdout !setsb3;
  pdebug debug stdout "\n";
  !setsb3;;

(* Printing CNF model *)
let export_cnf cnf filename = 
    let fp = open_out filename in
    SetSetSB.iter
      (fun ssb ->
	SetSB.iter
	  (fun (s,b) ->
	    if b then Printf.fprintf fp "+%s" s else Printf.fprintf fp "-%s" s )
	  ssb;
	Printf.fprintf fp "\n")
      cnf;
    close_out fp;;

type solutions =
  | Sols of (string*bool) list list
  | No_sols
  | Question_is_implicate
  | Inconsistent_Database;;

  
let primes sssb =
  let rec count_bits t =
    if t=0 then 0
    else (t mod 2)+(count_bits (t/2)) in
  let map = ref StringMap.empty in
  let ind = ref (-1) in
  let clauses = Array.make max_clauses None
  and names = Array.make max_vars ""
  and last = ref 0
  and clp = ref 0 and clv = ref 0 in
  List.iter
    (fun ssb ->
      clp := 0;clv := 0;
      List.iter
	(fun (s,b) ->
	  let v =
	    try StringMap.find s !map
	    with Not_found ->
	      incr ind;
	      map := StringMap.add s !ind !map;
	      names.(!ind) <- s;
	      !ind in
	  if v>=(max_vars-1) then failwith "too many vars";
	  clp := !clp lor (1 lsl v);
	  if b then clv := !clv lor (1 lsl v) )
	ssb;
      clauses.(!last) <- Some (!clp,!clv);
      incr last;
      if !last=Array.length clauses then failwith "too many clauses")
    sssb;
  let flag = ref true in
  while !flag do
    flag := false;
    for i = 0 to !last-1 do
      try
	match clauses.(i) with
	| None -> ()
	| Some (p1,v1) ->
	   for j=i+1 to !last-1 do
	     match clauses.(j) with
	     | None -> ()
	     | Some (p2,v2) ->
		let p = p1 land p2 in
		if (p=p1) && ((p land v2)=v1) then (flag:=true;clauses.(j) <-None)
		else if (p=p2) && ((p land v1)=v2) then
		  (flag:=true;clauses.(i)<-None;raise Exit)
	   done
      with Exit ->()
    done;
    let i = ref 0 in
    while (!last>0) && (clauses.(!last-1)=None) do decr last; done;
    while !i < (!last-1) do
      if clauses.(!i) = None then (
	if !last=0 then failwith "last=0";
	clauses.(!i) <- clauses.(!last-1);
	decr last;
	while (!last>0) && (clauses.(!last-1)=None) do decr last; done;
      );
      incr i
    done;
    let oldlast= !last in
    for i = 0 to oldlast-1 do
      for j = i+1 to oldlast-1 do
        (*	let Some (p1,v1) = clauses.(i) and Some (p2,v2)= clauses.(j) in*)
        match clauses.(i) with
        | Some (p1,v1) ->
           (match clauses.(j) with
            | Some (p2,v2) ->
	       let p = p1 land p2 in
	       let t = (v1 lxor v2) land p in
	       if (count_bits t)=1 then 
	         let np = (p1 lor p2) land (lnot t) in
	         if np<>0 then (
	           let nv = (v1 lor v2) land np in
	          clauses.(!last) <- Some (np,nv);
	          flag := true;
	          incr last;
	          if !last=Array.length clauses then failwith "Too many clauses"
                 )
            | _ -> failwith "Pattern matching failed on p2 v2")
        | _ -> failwith "Pattern matching failed on p1 v1"
      done;
    done;
  done;
  let nssb = ref [] in
  for i = 0 to !last-1 do
    match clauses.(i) with
    | Some(p,v) ->
       let e = ref [] in
       for j = 0 to !ind do
         let k = (1 lsl j) in
         if (k land p)<>0 then
	   e:= (names.(j),(k land v)<>0):: !e;
       done;
       nssb :=  !e :: !nssb
    | _ -> failwith "Pattern matching failed on p v"
  done;
  !nssb;;
  
let solve_cnf cnf query test =
  let map = ref StringMap.empty in
  let check l =
    List.iter
      (fun (e,b)->
	let v = StringMap.find e !map in
	if b then Picosat.assume v else Picosat.assume (-v)) l;
    Picosat.sat () in
  let rec iter_set s l =
    if s=StringSet.empty then
      if check l = Picosat.Unsatisfiable then [l] else []
    else (
      let e = StringSet.choose s in
      iter_set (StringSet.remove e s) ((e,true)::l) @
      iter_set (StringSet.remove e s) ((e,false)::l)) in
  Picosat.init();
  SetSetSB.iter
    (fun ssb ->
      SetSB.iter
	(fun (s,b) ->
	  let v =
	    try StringMap.find s !map
	    with Not_found ->
	      let nb = Picosat.inc_max_var () in 
	      map := StringMap.add s nb !map;
	      nb in
	  if b then ignore (Picosat.add(v)) else ignore(Picosat.add(-v)))
	ssb;
      ignore(Picosat.add(0)))
    cnf;
  let res = Picosat.sat() in
  if res=Picosat.Unsatisfiable then
    (Picosat.reset();Inconsistent_Database)
  else
    let s = String.sub query 1 (String.length query -1) in
    let v =
      try StringMap.find s !map
      with Not_found -> failwith "Question asked not in the database" in
    if query.[0]='+' then ignore (Picosat.add(-v))
    else ignore (Picosat.add(v));
    ignore (Picosat.add(0));
    let res = Picosat.sat() in
    if res = Picosat.Unsatisfiable then (Picosat.reset();Question_is_implicate)
    else
      let res = iter_set test [] in
      Picosat.reset();
      if List.length res=0 then No_sols
      else (Sols res);;

let parse_assumed s =
  let set = ref SetSetSB.empty  in
  let curr = ref 0 in
  while (!curr<> String.length s) do
    if (s.[!curr]<>'+') && (s.[!curr]<>'~') then
      failwith "Assumed values must start with + or ~";
    let i1 = try String.index_from s (!curr+1) '+' with Not_found -> String.length s
    and i2 = try String.index_from s (!curr+1) '~' with Not_found -> String.length s in
    let l = (min i1 i2) - !curr -1 in
    let s1 = String.sub s (!curr+1) l in
    let b1 = (s.[!curr]='+') in
    set := SetSetSB.add (SetSB.singleton (s1,b1)) !set;
    curr := (min i1 i2)
  done;
  !set;;

let parse_vars s =
  let l = (Str.split (Str.regexp "[ \t]+") s) in
  List.fold_left (fun e s-> StringSet.add s e) StringSet.empty l;;
    
let parse_ini filename =
  try 
    let fp = open_in (filename^".ini") in
    let sexos = input_line fp
    and sendos = input_line fp
    and sassumed = input_line fp in
    let exos = parse_vars sexos
    and endos = parse_vars sendos
    and assumed = parse_assumed sassumed in
    (exos,endos,assumed)
  with _ -> (StringSet.empty,StringSet.empty,SetSetSB.empty);;

(* Union of two sets of assumed values for variables.
   s1 has priority over s2 *)
let union_a s1 s2 =
  let s = SetSetSB.union s1 s2 in
  SetSetSB.filter
    (fun e ->
      let (name,b)= SetSB.choose e in
      (SetSetSB.mem e s1) ||
        ((SetSetSB.mem e s2)&&(not (SetSetSB.mem (SetSB.singleton (name,not b)) s1))))
  s;;

let main =
  let debug=ref false in
  let do_cnf=ref false in
  let grounding = ref 0
  and graph = ref false
  and orig_set = ref false
  and no_reduction = ref false
  and no_init_vars = ref false
  and no_init_values = ref false
  and exit_after_solve = ref false 
  and filename = ref "example"
  and query = ref ""
  and assumed = ref ""
  and inc_endos = ref false 
  and false_endos = ref false in
  Arg.parse
    [
      ("-filename",Arg.Set_string filename,"Filename for gpml source (default example)");
      ("-image",Arg.Set auto_image,"Save automatically an image file each time a graph is displayed");
      ("-query",Arg.Set_string query,"Query to answer,starting with + for positive and ~ for negative ones (default empty string, which means no query to answer). Remember that special caracters such as ~ or parenthesis must be escaped!");
      ("-assumed",Arg.Set_string assumed,"Assumed values for (some) exogenous or endogenous variables such as +pml~mdm2 (default empty string)");
      ("-graph",Arg.Set graph,"Try to find solution in supergraphs (default false)");
      ("-noreduction",Arg.Set no_reduction,"Print original set before reduced set (default false)");
      ("-orig",Arg.Set orig_set,"Print original set before reduced set (default false)");
      ("-cnf",Arg.Set do_cnf,"Export original set of clauses in CNF format (default false)");
      ("-debug",Arg.Set debug,"Set debug value (default false)");
      ("-endos",Arg.Set inc_endos,"Endogenous variables are included in the test set (default false)");
      ("-false",Arg.Set false_endos,"Endogenous variables are set to false at t=0. This has no effect is -endos is set. (default false)");
      ("-no-init-vars",Arg.Set no_init_vars,"do not use GUI to initialize type of variables (default false)");
      ("-no-init-values",Arg.Set no_init_values,"do not use GUI to initialize values of variables (default false)");
      ("-exit-after-solve",Arg.Set exit_after_solve,"directly exit after solve step (default false)");
      ("-grounding",Arg.Set_int grounding,"Last grounding step (default 0)")
    ]
    (fun _ -> ())
    "Usage: p3m -filename filename -grounding n";
  if !auto_image then Printf.printf "Auto_image is ON\n";
  
  if ((String.length !query) >0) && (!query.[0]<>'+') && (!query.[0]<>'~') then
    failwith "Query has to start with + or ~";

  flush stdout;
  let x = Xml.parse_file (!filename^".gpml") in
  if Xml.tag x <> "Pathway" then failwith "No pathway???";

  let (objects,last_id,positions,width,height) =
    parse_pathways (Xml.children x) in
  pdebug !debug stdout "Printing Graph:%d\n" last_id;
  print_graph !debug stdout objects;
  init_graph width height;
  let all_vars =
    GraphSet.fold
      (fun {id=id;v=v} e ->
	match v with
	| Atom s -> StringSet.add s e
	| _ -> e)
      objects StringSet.empty in

  pdebug !debug stdout "Printing all vars\n";
  StringSet.iter (fun s -> pdebug !debug stdout "%s " s) all_vars;
  pdebug !debug stdout "\n";

  let ht = translate_graph objects !debug in
  pdebug !debug stdout "\nPrinting original set of clauses\n";
  Hashtbl.iter (fun (s,flag) t -> pdebug !debug stdout "%s %b: %s\n" s flag (tree_to_string t false)) ht;

  let (fexos,fendos,fassumed)=parse_ini !filename in
  let (exos,endos)=
    if fexos<>StringSet.empty || fendos<>StringSet.empty then
      (fexos,fendos)
    else  extract_vars ht in
  Printf.printf "\nExogenous variables: ";
  StringSet.iter (fun s -> Printf.printf "%s " s) exos;print_newline();
  Printf.printf "Endogenous variables : ";
  StringSet.iter (fun s -> Printf.printf "%s " s) endos;print_newline();
  let assumedl = parse_assumed !assumed in
  (*
  let assumed = 
    if fassumed<>SetSetSB.empty then fassumed
    else assumedl in
   *)
  let assumed = union_a assumedl fassumed in
  let assumed =
    if !false_endos && (not !inc_endos) then 
      let dassumed = StringSet.fold (fun name e -> SetSetSB.add (SetSB.singleton (name,false)) e) endos SetSetSB.empty in
      union_a assumed dassumed
    else assumed in
  Printf.printf "Assumed values: ";
  let vassumed = SetSetSB.fold
    (fun s ens ->
      if (SetSB.cardinal s)<> 1 then
	failwith "Only one litteral in assumed clause";
      let (e,b) = SetSB.choose s in
      if b then 
	Printf.printf "+%s" e
      else Printf.printf "~%s" e;
      StringSet.add e ens
    )
    assumed StringSet.empty in
  print_newline();
  print_newline();
  Printf.printf "Click on a variable to change its type : endogenous (regular font) <-> exogenous (bold font)\n";
  Printf.printf "Hit i to save an image of the current graph\n";
  Printf.printf "Hit g to go to the next step\n";
  print_newline();
  let un_exo_endo = (StringSet.union exos endos) in
  let diff = StringSet.diff all_vars un_exo_endo in
  let exos = StringSet.union exos diff in
  if StringSet.compare all_vars (StringSet.union exos endos) <>0 then
    failwith "endos+exos<>all_vars";
  if StringSet.inter exos endos<>StringSet.empty then
    failwith "endos invcup exos<>empty";
  if StringSet.compare (StringSet.inter vassumed all_vars) vassumed <>0 then
    failwith "not all assumed in all_vars";

  let (exos,endos) =
    if !no_init_vars then (exos,endos)
    else graph_vars objects positions width height exos endos !filename in
  Printf.printf "\nExogenous variables: ";
  StringSet.iter (fun s -> Printf.printf "%s " s) exos;print_newline();
  Printf.printf "Endogenous variables : ";
  StringSet.iter (fun s -> Printf.printf "%s " s) endos;print_newline();

  print_newline();
  Printf.printf "Click on a variable to change its value: free(black) <-> true(green) <-> false(red)\n";
  Printf.printf "Hit s to save values in a .ini file for a future run\n";
  Printf.printf "Hit i to save an image of the current graph\n";
  Printf.printf "Hit g to go to the next step\n";
  print_newline();
  
  
  let assumed =
    if !no_init_values then assumed
    else graph_values objects positions width height exos endos assumed !filename in
  
  Printf.printf "Assumed values:";
  SetSetSB.iter (fun s -> let (e,b)=SetSB.choose s in Printf.printf "(%s,%b) " e b) assumed;
  print_newline();

  let test_set = if !inc_endos then StringSet.union exos endos
                 else exos in
  let tmps =
    SetSetSB.fold
      (fun ssb s -> let (e,b) = SetSB.choose ssb in StringSet.add e s) 
      assumed StringSet.empty in
  let test_set = StringSet.diff test_set tmps in
  let test_set = StringSet.map (fun s -> if (StringSet.mem s exos) then s else s^"(0)") test_set in
    
  Printf.printf "Test Set size: %d\n" (StringSet.cardinal test_set);
  Printf.printf "Test Set:";
  StringSet.iter (fun s -> Printf.printf "%s " s) test_set;
  print_newline();


  let assumed =
    SetSetSB.map
      (fun ens ->
        let (name,b) = SetSB.choose ens in
        SetSB.singleton (
        if (StringSet.mem name exos) then (name,b)
        else (name^"(0)",b)
      ))
      assumed in
  Printf.printf "Assumed values:";
  SetSetSB.iter (fun s -> let (e,b)=SetSB.choose s in Printf.printf "(%s,%b) " e b) assumed;
  print_newline();


  let ptime = get_cpu_time () in
  let cnf = build_temporal_cnf ht exos endos !grounding !debug in
  let ptime2 = get_cpu_time () in
  Printf.printf "Translation time:%f\n" (ptime2-.ptime);
  let cnf =  SetSetSB.union assumed cnf in
  if !do_cnf then (
    Printf.printf "Exporting CNF file WITH assumed values";print_newline();
    export_cnf cnf (!filename^".cnf");
  );
  if !query="" then exit 0;
  Printf.printf "Solving question %s" !query;print_newline();
  let res = solve_cnf cnf !query test_set in
  let ptime3=get_cpu_time () in
  Printf.printf "Resolution time:%f\n" (ptime3-.ptime2);
  if !exit_after_solve then (
    let fp =
      try open_out_gen [Open_append;Open_text] 0o755 (!filename^".time")
      with Sys_error _ -> open_out (!filename^".time") in
    Printf.fprintf fp
      "%d %d %f %f %f %f\n"
      (StringSet.cardinal test_set) !grounding ptime (ptime2-.ptime) (ptime3-.ptime2) ptime3;
    close_out fp;
    exit 0);
  (match res with
   | Sols res ->
      Printf.printf "Original set size:%d\n" (List.length res);flush stdout;
      if !orig_set then (
        Printf.printf "Original set:\n";
        List.iter (fun l -> List.iter (fun (e,b) -> Printf.printf "(%s,%b)" e b) l;print_newline()) res);
      let res = if !no_reduction then res else primes res in
      Printf.printf "Reduced set size: %d\n" (List.length res); flush stdout;
      List.iter (fun l -> List.iter (fun (e,b) -> Printf.printf "(%s,%b)" e b) l;print_newline()) res;
      print_newline();
      Printf.printf "Hit s to save values of variables\n";
      Printf.printf "Hit i to save an image of the current graph\n";
      Printf.printf "Hit n to go to the next set of variables that are solutions\n";
      Printf.printf "Hit p to go to the previous set of variables that are solutions\n";
      Printf.printf "Hit g to end the program\n";
      print_newline();
      graph_sols objects positions width height exos endos assumed res IntSet.empty !filename;
   | No_sols -> Printf.printf "No_sols\n"
   | Question_is_implicate ->
      Printf.printf "Question is already an implicate\n";
   | Inconsistent_Database ->  Printf.printf "Inconsistent Database\n");
  

  (* Supergraphs *)  
  if not !graph then exit 0;
  (match res with
  | Sols _ ->
     Printf.printf "Supergraphs asked but query %s already answered without them\n" !query;
    exit 0
  | _ -> ());
  Printf.printf "Trying to solve question %s by building supergraphs" !query;
  print_newline();

  let nb = ref 0 in
  let new_id = last_id+1 in
  let add_arrow id1 id2 kind =
    pdebug !debug stdout "\nMod:%d %d %s\n" id1 id2 (arrow_to_string kind);
    incr nb;
    let new_objects=
      GraphSet.add {id=new_id;v=Arrow(lazy id1,lazy id2,kind)} objects in
    let ht = translate_graph new_objects !debug in
    pdebug !debug stdout "\nPrinting set of clauses\n";
    Hashtbl.iter (fun (s,flag) t -> pdebug !debug stdout "%s: %s\n" s (tree_to_string t false)) ht;
    let cnf = build_temporal_cnf ht exos endos !grounding !debug in
    let cnf =  SetSetSB.union assumed cnf in
    let res = solve_cnf cnf !query test_set in
    (match res with
    | Sols res ->
       Printf.printf "\nMod:%d %d %s\n" id1 id2 (arrow_to_string kind);
       if !orig_set then (
         Printf.printf "Original set:\n";
         List.iter (fun l -> List.iter (fun (e,b) -> Printf.printf "(%s,%b)" e b) l;print_newline()) res);
       let res = if !no_reduction then res else primes res in
       Printf.printf "Reduced set:\n";flush stdout;
       List.iter (fun l -> List.iter (fun (e,b) -> Printf.printf "(%s,%b)" e b) l;print_newline()) res;
       graph_arrow
	 new_objects positions width height exos endos assumed res new_id !filename;
    | No_sols ->
       ()
    | Question_is_implicate ->
       Printf.printf "\nMod:%d %d %s\n" id1 id2 (arrow_to_string kind);
      Printf.printf "Question is already an implicate\n";
      graph_arrow
	new_objects positions width height exos endos assumed [] new_id !filename;
    | Inconsistent_Database ->
       Printf.printf "\nMod:%d %d %s\n" id1 id2 (arrow_to_string kind);
       Printf.printf "Inconsistent Database\n");
    flush stdout;
    ()
  in
(* Creates a map binding each atom to its ids.
   An atom can have multiple ids as a protein can be duplicated in the map *)
  Printf.printf "Building atoms_ids";print_newline();
  let atoms_ids =
    GraphSet.fold
      (fun {id=id;v=v} m ->
	match v with
	| Atom s ->
	   let e = try StringMap.find s m with Not_found -> IntSet.empty in
	   StringMap.add s (IntSet.add id e) m
	| _ -> m)
      objects StringMap.empty
  in
  (* Create a set of all existing connections between two objects *)
  (* It is a little bit complicated because some proteins can be duplicated
     on the map *)
  Printf.printf "Building connections";print_newline();
  let connections =
    GraphSet.fold
      (fun {id=id;v=v} s -> 
	match v with
	| Atom _ -> s
	| Arrow (id1,id2,arr_t) ->
           (* A binding is not really a connection *)
           if arr_t <> Binding then (
	   let (id1,id2) = (Lazy.force id1,Lazy.force id2) in
	   let ({id=_;v=v},e2) =
             (find_from_id id1 objects,find_from_id id2 objects) in
           match v with
           | Atom str1 ->
	      let {id=_;v=v2} = e2 in
	      let atoms1 = StringMap.find str1 atoms_ids in
	      let atoms2 = match v2 with
	        | Atom str2 -> StringMap.find str2 atoms_ids
	        | _ -> IntSet.singleton id2
	      in
	      IntSet.fold
	        (fun id1 s1 ->
	          IntSet.fold
                    (fun id2 s2 -> IntIntSet.add (id1,id2) s2) atoms2 s1)
	        atoms1 s
           | _ -> failwith "Pattern matchin failed here")
           else s
      )
      objects IntIntSet.empty in
  let origs = ref StringSet.empty in
  GraphSet.iter
    (fun {id=id1;v=v1} ->
      match v1 with
      | Atom s1 ->
	 if not (StringSet.mem s1 !origs) then (
	   origs := StringSet.add s1 !origs;
	   GraphSet.iter
	     (fun {id=id2;v=v2} ->
               (* Never adds an arrow to a binding *)
               let cmp = match v2 with
                 | Arrow (_,_,Binding) -> false
                 | _ -> true in
	       if cmp && (id1<>id2) && (not (IntIntSet.mem (id1,id2) connections)) then (
		 add_arrow id1 id2 Stimulation;
		 match v2 with
		 | Arrow _ ->
		    add_arrow id1 id2 Inhibition;
		 | Atom s2 ->
		    (* Do not add production rule for exogenous variables *)
		    if not (StringSet.mem s2 exos) then add_arrow id1 id2 Conversion;
		    ())
	     )
	     objects)
      | _ -> ())
    objects;
  pdebug true stdout "Graphs tested:%d\n" !nb;;

  
