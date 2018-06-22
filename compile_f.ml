type doa = {
  states: string list;
  choices: string list;
  methods: string list;
  labels: string list;
  start: string;
  final: string list;
  method_trans: t_elem list;
  label_trans: t_elem list
}
and t_elem = {
  init: string;
  trans: string;
  fin: string;
};;

type automaton = Nil | DOA of doa;;


type typestate = e_state list
and e_state = {
  name: string;
  transitions: transition list
}
and transition = {
  op: string;
  result: w
}
and w =
  | NextState of string
  | Option of option list
  | InnerState of inner_state
and option = {
  label: string;
  state: w;
}
and inner_state = transition list
;;


let c = ref 0;;
let i = ref 0;;

let next_val p =
  p := (!p) + 1;
  !p
;;

let next_choice () = "choice:"^string_of_int (next_val c);;
let next_inner () = "inner:"^string_of_int (next_val i);;


let ts = [
	{ name = "Init"; transitions =
		[ { op = "Status open()"; result = Option [{ label = "OK"; state = NextState "Open" }; { label = "ERROR"; state = NextState "end" }] } ]
	};
	{ name = "Open"; transitions =
		[ { op = "Boolean hasNext()"; result = Option [{ label = "TRUE"; state = NextState "Read" }; { label = "FALSE"; state = NextState "Close" }] };
			{ op = "void close()"; result = NextState "end" } ]
	};
	{ name = "Read"; transitions =
		[ { op = "void read()"; result = NextState "Open" } ]
	};
	{ name = "Close"; transitions =
		[ { op = "void close()"; result = NextState "end" } ]
	};
];;


let rec belongs x l =
  match l with
  | [] -> false
  | hd::tl ->
    if x = hd then true
    else belongs x tl
;;

let rec list_union l1 l2 =
  match l1 with
  | [] -> l2
  | x::xs ->
    if belongs x l2 then list_union xs l2
    else x::list_union xs l2
;;


let rec available_states ts =
  match ts with
  | [] -> ["end"]
  | s::next ->
    let v = (available_states next) in
      if s.name = "end" then failwith "State with name 'end' not allowed."
      else if belongs s.name v then let err = "Found duplicate state: "^s.name in failwith err
      else s.name::v
;;
let avs = ref (available_states ts);;

let add_avs s =
  avs := s::!avs
;;


let rec duplicate_trans t =
  match t with
  | [] -> []
  | x::xs ->
    let sl = (findPairs xs) in
      if belongs (x.init, x.trans) sl then let err = "Found duplicate method or label: "^x.trans in failwith err
      else x::(duplicate_trans xs)
and findPairs l =
  match l with
  | [] -> []
  | y::ys -> (y.init, y.trans)::(findPairs ys)
;;


let union b a =
  match b, a with
  | Nil, Nil -> Nil
  | Nil, DOA d
  | DOA d, Nil -> DOA { states = list_union d.states ["end"]; choices = d.choices; methods = d.methods; labels = d.labels; start = d.start;
                        final = list_union d.final ["end"]; method_trans = d.method_trans; label_trans = d.label_trans }
  | DOA b, DOA a -> DOA { states = list_union a.states b.states; choices = list_union a.choices b.choices; methods = list_union a.methods b.methods;
                          labels = list_union a.labels b.labels; start = a.start; final = list_union a.final b.final;
                          method_trans = duplicate_trans (a.method_trans @ b.method_trans); label_trans = duplicate_trans (a.label_trans @ b.label_trans) }
;;


let rec compile_typestate t =
  match t with
  | [] -> Nil
  | a::body -> union (compile_typestate body) (compile_state_def a)


and compile_state_def s =
  match s.transitions with
  | [] -> DOA { states = [s.name]; choices = []; methods = []; labels = []; start = s.name; final = [s.name]; method_trans = []; label_trans = [] }
  | x::xs -> compile_state s.name x xs


and compile_state name first next =
  match next with
  | [] -> compile_method name first
  | x::xs -> union (compile_state name x xs) (compile_method name first)


and compile_method name m =
  match m.result with
  | NextState "{}"
  | NextState "end" -> DOA { states = [name; "end"]; choices = []; methods = [m.op]; labels = []; start = name;
                             final = ["end"]; method_trans = [{ init = name; trans = m.op; fin = "end" }]; label_trans = [] }
  | NextState next -> if (belongs next !avs) then
                        DOA { states = if name = next then [name] else [name;next]; choices = []; methods = [m.op]; labels = []; start = name;
                              final = []; method_trans = [{ init = name; trans = m.op; fin = next }]; label_trans = [] }
                      else let err = "Undefined state: "^next in failwith err
  | InnerState inner -> let next = next_inner() in
                          (add_avs next; let trans = { op = m.op; result = NextState next } and
                             nextState = { name = next; transitions = inner } in
                                union (compile_state_def nextState) (compile_method name trans) )
  | Option opt -> compile_options name m.op opt


and compile_options name met options =
  match options with
  | [] -> invalid_arg "There must be at least an option"
  | o::tl -> let choice = next_choice() in
              let a = DOA { states = [name]; choices = [choice]; methods = [met]; labels = []; start = name;
                            final = []; method_trans = [{ init = name; trans = met; fin = choice }]; label_trans = [] } in
                union (compile_label_options choice o tl) a


and compile_label_options name first next =
  match next with
  | [] -> compile_label name first
  | nxt::tl -> union (compile_label_options name nxt tl) (compile_label name first)


and compile_label name opt =
  let l = opt.label and s = opt.state in
  match s with
  | NextState "{}"
  | NextState "end" -> DOA { states = ["end"]; choices = [name]; methods = []; labels = [l]; start = "";
                             final = ["end"]; method_trans = []; label_trans = [{ init = name; trans = l; fin = "end" }] }
  | NextState next -> if (belongs next !avs) then
                        DOA { states = [next]; choices = [name]; methods = []; labels = [l]; start = "";
                              final = []; method_trans = []; label_trans = [{ init = name; trans = l; fin = next }] }
                      else let err = "Undefined state: "^next in failwith err
  | InnerState inner -> let next = next_inner() in
                          (add_avs next; let nextState = { name = next; transitions = inner } and
                            option = { label = l; state = NextState next } in
                              union (compile_state_def nextState) (compile_label name option) )
  | Option _ -> failwith "Internal choice states must always transition to external choice states."
;;

available_states ts;;
compile_typestate ts;;
