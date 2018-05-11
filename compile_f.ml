type doa = {
  states: string list;
  choices: string list;
  methods: string list;
  labels: string list;
  start: string;
  final: string list;
  mTransitions: t_elem list;
  lTransitions: t_elem list
}
and t_elem = {
  init: string;
  trans: string;
  fin: string;
};;

type automaton = Nil | DOA of doa;;


type typestate = eState list
and eState = {
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
  | InnerState of iState
and option = {
  label: string;
  state: w;
}
and iState = transition list
;;


let c = ref 0;;
let i = ref 0;;

let next_val p =
  p := (!p) + 1;
  !p
;;


let ts = [
  { name = "Init"; transitions =
                     [ { op = "open"; result = Option [{ label = "OK"; state = NextState "Open" }; { label = "ERROR"; state = NextState "end" }] } ]
  };
  { name = "Open"; transitions =
                     [ { op = "hasNext"; result = Option [{ label = "TRUE"; state = NextState "Read" }; { label = "FALSE"; state = NextState "Close" }] };
                       { op = "close"; result = NextState "end" } ]
  };
  { name = "Read"; transitions =
                     [ { op = "read"; result = NextState "Open" } ]
  };
  { name = "Close"; transitions =
                      [ { op = "close"; result = NextState "end" } ]
  };
];;


let rec belongs x l =
  match l with
  | [] -> false
  | hd::tl ->
    if x = hd then true
    else belongs x tl
;;

let rec listUnion l1 l2 =
  match l1 with
  | [] -> l2
  | x::xs ->
    if belongs x l2 then listUnion xs l2
    else x::listUnion xs l2
;;


let rec availableStates ts =
  match ts with
  | [] -> ["end"]
  | s::next ->
    let v = (availableStates next) in
      if s.name = "end" then failwith "State with name 'end' not allowed."
      else if belongs s.name v then let err = "Found duplicate state: "^s.name in failwith err
      else s.name::v
;;
let avs = ref (availableStates ts);;

let add_avs s =
  avs := s::!avs
;;


let rec duplicateTrans t =
  match t with
  | [] -> []
  | x::xs ->
    let sl = (findPairs xs) in
      if belongs (x.init, x.trans) sl then let err = "Found duplicate method or label: "^x.trans in failwith err
      else x::(duplicateTrans xs)
and findPairs l =
  match l with
  | [] -> []
  | y::ys -> (y.init, y.trans)::(findPairs ys)
;;


let union b a =
  match b, a with
  | Nil, Nil -> Nil
  | Nil, DOA d
  | DOA d, Nil -> DOA { states = listUnion d.states ["end"]; choices = d.choices; methods = d.methods; labels = d.labels; start = d.start;
                        final = listUnion d.final ["end"]; mTransitions = d.mTransitions; lTransitions = d.lTransitions }
  | DOA b, DOA a -> DOA { states = listUnion a.states b.states; choices = listUnion a.choices b.choices; methods = listUnion a.methods b.methods;
                          labels = listUnion a.labels b.labels; start = a.start; final = listUnion a.final b.final;
                          mTransitions = duplicateTrans (a.mTransitions @ b.mTransitions); lTransitions = duplicateTrans (a.lTransitions @ b.lTransitions) }
;;


let rec compileTypestate t =
  match t with
  | [] -> Nil
  | a::body -> union (compileTypestate body) (compileStateDef a)


and compileStateDef s =
  match s.transitions with
  | [] -> DOA { states = [s.name]; choices = []; methods = []; labels = []; start = s.name; final = [s.name]; mTransitions = []; lTransitions = [] }
  | x::xs -> compileState s.name x xs


and compileState name first next =
  match next with
  | [] -> compileMethod name first
  | x::xs -> union (compileState name x xs) (compileMethod name first)


and compileMethod name m =
  match m.result with
  | NextState "{}"
  | NextState "end" -> DOA { states = [name; "end"]; choices = []; methods = [m.op]; labels = []; start = name;
                             final = ["end"]; mTransitions = [{ init = name; trans = m.op; fin = "end" }]; lTransitions = [] }
  | NextState next -> if (belongs next !avs) then
                        DOA { states = if name = next then [name] else [name;next]; choices = []; methods = [m.op]; labels = []; start = name;
                              final = []; mTransitions = [{ init = name; trans = m.op; fin = next }]; lTransitions = [] }
                      else let err = "Undefined state: "^next in failwith err
  | InnerState inner -> let next = "inner"^string_of_int (next_val i) in
                          (add_avs next; let trans = { op = m.op; result = NextState next } and 
                             nextState = { name = next; transitions = inner } in
                                union (compileStateDef nextState) (compileMethod name trans) )
  | Option opt -> compileOptions name m.op opt


and compileOptions name met options =
  match options with
  | [] -> invalid_arg "There must be at least an option"
  | o::tl -> let choice = "choice"^string_of_int (next_val c) in
              let a = DOA { states = [name]; choices = [choice]; methods = [met]; labels = []; start = name;
                            final = []; mTransitions = [{ init = name; trans = met; fin = choice }]; lTransitions = [] } in
                union (compileLabelOptions choice o tl) a


and compileLabelOptions name first next =
  match next with
  | [] -> compileLabel name first
  | nxt::tl -> union (compileLabelOptions name nxt tl) (compileLabel name first)


and compileLabel name opt =
  let l = opt.label and s = opt.state in
  match s with
  | NextState "{}"
  | NextState "end" -> DOA { states = ["end"]; choices = [name]; methods = []; labels = [l]; start = "";
                             final = ["end"]; mTransitions = []; lTransitions = [{ init = name; trans = l; fin = "end" }] }
  | NextState next -> if (belongs next !avs) then
                        DOA { states = [next]; choices = [name]; methods = []; labels = [l]; start = "";
                              final = []; mTransitions = []; lTransitions = [{ init = name; trans = l; fin = next }] }
                      else let err = "Undefined state: "^next in failwith err
  | InnerState inner -> let next = "inner"^string_of_int (next_val i) in
                          (add_avs next; let nextState = { name = next; transitions = inner } and
                            option = { label = l; state = NextState next } in
                              union (compileStateDef nextState) (compileLabel name option) )
  | Option _ -> failwith "Internal choice states must always transition to external choice states."
;;

availableStates ts;;
compileTypestate ts;;
