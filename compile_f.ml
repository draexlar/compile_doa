type doa = { 
	states: string list; 
	choices: string list; 
	methods: string list; 
	labels: string list; 
	start: string; 
	final: string list; 
	mTransitions: string list; 
	lTransitions: string list
};;

type automaton = Nil | DOA of doa;;


type option = {
	label: string;
	state: string;
}
and iState = transition list
and w = 
	| NextState of string
	| Option of option list
	| InnerState of iState
and
transition = { 
	op: string; 
	result: w 
}
and
eState = { 
	name: string; 
	transitions: transition list 
}
and typestate = eState list;;


let c = ref 0;;
let i = ref 0;;

let ts = [
	{ name = "Init"; transitions = 
		[ { op = "open"; result = Option [{ label = "OK"; state = "Open" }; { label = "ERROR"; state = "end" }] } ] 
	};
	{ name = "Open"; transitions = 
		[ { op = "hasNext"; result = Option [{ label = "TRUE"; state = "Read" }; { label = "FALSE"; state = "Close" }] }; 
			{ op = "close"; result = NextState "end" } ] 
	};
	{ name = "Read"; transitions = 
		[ { op = "read"; result = NextState "Open" } ] 
	};
	{ name = "Close"; transitions = 
		[ { op = "close"; result = NextState "end" } ] 
	};
];;


let rec availableStates ts =
	match ts with
	| [] -> ["end"]
	| s::tl -> s.name::(availableStates tl)
;;
let avs = ref (availableStates ts);;

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

(*let rec listInter l2 =
	match avs with
	| [] -> []
	| x::xs ->
		if belongs x l2 then x::listInter xs l2
		else inter xs l2
;;*)

(*let cleanAut a =
	{ states *)

let union a b =
	match a, b with
	| Nil, Nil -> Nil
	| Nil, DOA d
	| DOA d, Nil -> DOA d
	| DOA x, DOA y -> DOA { states = listUnion x.states y.states; choices = listUnion x.choices y.choices; methods = listUnion x.methods y.methods;
											labels = listUnion x.labels y.labels; start = x.start; final = listUnion x.final y.final; mTransitions = x.mTransitions @ y.mTransitions; 
											lTransitions = x.lTransitions @ y.lTransitions }
;;


let rec compileOptionsNext name options =
	match options with
	| [] -> Nil
	| o::tl -> let l = o.label and s = o.state in
								if s = "end" then 
										union (DOA { states = [s]; choices = [name]; methods = []; labels = [l]; start = ""; 
												final = [s]; mTransitions = []; lTransitions = ["T("^name^", "^l^") = "^s] }) (compileOptionsNext name tl)
								else
										if belongs s !avs then
												union (DOA { states = [s]; choices = [name]; methods = []; labels = [l]; start = ""; 
														final = []; mTransitions = []; lTransitions = ["T("^name^", "^l^") = "^s] }) (compileOptionsNext name tl)
										else failwith "Undefined state"
;;

let compileOptions name options =
	match options with
	| [] -> invalid_arg "There must at least be an option"
	| _ -> compileOptionsNext name options
;;

let rec compileMethod name m =
	match m.result with
	| NextState "end" -> DOA { states = [name; "end"]; choices = []; methods = [m.op]; labels = []; start = name; 
												final = ["end"]; mTransitions = ["D("^name^", "^m.op^") = end"]; lTransitions = [] }
	| NextState next -> if (belongs next !avs) then
													DOA { states = [name; next]; choices = []; methods = [m.op]; labels = []; start = name;
																final = []; mTransitions = ["D("^name^", "^m.op^") = "^next]; lTransitions = [] }
											else failwith "Undefined state"
	| InnerState inner -> let x = (i := !i + 1) and next = "inner"^string_of_int !i in
														let trans = { op = m.op; result = NextState (next) } and nextState = { name = next; transitions = inner } 
																and y = (avs := next::!avs) in
																union (compileMethod name trans) (compileStateDef nextState)
	| Option opt -> let x = (c := !c + 1) and choice = "choice"^string_of_int !c in 
											let a = DOA { states = [name]; choices = [choice]; methods = [m.op]; labels = []; start = name; 
													final = []; mTransitions = ["D("^name^", "^m.op^") = "^choice]; lTransitions = [] } in
															union a (compileOptions choice opt)

and compileState name first next =
	match next with
	| [] -> compileMethod name first
	| x::xs -> union (compileMethod name first) (compileState name x xs)


and compileStateDef s =
	match s.transitions with
	| [] -> DOA { states = [s.name]; choices = []; methods = []; labels = []; start = s.name; final = [s.name]; mTransitions = []; lTransitions = [] }
	| x::xs -> compileState s.name x xs


and compileTypestate t =
	match t with
	| [] -> Nil
	| a::body -> union (compileStateDef a) (compileTypestate body)
;;

availableStates ts;;
compileTypestate ts;;