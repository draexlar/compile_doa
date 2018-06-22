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

let find_start l =
	match l with
	| [] -> ""
	| x::xs -> x
;;

let remove_elem s d =
  let new_states = List.filter(fun x -> x <> s) d.states in
  {states = new_states; choices = d.choices; methods = d.methods;
   labels = d.labels; start = find_start new_states; final = d.final;
   method_trans = List.filter(fun x -> x.init <> s) d.method_trans;
   label_trans = d.label_trans}
;;


let rec decompile name d =
  print_string ("typestate " ^ name ^ " {\n" ^ decompile_doa d ^ "\n}\n")

and decompile_doa d =
  match d.states with
  | [] -> ""
  | _ -> let s = d.start in
    let a = List.filter(fun x -> x.init = s) d.method_trans in
    "\t" ^ s ^ " = {\n\t\t" ^ decompile_state s d a ^ "\n\t}\n" ^ decompile_doa (remove_elem s d)

and decompile_state c d a =
  match a with
  | [] -> ""
  | x::xs ->
    match xs with
    | [] -> if x.init = c then x.trans ^ ": " ^ decompile_next x.fin d else ""
    | _ -> if x.init = c then x.trans ^ ": " ^ decompile_next x.fin d ^ ",\n\t\t" ^ decompile_state c d xs else ""

and decompile_next n d =
  if List.exists(fun x -> x = n) d.states then n
  else if List.exists(fun x -> x = n) d.choices then "<" ^ decompile_choice n (List.filter(fun x -> x.init = n) d.label_trans) ^ ">"
  else let err = "Undefined state: " ^ n in failwith err

and decompile_choice c b =
  match b with
  | [] ->  invalid_arg "DOA not well defined. Internal choice states must have at least an option"
  | x::xs ->
    match xs with
    | [] -> if x.init = c then x.trans ^ ": " ^ x.fin else ""
    | _ -> if x.init = c then x.trans ^ ": " ^ x.fin ^ ", " ^ decompile_choice c xs else ""
;;

let ex = {states = ["Init"; "Read"; "Open"; "Close"; "end"];
  choices = ["choice:1"; "choice:2"];
  methods =
   ["Status open()"; "Boolean hasNext()"; "void read()"; "void close()"];
  labels = ["OK"; "ERROR"; "TRUE"; "FALSE"]; start = "Init"; final = ["end"];
  method_trans =
   [{init = "Init"; trans = "Status open()"; fin = "choice:1"};
    {init = "Open"; trans = "Boolean hasNext()"; fin = "choice:2"};
    {init = "Open"; trans = "void close()"; fin = "end"};
    {init = "Read"; trans = "void read()"; fin = "Open"};
    {init = "Close"; trans = "void close()"; fin = "end"}];
  label_trans =
   [{init = "choice:1"; trans = "OK"; fin = "Open"};
    {init = "choice:1"; trans = "ERROR"; fin = "end"};
    {init = "choice:2"; trans = "TRUE"; fin = "Read"};
    {init = "choice:2"; trans = "FALSE"; fin = "Close"}]};;

decompile "FileProtocol" ex;;
