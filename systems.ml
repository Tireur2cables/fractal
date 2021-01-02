(** Words, rewrite systems, and rewriting *)

type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word

type 's rewrite_rules = 's -> 's word

type 's system = {
    axiom : 's word;
    rules : 's rewrite_rules;
    interp : 's -> Turtle.command list }

(** Put here any type and function implementations concerning systems *)

type symbol = A|P|M

let system : symbol system =
  let a = Symb A in
  let p = Symb P in
  let m = Symb M in
  {
    axiom = Seq [a;p;p;a;p;p;a];
    rules =
      (function
       | A -> Seq [a;m;a;p;p;a;m;a]
       | s -> Symb s);
    interp =
      (function
       | A -> [Line 30]
       | P -> [Turn 60]
       | M -> [Turn (-60)])
  }

(**
let rec word_map rules word =
  match word with
  | Symb s -> rules s
  | Seq s ->
    (match s with
     | [] -> []
     | w :: s -> word_map rules w @ word_map rules (Seq (s))
    )
  | Branch b -> word_map rules b
;;

let rewrite axiom rules degre =
  if degre <= 1 then (
    axiom;
  )else (
    match axiom with
    | Symb s -> rules s
    | Seq s -> word_map rules s
    | Branch b -> rewrite b rules degre
  )
;;
*)

let is_comment line =
  if (String.length line) > 0 then (
  String.get line 0 = '#') else false
;;

let is_space line =
  if (String.length line) = 0 then
    true else String.get line 0 = '\n';
;;

let rec interpret_line file block axiom rules inter  =
  try
    let x = input_line file in
    if is_comment x then (
      interpret_line file block axiom rules inter;
    )
    else (
      if is_space x then (
        interpret_line file (block+1) axiom rules inter;
      )else (
        if block = 0 then(
          interpret_line file block (axiom ^ x) rules inter;
        )else if block = 1 then (
          interpret_line file block axiom (rules ^ x ^ ":") inter;
        )
        else(
          interpret_line file block axiom rules (inter ^ x ^ ":");
        )
      );
    );
  with End_of_file -> (
      (axiom, rules, inter);
    )
;;

let rec axiom_of_string s =
  match s with
  | "" -> []
  | s -> Symb (String.get s 0) :: (axiom_of_string (String.sub s 1 ((String.length s)-1) ) );;

let rec create_pair rules_list =
  match rules_list with
  | [] -> []
  | w :: list -> let res = String.split_on_char ' ' w in
  	if List.length res = 1 then [] else
    [(List.nth res 0, List.nth res 1)] @ create_pair list
;;

let create_rules rules =
  let rules_list = String.split_on_char ':' rules in
  let rules_pair = create_pair rules_list in
  function s -> (match List.assoc_opt s rules_pair with
    | None -> s
    | Some r -> r
    )
;;

let create_interp inter =
  match String.get inter 0 with
  | 'L' -> Turtle.Line (int_of_string (String.sub inter 1 ((String.length inter)-1)))
  | 'M' -> Turtle.Move (int_of_string (String.sub inter 1 ((String.length inter)-1)))
  | 'T' -> Turtle.Turn (int_of_string (String.sub inter 1 ((String.length inter)-1)))
  | _ -> failwith "erreur"
;;

let interpret_file file =
   let canal_in = open_in file in
   let (axiom, rules, inter) = interpret_line canal_in 0 "" "" "" in
   let ax = Seq (axiom_of_string axiom) in
   let ruless = create_rules rules in
   let interp = create_interp (create_rules inter) in
   print_string axiom;
   print_string rules;
   print_string inter;
   close_in canal_in;
;;
