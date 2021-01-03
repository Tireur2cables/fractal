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
  | s -> Symb (String.make 1 (String.get s 0)) :: (axiom_of_string (String.sub s 1 ((String.length s)-1) ) );;

let rec string_of_word w =
  match w with
  | Symb s -> s
  | Seq s -> (match s with
        | [] -> ""
        | s :: l -> (string_of_word s) ^ string_of_word (Seq l)
      )
  | Branch b -> string_of_word b
;;

let rec word_of_string s =
  match s with
  | "" -> Symb ""
  | s ->  if String.length s = 1 then ( Symb s ) else ( Seq(Symb (String.make 1 (String.get s 0)) :: (word_of_string (String.sub s 1 ((String.length s)-1))) :: []))

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
    | None -> word_of_string s
    | Some r -> word_of_string r
    )
;;

let create_interp inter =
  let rules_list = String.split_on_char ':' inter in
  let rules_pair = create_pair rules_list in
  let fun_aux (s1,s2) =
    match String.get s2 0 with
    | 'L' -> (s1, [Turtle.Line (int_of_string (String.sub s2 1 ((String.length s2)-1)))])
    | 'M' -> (s1, [Turtle.Move (int_of_string (String.sub s2 1 ((String.length s2)-1)))])
    | 'T' -> (s1, [Turtle.Turn (int_of_string (String.sub s2 1 ((String.length s2)-1)))])
    | _ -> failwith "erreur"
  in
  let paires = List.map fun_aux rules_pair in
  function s ->
    begin
      match List.assoc_opt s paires with
      | None -> failwith "erreur liste pas complete"
      | Some r -> r
    end
;;

let interpret_file file =
   let canal_in = open_in file in
   let (axiom, rules, inter) = interpret_line canal_in 0 "" "" "" in
   let ax = Seq (axiom_of_string axiom) in
   let rules = create_rules rules in
   let interp = create_interp inter in
   close_in canal_in;
   {axiom = ax; rules = rules; interp = interp}
;;
