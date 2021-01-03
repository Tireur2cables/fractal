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
  if (String.length line) > 0
  then line.[0] = '#'
  else false
;;

let is_space line =
  if (String.length line) = 0
  then true
  else line.[0] = '\n';
;;

let rec interpret_line file block axiom rules inter  =
  try
    let x = input_line file in
    if is_comment x
    then interpret_line file block axiom rules inter

    else if is_space x
    then interpret_line file (block+1) axiom rules inter

    else if block = 0
    then interpret_line file block (axiom ^ x) rules inter

    else if block = 1
    then interpret_line file block axiom (rules ^ x ^ ":") inter

    else interpret_line file block axiom rules (inter ^ x ^ ":")

  with End_of_file -> (axiom, rules, inter)
;;

let word_of_string s =
  let rec seq_of_string s res =
    match s with
    | "" -> (res, s)
    | s ->
       let sub = String.sub s 1 (String.length s - 1) in
       match s.[0] with
       | '[' ->
          begin
            let (newres, news) = seq_of_string sub [] in
            match newres with
            | [] -> seq_of_string news res
            | w :: [] -> seq_of_string news (res @ [Branch (w)])
            | _ -> seq_of_string news (res @ [Branch (Seq (newres))])
          end
       | ']' -> (res, sub)
       | c -> seq_of_string sub (res @ [Symb (String.make 1 c)])
  in
  match s with
  | "" -> failwith "Erreur l'axiome est vide!"
  | s ->
     if String.length s = 1 then Symb s
     else
       let (res, str) = seq_of_string s [] in
       Seq res
;;

let rec string_of_word w =
  match w with
  | Symb s -> s
  | Seq s ->
     begin
       match s with
       | [] -> ""
       | s :: l -> (string_of_word s) ^ string_of_word (Seq l)
     end
  | Branch b -> "[" ^ (string_of_word b) ^ "]"
;;

let rec create_paires list =
  match list with
  | [] -> []
  | w :: list ->
     let res = String.split_on_char ' ' w in
  	 if List.length res = 1 then [] (* mauvais formattage ou fin de ligne *)
     else [(List.nth res 0, List.nth res 1)] @ (create_paires list)
;;

let make_match_function action paires =
  fun s ->
  begin
    match List.assoc_opt s paires with
    | None -> action s
    | Some r -> r
  end
;;

let create_rules rules =
  let rules_list = String.split_on_char ':' rules in
  let rules_paires = create_paires rules_list in
  let transform (s1, s2) =
    (s1, word_of_string s2)
  in
  let paires = List.map transform rules_paires in
  make_match_function word_of_string paires
;;

let create_interp inter =
  let inter_list = String.split_on_char ':' inter in
  let inter_paires = create_paires inter_list in
  let transform (s1,s2) =
    match String.get s2 0 with
    | 'L' -> (s1, [Turtle.Line (int_of_string (String.sub s2 1 ((String.length s2)-1)))])
    | 'M' -> (s1, [Turtle.Move (int_of_string (String.sub s2 1 ((String.length s2)-1)))])
    | 'T' -> (s1, [Turtle.Turn (int_of_string (String.sub s2 1 ((String.length s2)-1)))])
    | _ -> failwith "Erreur commande de tortue inconnue!"
  in
  let paires = List.map transform inter_paires in
  let action s =
    match s with
    | "[" -> [Turtle.Store]
    | "]" -> [Turtle.Restore]
    | _ -> failwith ("Erreur " ^ s ^ " n'est pas dans la liste des interprétations!")
  in
  make_match_function action paires
;;

let interpret_file file =
   let canal_in = open_in file in
   let (axiom_str, rules_str, interp_str) = interpret_line canal_in 0 "" "" "" in
   let axiom = word_of_string axiom_str in
   let rules = create_rules rules_str in
   let interp = create_interp interp_str in
   close_in canal_in;
   {axiom = axiom; rules = rules; interp = interp}
;;
