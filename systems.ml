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

(** Here are the function concerning files interpretation *)

(**
  Return true if line is a comment in sys file
  false otherwise
*)
let is_comment (line :string) : (bool) =
  if (String.length line) > 0
  then line.[0] = '#'
  else false
;;
(**
  Return true if line is a spacing line in sys file
  false otherwise
*)
let is_space (line :string) : (bool) =
  if (String.length line) = 0
  then true
  else line.[0] = '\n';
;;

(**
  Interpret lines of the open channel "file", creating the three piece
  of a L-System, depending of the three blocks the sys file is formatted in
*)
let rec interpret_line (file :in_channel) (block :int) (axiom :string)
        (rules :string) (inter :string) : (string * string * string)  =
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

(** Here are the functions concerning the making of L-Systems function *)

(**
  Convert the string "s" into a word (as the word type)
*)
let word_of_string (s :string) : ('s word) =

  let rec seq_of_string (s :'s) (res : 's word list) : ('s word list * 's) =
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
  | "" -> failwith "Erreur : axiom is empty"
  | s ->
     if String.length s = 1 then Symb s
     else
       let (res, str) = seq_of_string s [] in
       Seq res
;;
(**
  Convert the word "w" into a string
*)
let rec string_of_word (w :'s word) : (string) =
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

let rec create_paires (l : 's list) : (('s * 's) list) =
  match l with
  | [] -> []
  | w :: l ->
     let res = String.split_on_char ' ' w in
  	 if List.length res = 1 then [] (* Wrong format or end of line *)
     else [(List.nth res 0, List.nth res 1)] @ (create_paires l)
;;

let make_match_function action paires =
  fun s ->
  begin
    match List.assoc_opt s paires with
    | None -> action s
    | Some r -> r
  end
;;

(**
  Return the rewrites_rules function of the L-System, that the
  string "rules" describ as a sequence of couples, each
  separated by ":"
*)
let create_rules (rules :string) : ('s rewrite_rules) =
  let rules_list = String.split_on_char ':' rules in
  let rules_paires = create_paires rules_list in

  let transform (s1, s2) =
    (s1, word_of_string s2)
  in

  let paires = List.map transform rules_paires in
  make_match_function word_of_string paires
;;

(**
  Return the interpretation function of the L-System, that the
  string "inter" describ as a sequence of couples, each
  separated by ":"
*)
let create_interp (inter :string) : ('s -> Turtle.command list) =
  let inter_list = String.split_on_char ':' inter in
  let inter_paires = create_paires inter_list in

  let transform (s1,s2) =
    let distance_str = String.sub s2 1 ((String.length s2)-1) in
    let distance = int_of_string distance_str in
    match String.get s2 0 with
    | 'L' -> (s1, [Turtle.Line distance])
    | 'M' -> (s1, [Turtle.Move distance])
    | 'T' -> (s1, [Turtle.Turn distance])
    | _ -> failwith "Turtle error : Unknown turtle command"
  in

  let paires = List.map transform inter_paires in

  let action (s :string) : (Turtle.command list) =
    match s with
    | "[" -> [Turtle.Store]
    | "]" -> [Turtle.Restore]
    | _ -> failwith ("Error " ^ s ^ " is not in the interpretations list")
  in

  make_match_function action paires
;;

(**
  Return a triplet containing the axiom, rules of transformation and rules of
  interpretation of the string "file" describing the path of a
  formatted sys file
*)
let interpret_file (file : string) : ('s system) =
   let canal_in = open_in file in
   let (ax_str, rules_str, inter_str) = interpret_line canal_in 0 "" "" "" in
   let axiom = word_of_string ax_str in
   let rules = create_rules rules_str in
   let interp = create_interp inter_str in
   close_in canal_in;
   {axiom = axiom; rules = rules; interp = interp}
;;
