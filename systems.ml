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
  String.get line 0 == '#') else false
;;

let rec interprete_ligne ci =
  try
    let x = input_line ci in
    if is_comment x then ()
    else (
      print_string x;
      print_string "\n";
    );
    interprete_ligne ci;
  with End_of_file -> print_string "fin";
;;

let interprete file =
   let canal_in = open_in file in
   interprete_ligne canal_in;
   close_in canal_in;
;;


