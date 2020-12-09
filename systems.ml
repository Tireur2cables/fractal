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
let rec interpret_line file block =
  try
    let x = input_line file in
    if is_comment x then (
      interpret_line file block;
    )
    else (
      if is_space x then (
        print_string "\n";
        interpret_line file (block+1);
      )else (
        print_int block;
        print_string (" " ^ x);
        print_string "\n";
        interpret_line file block;
      );
    );
  with End_of_file -> print_string "fin";
;;

let interpret_file file =
   let canal_in = open_in file in
   interpret_line canal_in 0;
   close_in canal_in;
;;


