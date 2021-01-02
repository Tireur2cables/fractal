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
          interpret_line file block axiom (rules ^ x) inter;
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

let interpret_file file =
   let canal_in = open_in file in
   let (axiom, rules, inter) = interpret_line canal_in 0 "" "" "" in
   let inter_list = String.split_on_char ':' inter in
   close_in canal_in;
;;


