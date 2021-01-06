open Lsystems;;
open Systems;;
open Turtle;;
open Graphics;;

(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)


let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"
;;

let action_what () =
  Printf.printf "%s\n" usage;
  exit 0
;;

let cmdline_options = [
("--what" , Arg.Unit action_what, "description");
]
;;

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s);;


let open_window w h =
  open_graph (" " ^ (string_of_int w) ^ "x" ^ (string_of_int h));
  auto_synchronize true
;;

let close_after_event () =
  try
    ignore (wait_next_event [Button_down ; Key_pressed])
  with
    Graphic_failure s -> exit 0
;;

let try_exec (t: turtle) (l: command list) draw: (turtle) =
  try
    exec_commands t l draw
  with
  | Restoration_failure s ->
     print_string s;
     create_turtle ()
;;

let rec calc_aux turtle system degre symbol (h,v) =
  if degre = 0 then
    calc_commands turtle (system.interp symbol) (h,v)
  else
	let rec fun_aux turtle (h,v) s =
	  match s with
	  | "" -> (h,v, turtle)
	  | s ->
         let sub = String.sub s 1 (String.length s - 1) in
		 let (x , y, tort) = calc_aux turtle system (degre-1) (String.make 1 s.[0]) (h,v) in
         fun_aux tort (x,y) sub
	in
    let res = string_of_word (system.rules symbol) in (* ATTENTION : mise en mémoire des itérations *)
    fun_aux turtle (h,v) res
;;

let calc turtle system degre curr_dim =
	let rec fun_aux turtle curr_dim s =
		match s with
		| "" -> (curr_dim, turtle)
		| s ->
           let sub = String.sub s 1 (String.length s - 1) in
		   let (x,y, tort) = calc_aux turtle system degre (String.make 1 s.[0]) curr_dim in
           fun_aux tort (x,y) sub
	in
	fun_aux turtle curr_dim (string_of_word system.axiom)
;;

let rec rewrite_aux turtle system degre symbol draw =
  if degre = 0 then
    exec_commands turtle (system.interp symbol) draw
  else
	let rec fun_aux turtle s =
	  match s with
	  | "" -> turtle
	  | s ->
         let sub = String.sub s 1 (String.length s - 1) in
         fun_aux (rewrite_aux turtle system (degre-1) (String.make 1 s.[0]) draw) sub
	in
    let res = string_of_word (system.rules symbol) in (* ATTENTION : mise en mémoire des itérations *)
    fun_aux turtle res
;;

let rewrite turtle system degre draw =
	let rec fun_aux turtle s =
		match s with
		| "" -> turtle
		| s ->
           let sub = String.sub s 1 (String.length s - 1) in
           fun_aux (rewrite_aux turtle system degre (String.make 1 s.[0]) draw) sub
	in
	fun_aux turtle (string_of_word system.axiom)
;;

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  open_window 1000 1000;
  let system = interpret_file "./examples/snow.sys" in
  let turtle2 = (create_turtle ()) in
  let ((x,y), turtlef) = calc turtle2 system 4 (0.,0.) in
  print_float (x);
  print_string "\n";
  print_float (y);
  let z = min x y in
  let turtle = (create_turtle ()) in
  let turle_fin = rewrite turtle system 4 (400./.z,400./.z) in
  close_after_event ()
;;
(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)
let () = if not !Sys.interactive then main ();;
