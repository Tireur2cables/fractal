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

let try_exec (t: turtle) (l: command list) : (turtle) =
  try
    exec_commands t l
  with
  | Restoration_failure s ->
     print_string s;
     create_turtle ()
;;

let rec rewrite_aux turtle system degre symbol =
  if degre = 0 then
    exec_commands turtle (system.interp symbol)
  else
	let rec fun_aux turtle s =
	  match s with
	  | "" -> turtle
	  | s ->
         let sub = String.sub s 1 (String.length s - 1) in
         fun_aux (rewrite_aux turtle system (degre-1) (String.make 1 s.[0])) sub
	in
    let res = string_of_word (system.rules symbol) in (* ATTENTION : mise en mémoire des itérations *)
    fun_aux turtle res
;;

let rewrite turtle system degre =
	let rec fun_aux turtle s =
		match s with
		| "" -> turtle
		| s ->
           let sub = String.sub s 1 (String.length s - 1) in
           fun_aux (rewrite_aux turtle system degre (String.make 1 s.[0])) sub
	in
	fun_aux turtle (string_of_word system.axiom)
;;

let start file nb =
	open_window 800 800;
	let system = interpret_file file in
	let turtle = (create_turtle ()) in
	let turle_fin = rewrite turtle system nb in
	close_after_event ()
;;

let action_what () =
  Printf.printf "%s\n" usage;
  exit 0
;;

let action_file () =
  let path = "./examples/br3.sys" in
  let iter = 3 in
  match Array.length Sys.argv with
  | 1
  | 2 -> start path iter
  | 3 -> (try let x = int_of_string Sys.argv.(2) in start path x
  		with Failure _ -> start Sys.argv.(2) iter);
  | _ -> (try let x = int_of_string Sys.argv.(2) in start Sys.argv.(3) x
  		with Failure _ -> start Sys.argv.(2) (int_of_string Sys.argv.(3)));
  exit 0
;;

let cmdline_options = [
("--what" , Arg.Unit action_what, "description");
("--f" , Arg.Unit action_file, "description");
]
;;

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s);;


let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  start "./examples/br3.sys" 7
;;
(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)
let () = if not !Sys.interactive then main ();;
