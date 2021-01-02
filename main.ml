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

let try_exec (t: turtle) (l: command list) : (turtle) =
  try
    exec_commands t l
  with
  | Restoration_failure s ->
     print_string s;
     create_turtle ()
;;

let interp_syst system degre =
  if degre = 0 then
    system.interp (string_of_word system.axiom)
  else failwith "wip";
;;

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  open_window 800 800;
  let system = interpret_file "./examples/test.sys" in
  let list = interp_syst system 0 in
  let turtle = try_exec (create_turtle ()) list in
  close_after_event ()
;;
(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)
let () = if not !Sys.interactive then main ();;
