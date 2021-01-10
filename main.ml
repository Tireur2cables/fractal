open Lsystems;;
open Systems;;
open Turtle;;
open Graphics;;

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

let try_exec (t: turtle) (l: command list) (coefx, coefy) : (turtle) =
  try
    exec_commands t l (coefx, coefy)
  with
  | Restoration_failure s ->
     print_string s;
     create_turtle ()
;;

let rec calc_aux turtle system degre symbol (hp, vp, hn, vn) =
  if degre = 0 then
    calc_commands turtle (system.interp symbol) (hp, vp, hn, vn)
  else
	let rec fun_aux turtle (hp, vp, hn, vn) s =
	  match s with
	  | "" -> (hp, vp, hn, vn, turtle)
	  | s ->
         let sub = String.sub s 1 (String.length s - 1) in
		 let (xmax, ymax, xmin, ymin, tort) = calc_aux turtle system (degre-1) (String.make 1 s.[0]) (hp, vp, hn, vn) in
         fun_aux tort (xmax, ymax, xmin, ymin) sub
	in
    let res = string_of_word (system.rules symbol) in
    fun_aux turtle (hp, vp, hn, vn) res
;;

let calc turtle system degre curr_dim =
	let rec fun_aux turtle curr_dim s =
		match s with
		| "" -> (curr_dim, turtle)
		| s ->
           let sub = String.sub s 1 (String.length s - 1) in
		   let (xmax, ymax, xmin, ymin, tort) = calc_aux turtle system degre (String.make 1 s.[0]) curr_dim in
           fun_aux tort (xmax, ymax, xmin, ymin) sub
	in
	fun_aux turtle curr_dim (string_of_word system.axiom)
;;

let rec rewrite_aux turtle system degre symbol draw =
  if degre = 0 then
    try_exec turtle (system.interp symbol) draw
  else
	let rec fun_aux turtle s =
	  match s with
	  | "" -> turtle
	  | s ->
         let sub = String.sub s 1 (String.length s - 1) in
         fun_aux (rewrite_aux turtle system (degre-1) (String.make 1 s.[0]) draw) sub
	in
    let res = string_of_word (system.rules symbol) in
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

let path = "./examples/br1.sys";;

let iter = 5;;

let start file nb =
  let taillex = 1000. in
  let tailley = 1000. in
  let system = interpret_file file in
  let ((xmax, ymax, xmin, ymin), turtlef) = calc (create_turtle ()) system nb (0., 0., 0., 0.) in
  let coefx = xmax -. xmin in
  let coefy = ymax -. ymin in
  let coefx = if coefx <= taillex then 1. else taillex /. coefx in
  let coefy = if coefy <= tailley then 1. else tailley /. coefy in
  let posxmax = taillex -. (xmax *. coefx) in
  let posymax = tailley -. (ymax *. coefy) in
  let posxmin = 0. -. (xmin *. coefx) in
  let posymin = 0. -. (ymin *. coefy) in
  let middlex = taillex /. 2. in
  let middley = tailley /. 2. in
  let posx = int_of_float (
                 if middlex +. xmax < taillex && middlex -. xmin > 0. then middlex
                 else if abs (int_of_float (posxmax -. middlex)) < abs (int_of_float (posxmin -. middlex))
                 then posxmax else posxmin
               ) in
  let posy = int_of_float (
                 if middley +. ymax < tailley && middley -. ymin > 0. then middley
                 else if abs (int_of_float (posymax -. middley)) < abs (int_of_float (posymin -. middley))
                 then posymax else posymin
               ) in
  open_window (int_of_float taillex) (int_of_float tailley);
  let turle_fin = rewrite (create_turtle_at posx posy) system nb (min coefx coefy, min coefx coefy) in
  close_after_event ()
;;

(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)
let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"
;;

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s);;

let action_what () =
 print_string (usage ^ "\n");
 exit 0
;;

let action_file () =
  begin
    match Array.length Sys.argv with
    | 1 | 2 -> start path iter
    | 3 ->
       begin
         try
           let x = int_of_string Sys.argv.(2) in
           start path x
         with Failure _ -> start Sys.argv.(2) iter
       end
    | _ ->
       begin
         try
           let x = int_of_string Sys.argv.(2) in
           start Sys.argv.(3) x
	     with Failure _ ->
               try
                 start Sys.argv.(2) (int_of_string Sys.argv.(3))
               with Failure _ -> failwith "Il faut donner un nombre sur un des deux paramètres!"

       end
  end;
    exit 0
;;

let cmdline_options =
  [
    ("--what" , Arg.Unit action_what, "description");
    ("-what" , Arg.Unit action_what, "description");
    ("--custom" , Arg.Unit action_file, "specify custom path and/or custom number of iteration");
    ("-custom" , Arg.Unit action_file, "specify custom path and/or custom number of iteration");
    ("-c" , Arg.Unit action_file, "specify custom path and/or custom number of iteration");
    ("--c" , Arg.Unit action_file, "specify custom path and/or custom number of iteration");
  ]
;;

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  start path iter
;;
(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)
let () = if not !Sys.interactive then main ();;
