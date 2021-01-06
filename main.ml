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

let try_exec (t: turtle) (l: command list) draw (maxx, maxy): (turtle) =
  try
    exec_commands t l draw (maxx, maxy)
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

let rec rewrite_aux turtle system degre symbol draw (maxx, maxy) =
  if degre = 0 then
    exec_commands turtle (system.interp symbol) draw (maxx, maxy)
  else
	let rec fun_aux turtle s (maxx, maxy) =
	  match s with
	  | "" -> turtle
	  | s ->
         let sub = String.sub s 1 (String.length s - 1) in
         fun_aux (rewrite_aux turtle system (degre-1) (String.make 1 s.[0]) draw (maxx, maxy)) sub (maxx, maxy)
	in
    let res = string_of_word (system.rules symbol) in
    fun_aux turtle res (maxx, maxy)
;;

let rewrite turtle system degre draw (maxx, maxy) =
	let rec fun_aux turtle s (maxx, maxy) =
		match s with
		| "" -> turtle
		| s ->
           let sub = String.sub s 1 (String.length s - 1) in
           fun_aux (rewrite_aux turtle system degre (String.make 1 s.[0]) draw (maxx, maxy)) sub (maxx, maxy)
	in
	fun_aux turtle (string_of_word system.axiom) (maxx, maxy)
;;

let round_millieme f = (floor (f *. 100.)) /. 100. ;;

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  let taillex = 1500 in
  let tailley = 900 in
  let degre = 10 in
  let system = interpret_file "./examples/br3.sys" in
  let turtle = create_turtle () in
  let ((xmax, ymax, xmin, ymin), turtlef) = calc turtle system degre (0., 0., 0., 0.) in
  let xmin = if (round_millieme xmin) <> 0. then (float_of_int taillex) /. xmin else 0. in
  let xmin = if xmin > 0. then xmin else (0. -. xmin) in
  let xmax = if (round_millieme xmax) <> 0. then (float_of_int taillex) /. xmax else 0. in
  let coefx = if xmax = 0. then xmin else if xmin = 0. then xmax else min xmin xmax in
  let ymin = if (round_millieme ymin) <> 0. then (float_of_int tailley) /. ymin else 0. in
  let ymin = if ymin > 0. then ymin else (0. -. ymin) in
  let ymax = if (round_millieme ymax) <> 0. then (float_of_int tailley) /. ymax else 0. in
  let coefy = if ymax = 0. then ymin else if ymin = 0. then ymax else min ymax ymin in
  let turtle = create_turtle () in
  if (round_millieme coefx) = 0. || (round_millieme coefy) < 0. then failwith "impossible de dessiner un si grand dessin sur si peu de pixels!";
  print_float coefx; print_string "\n"; print_float coefy; print_string "\n";
  open_window taillex tailley;
  let turle_fin = rewrite turtle system degre (coefx, coefy) (float_of_int taillex, float_of_int tailley) in
  close_after_event ()
;;
(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)
let () = if not !Sys.interactive then main ();;
