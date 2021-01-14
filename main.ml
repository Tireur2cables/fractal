open Lsystems;;
open Systems;;
open Turtle;;
open Graphics;;

(** open a graphic window of size w * h *)
let open_window w h =
  open_graph (" " ^ (string_of_int w) ^ "x" ^ (string_of_int h));
  fill_rect 0 0 w h;
  auto_synchronize true
;;

(** wait a click before closing the graph *)
let close_after_event () : unit =
  ignore (wait_next_event [Button_down ; Key_pressed])
;;

(** try to execute the list of commands and eventually catch a Restoration exception *)
let try_exec (t: turtle) (l: command list) (coef: float) : (turtle) =
  try
    exec_commands t l coef
  with
  | Restoration_failure s ->
     print_string s;
     create_turtle ()
;;

(** transform the symbol a correct number of times and calls the calc_commands for each symbol when finish *)
let rec trans_calc (turtle: turtle) (syst: 's system) (degre: int) (symb: string)
          ((hp, vp, hn, vn): float * float * float *float) :
          (float * float * float * float * turtle) =
  if degre = 0 then
    calc_commands turtle (syst.interp symb) (hp, vp, hn, vn)
  else
	let rec trans_calc_aux turtle (hp, vp, hn, vn) s =
	  match s with
	  | "" -> (hp, vp, hn, vn, turtle)
	  | s ->
         let sub = String.sub s 1 (String.length s - 1) in
         let c = String.make 1 s.[0] in
         let d = degre - 1 in
		 let (hp, vp, hn, vn, t) = trans_calc turtle syst d c (hp, vp, hn, vn) in
         trans_calc_aux t (hp, vp, hn, vn) sub
	in
    let res = string_of_word (syst.rules symb) in
    trans_calc_aux turtle (hp, vp, hn, vn) res
;;

(** calls trans_calc on each symbol of the axiom *)
let calc (turtle: turtle) (syst: 's system) (deg: int)
      (dim: float * float * float * float) :
      ((float * float * float * float) * turtle) =
  let rec calc_aux turtle dim s =
	match s with
	| "" -> (dim, turtle)
	| s ->
       let sub = String.sub s 1 (String.length s - 1) in
       let c = String.make 1 s.[0] in
	   let (xmax, ymax, xmin, ymin, tort) = trans_calc turtle syst deg c dim in
       calc_aux tort (xmax, ymax, xmin, ymin) sub
  in
  calc_aux turtle dim (string_of_word syst.axiom)
;;

(** transform the symbol a correct number of times and call the try_exec for each symbol when finish *)
let rec transform_symb (turtle: turtle) (system: 's system) (degre: int)
          (symbol: string) (coef: float) : turtle =
  if degre = 0 then
    try_exec turtle (system.interp symbol) coef
  else
	let rec transform_symb_aux turtle s =
	  match s with
	  | "" -> turtle
	  | s ->
         let sub = String.sub s 1 (String.length s - 1) in
         let c = String.make 1 s.[0] in
         transform_symb_aux (transform_symb turtle system (degre-1) c coef) sub
	in
    let res = string_of_word (system.rules symbol) in
    transform_symb_aux turtle res
;;

(** calls transform_symb on each symbole of the axiom *)
let transform_system (turtle: turtle) (system: 's system) (degre: int)
      (coef: float) : turtle =
  let rec transform_system_aux turtle s =
	match s with
	| "" -> turtle
	| s ->
       let sub = String.sub s 1 (String.length s - 1) in
       let c = String.make 1 s.[0] in
       transform_system_aux (transform_symb turtle system degre c coef) sub
  in
  transform_system_aux turtle (string_of_word system.axiom)
;;

(** Calcul for coef and posx posy *)
let calc_pos ((xmax, ymax, xmin, ymin) : (float * float * float * float))
      ((taillex, tailley) : (float * float)) : (float * int * int)  =
  let sizex = xmax -. xmin in
  let sizey = ymax -. ymin in
  let coefx = if sizex < taillex
              then taillex /. (sizex *. 1.5)
              else taillex /. sizex in
  let coefy = if sizey < tailley
              then tailley /. (sizey *. 1.5)
              else tailley /. sizey in
  let coef = min coefx coefy in
  let posx = int_of_float (
                 (((taillex -. (coef *. (xmax -. xmin))) /. 2.) -. (coef *. xmin))
               ) in
  let posy = int_of_float (
                 (((tailley -. (coef *. (ymax -. ymin))) /. 2.) -. (coef *. ymin))
               ) in
  (coef, posx, posy)
;;

(** default L-systeme file *)
let path = "./examples/br1.sys";;

(** default number of iterations *)
let iter = 3;;

(** start the calcul for the size of the graph and then draw the graph *)
let start (file: string) (nb: int) : unit =
  let taillex = 1000. in
  let tailley = 1000. in
  let system = interpret_file file in
  let coords = (0., 0., 0., 0.) in
  let turtle = create_turtle () in
  let ((xmax, ymax, xmin, ymin), _) = calc turtle system nb coords in

  let (coef, posx, posy) = calc_pos (xmax, ymax, xmin, ymin) (taillex, tailley) in

  try
    open_window (int_of_float taillex) (int_of_float tailley);
    let turtle = create_turtle_at posx posy in
    let turle_fin = transform_system turtle system nb coef in
    close_after_event ()
  with
    Graphic_failure s -> exit 0
;;

(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)
let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"
;;

(** launch error if unknown argument in command line *)
let extra_arg_action = fun s -> failwith ("Argument inconnu :" ^ s);;

(** print the string usage if -what is in the command line *)
let action_what () : unit =
  print_string (usage ^ "\n");
  exit 0
;;

(** launch start function with correct arguments if -c option is in command line *)
let action_file () : unit =
  begin
    match Array.length Sys.argv with
    | 1 | 2 -> start path iter
    | 3 ->
       begin
         try
           let x = int_of_string Sys.argv.(2) in
           start path x
         with
           Failure _ -> start Sys.argv.(2) iter
       end
    | _ ->
       begin
         try
           let x = int_of_string Sys.argv.(2) in
           start Sys.argv.(3) x
	     with Failure _ ->
           try
             start Sys.argv.(2) (int_of_string Sys.argv.(3))
           with
             Failure _ -> failwith "Il faut donner un nombre sur un des deux paramètres!"
       end
  end;
  exit 0
;;

(** list of all caugth command line option *)
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

(** parse the command line and launch start *)
let main () : unit =
  Arg.parse cmdline_options extra_arg_action usage;
  start path iter
;;

(** launch main if we are not in interactive mod *)
let () : unit = if not !Sys.interactive then main ();;
