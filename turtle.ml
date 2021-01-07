open Graphics;;

(** Turtle graphical commands *)
type command =
| Line of int      (** advance turtle while drawing *)
| Move of int      (** advance without drawing *)
| Turn of int      (** turn turtle by n degrees *)
| Store            (** save the current position of the turtle *)
| Restore          (** restore the last saved position not yet restored *)
;;
(** Position and angle of the turtle *)
type position = {
    x: float;        (** position x *)
    y: float;        (** position y *)
    a: float;        (** angle of the direction *)
  };;

type turtle = {
    current_pos: position;
    saved_pos: position list;
  };;

exception Restoration_failure of string;;

(** Put here any type and function implementations concerning turtle *)

let pi = 4.0 *. atan 1.0;;

let round f =
  let dessus = ceil f in
  if compare (dessus-.f) 0.5 <= 0 then dessus
  else floor f
;;

let create_turtle () =
  moveto 400 400; (* move to middle bottom *)
  {current_pos = {
     x = float_of_int (current_x ());
     y = float_of_int (current_y ());
     a = 90.}; (* default angle = 90 = toward top of screen *)
  saved_pos = []} (* No saved postion *)
;;

let exec_command (t: turtle) (c: command) : (turtle) =
  match c with

  | Line i -> (* move while drawing by i pixels *)
     let newx = (float_of_int i) *. (cos ((t.current_pos.a /. 180.) *. pi)) in
	 let newx = round newx in
     let newx = int_of_float (newx +. t.current_pos.x) in
     let newy = (float_of_int i) *. (sin ((t.current_pos.a /. 180.) *. pi)) in
	 let newy = round newy in
     let newy = int_of_float (newy +. t.current_pos.y) in
     lineto (newx) (newy);
     {current_pos = {
        x = float_of_int (current_x ());
        y = float_of_int (current_y ());
        a = t.current_pos.a};
      saved_pos = t.saved_pos}

  | Move i -> (* move without drawing by i pixels *)
     let newx = (float_of_int i) *. (cos ((t.current_pos.a /. 180.) *. pi)) in
     let newx = int_of_float (newx +. t.current_pos.x) in
     let newy = (float_of_int i) *. (sin ((t.current_pos.a /. 180.) *. pi)) in
     let newy = int_of_float (newy +. t.current_pos.y) in
     moveto (newx) (newy);
     {current_pos = {
        x = float_of_int (current_x ());
        y = float_of_int (current_y ());
        a = t.current_pos.a};
      saved_pos = t.saved_pos}

  | Turn i -> (* turn by i degrees *)
     {current_pos = {
        x = t.current_pos.x;
        y = t.current_pos.y;
        a = mod_float ( t.current_pos.a +. (float_of_int i) ) 360.};
      saved_pos = t.saved_pos}

  | Store -> (* save current_pos in saved_pos *)
     {current_pos = t.current_pos;
      saved_pos = t.current_pos :: t.saved_pos} (* store at start *)

  | Restore -> (* put saved_pos in current_pos if possible *)
     match t.saved_pos with
     | [] -> raise (Restoration_failure "Erreur de Restoration -> Aucune position sauvegardée!\n")
     | s :: l ->
        begin
          moveto (int_of_float s.x) (int_of_float s.y);
          {current_pos = s;
           saved_pos = l} (* list without s *)
        end
;;


let rec exec_commands (t: turtle) (l: command list) : turtle =
  match l with
  | [] -> t
  | x :: l -> exec_commands (exec_command t x) l
;;
