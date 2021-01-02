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
    saved_pos: position;
  };;

exception Restoration_failure of string;;

(** Put here any type and function implementations concerning turtle *)

let pi = 4.0 *. atan 1.0;;

let create_turtle () =
  moveto 0 0;
  {current_pos = {
     x = float_of_int (current_x ());
     y = float_of_int (current_y ());
     a = 0.}; (* default angle = 0 *)
   saved_pos = {
       x = -1.;
       y = -1.;
       a = -1.}} (* {-1; -1; -1} = Null position *)
;;

let exec_command (t: turtle) (c: command) : (turtle) =
  match c with
  | Line i -> (* move while drawing by i pixels *)
     lineto (int_of_float ( ((float_of_int i) *. (cos ((t.current_pos.a /. 180.) *. pi))) +. t.current_pos.x) )
       (int_of_float ( ((float_of_int i) *. (sin ((t.current_pos.a /. 180.) *. pi))) +. t.current_pos.y) );
     {current_pos = {
        x = float_of_int (current_x ());
        y = float_of_int (current_y ());
        a = t.current_pos.a};
      saved_pos = t.saved_pos}
  | Move i -> (* move without drawing by i pixels *)
     moveto (int_of_float ( ((float_of_int i) *. (cos ((t.current_pos.a /. 180.) *. pi))) +. t.current_pos.x) )
       (int_of_float ( ((float_of_int i) *. (sin ((t.current_pos.a /. 180.) *. pi))) +. t.current_pos.y) );
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
      saved_pos = t.current_pos}
  | Restore -> (* put saved_pos in current_pos if possible *)
     if t.saved_pos <> {x = -1.; y = -1.; a = -1.} then
       begin
         moveto (int_of_float t.saved_pos.x) (int_of_float t.saved_pos.y);
         {current_pos = t.saved_pos;
          saved_pos = {x = -1.; y = -1.; a = -1.}} (* {-1; -1; -1} = Null position *)
       end
     else
       raise (Restoration_failure "Erreur de Restoration -> Aucune position sauvegardée!\n")
;;


let rec exec_commands (t: turtle) (l: command list) : turtle =
  match l with
  | [] -> t
  | x :: l -> exec_commands (exec_command t x) l
;;
