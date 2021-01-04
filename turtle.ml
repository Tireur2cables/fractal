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

  (** Size of the drawing*)
type draw_size = {
  nord : float;        (** position nord *)
  east : float;        (** position east *)
  south: float;       (** position south *)
  west: float;        (** position west *)
};;

type turtle = {
    current_pos: position;
    saved_pos: position list;
  };;

exception Restoration_failure of string;;

(** Put here any type and function implementations concerning turtle *)

let pi = 4.0 *. atan 1.0;;

let create_turtle () =
  moveto 400 400; (* move to middle bottom *)
  {current_pos = {
     x = float_of_int (current_x ());
     y = float_of_int (current_y ());
     a = 90.}; (* default angle = 90 = toward top of screen *)
  saved_pos = []} (* No saved postion *)
;;

let calc_size (t:turtle) (c:command) (d:draw_size): (draw_size * turtle) =
	match c with
	| Line i -> (* Tester : si l'angle de la tortue dirige vers quel quart du plan ET si on "revient sur nos pas (on est avant le milieu de l'écran) ou si on avance vers la direction"*)
	   if t.current_pos.a <= 90.0 && t.current_pos.a >= 0. || t.current_pos.a < -270.
	   then ({
			nord = d.nord;
			east = d.east +. float_of_int i;
			south = d.south;
			west = d.west;
	   },{current_pos = {
		  x = float_of_int (current_x ());
		  y = float_of_int (current_y ());
		  a = t.current_pos.a};
		saved_pos = t.saved_pos})
	   else if t.current_pos.a <= 180. && t.current_pos.a > 90. || t.current_pos.a < -180. && t.current_pos.a > 270.
	   then ({
			nord = d.nord +. float_of_int i;
			east = d.east;
			south = d.south;
			west = d.west;
	   },{current_pos = {
		  x = float_of_int (current_x ());
		  y = float_of_int (current_y ());
		  a = t.current_pos.a};
		saved_pos = t.saved_pos})
		else if t.current_pos.a <= 270. && t.current_pos.a > 180. || t.current_pos.a < -90. && t.current_pos.a > -180.
 	   then ({
 			nord = d.nord;
 			east = d.east;
 			south = d.south;
 			west = d.west  +. float_of_int i;
 	   },{current_pos = {
 		  x = float_of_int (current_x ());
 		  y = float_of_int (current_y ());
 		  a = t.current_pos.a};
 		saved_pos = t.saved_pos})
 		else ( {
  			nord = d.nord;
  			east = d.east;
  			south = d.south  +. float_of_int i;
  			west = d.west;
  	   },{current_pos = {
  		  x = float_of_int (current_x ());
  		  y = float_of_int (current_y ());
  		  a = t.current_pos.a};
		  saved_pos = t.saved_pos})
	| Move i -> (* move without drawing by i pixels *)
	   moveto (int_of_float ( ((float_of_int i) *. (cos ((t.current_pos.a /. 180.) *. pi))) +. t.current_pos.x) )
		 (int_of_float ( ((float_of_int i) *. (sin ((t.current_pos.a /. 180.) *. pi))) +. t.current_pos.y) );
		 ({
  		   nord = d.nord;
  		   east = d.east;
  		   south = d.south;
  		   west = d.west;
  	  },{current_pos = {
  		 x = float_of_int (current_x ());
  		 y = float_of_int (current_y ());
  		 a = t.current_pos.a};
  	   saved_pos = t.saved_pos})

	| Turn i -> (* turn by i degrees *)
	({
		nord = d.nord;
		east = d.east;
		south = d.south;
		west = d.west;
   },{current_pos = {
	  x = float_of_int (current_x ());
	  y = float_of_int (current_y ());
	  a = mod_float ( t.current_pos.a +. (float_of_int i) ) 360.};
	saved_pos = t.saved_pos})

	| Store -> (* save current_pos in saved_pos *)
	({
		nord = d.nord;
		east = d.east;
		south = d.south;
		west = d.west;
   },{current_pos = {
	  x = float_of_int (current_x ());
	  y = float_of_int (current_y ());
	  a = t.current_pos.a};
	saved_pos = t.saved_pos})

	| Restore -> (* put saved_pos in current_pos if possible *)
	   match t.saved_pos with
	   | [] -> raise (Restoration_failure "Erreur de Restoration -> Aucune position sauvegardée!\n")
	   | s :: l ->
		  begin
			moveto (int_of_float s.x) (int_of_float s.y);
			({
	 			nord = d.nord;
	 			east = d.east;
	 			south = d.south;
	 			west = d.west;
	 	   },{current_pos = {
	 		  x = float_of_int (current_x ());
	 		  y = float_of_int (current_y ());
	 		  a = t.current_pos.a};
	 		saved_pos = t.saved_pos})
		  end

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

let rec calc_commands (t:turtle) (l:command list) (d:draw_size): (draw_size * turtle) =
  match l with
  | [] -> (d,t)
  | x :: l -> (
	  let (dim, tort) = calc_size t x d in
	  calc_commands tort l dim
	  )
;;

let rec exec_commands (t: turtle) (l: command list) : turtle =
  match l with
  | [] -> t
  | x :: l -> exec_commands (exec_command t x) l
;;
