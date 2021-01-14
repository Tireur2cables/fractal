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

(** Turtle with current pos and a list of old pos *)
type turtle = {
    current_pos: position;
    saved_pos: position list;
  };;

(** This exception is raised if saved_pos list is empty and a Restore command is called *)
exception Restoration_failure of string;;

(** Good approximation of pi *)
let pi = 4.0 *. atan 1.0;;

(** Implementation of round function for Float *)
let round (f: float) : float =
  let dessus = ceil f in
  if compare (dessus-.f) 0.5 <= 0 then dessus
  else floor f
;;

(** Create a turtle at specific postion on the graph *)
let create_turtle_at (x: int) (y: int) : turtle =
  moveto x y;
  {current_pos = {
     x = float_of_int (current_x ());
     y = float_of_int (current_y ());
     a = 90.}; (* default angle = 90° => toward top of the screen *)
  saved_pos = []} (* No saved postion *)
;;

(** Create a turtle at origin of the graph *)
let create_turtle () : turtle =
  create_turtle_at 0 0
;;

(** Update the max and min postion for horizontal and vertical axes *)
let calc_size (t: turtle) (c: command) ((hp, vp, hn, vn): (float * float * float * float))
    : (float * float * float * float * turtle) =
  match c with

    | Move i | Line i ->
     let newx = (float_of_int i) *. (cos ((t.current_pos.a /. 180.) *. pi)) in
	 let newx = round newx in
     let coefx = newx +. t.current_pos.x in
     let newx = int_of_float coefx in
     let newy = (float_of_int i) *. (sin ((t.current_pos.a /. 180.) *. pi)) in
	 let newy = round newy in
     let coefy = newy +. t.current_pos.y in
     let newy = int_of_float coefy in
     let hp = max hp coefx in
     let vp = max vp coefy in
     let hn = min hn coefx in
     let vn = min vn coefy in
     (hp, vp, hn, vn, {
          current_pos = {
  		    x = float_of_int newx;
  		    y = float_of_int newy;
  		    a = t.current_pos.a};
          saved_pos = t.saved_pos})

  | Turn i -> (* turn by i degrees *)
	 (hp, vp, hn, vn, {
          current_pos = {
	        x = t.current_pos.x;
	        y = t.current_pos.y;
	        a = mod_float ( t.current_pos.a +. (float_of_int i) ) 360.};
	      saved_pos = t.saved_pos})

  | Store -> (* save current_pos in saved_pos *)
	 (hp, vp, hn, vn, {
          current_pos = {
	        x = t.current_pos.x;
	        y = t.current_pos.y;
	        a = t.current_pos.a};
          saved_pos = t.current_pos :: t.saved_pos})

  | Restore -> (* put saved_pos in current_pos if possible *)
	 match t.saved_pos with
	 | [] ->
        let mes = "Erreur de Restoration -> Aucune position sauvegardée!\n" in
        raise (Restoration_failure mes)
	 | s :: l ->
		begin
		  (hp, vp, hn, vn, {
               current_pos = s;
		       saved_pos = l})
		end
;;

(** Calls calc_command for each command of the list *)
let rec calc_commands (t: turtle) (l: command list) ((hp, vp, hn, vn): float * float * float * float)
        : (float * float * float * float * turtle) =
  match l with
  | [] -> (hp, vp, hn, vn, t)
  | x :: l ->
     begin
	   let (hp, vp, hn, vn, tort) = calc_size t x (hp, vp, hn, vn) in
	   calc_commands tort l (hp, vp, hn, vn)
	 end
;;

(** Execute the turtle command as a graphics command add applies the coef in line length i *)
let exec_command (t: turtle) (c: command) ((coefx, coefy): float * float) : (turtle) =
  print_float coefx;
  print_string "\n";
  set_color ((current_x()+current_y()) * (0xFFFFFF / (1000+1000)));
  match c with

  | Line i -> (* move while drawing by i * coef pixels *)
     let i = float_of_int (if i mod 2 = 0 then i else i + 1) in
     let newx = coefx *. i *. (cos ((t.current_pos.a /. 180.) *. pi)) in
	 let newx = round newx in
     let newx = int_of_float (newx +. t.current_pos.x) in
     let newy = coefy *. i *. (sin ((t.current_pos.a /. 180.) *. pi)) in
	 let newy = round newy in
     let newy = int_of_float (newy +. t.current_pos.y) in
     let time = if coefx > 0.8
	            then coefx *. 0.0005 *. i
				else if coefx > 0.25
				then coefx *. 0.0001 *. i
				else coefx *. 0.0000001 *. i in
     Unix.sleepf(time);
     lineto (newx) (newy);
     {current_pos = {
        x = float_of_int (current_x ());
        y = float_of_int (current_y ());
        a = t.current_pos.a};
      saved_pos = t.saved_pos}

  | Move i -> (* move without drawing by i * coef pixels *)
     let i = float_of_int (if i mod 2 = 0 then i else i + 1) in
     let newx = coefx *. i *. (cos ((t.current_pos.a /. 180.) *. pi)) in
     let newx = int_of_float (newx +. t.current_pos.x) in
     let newy = coefy *. i *. (sin ((t.current_pos.a /. 180.) *. pi)) in
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
     | [] ->
        let mes = "Erreur de Restoration -> Aucune position sauvegardée!\n" in
        raise (Restoration_failure mes)
     | s :: l ->
        begin
          moveto (int_of_float s.x) (int_of_float s.y);
          {current_pos = s;
           saved_pos = l} (* list without s *)
        end
;;

(** Calls exec_command for each command of the list *)
let rec exec_commands (t: turtle) (l: command list) ((coefx, coefy) : (float * float)) : turtle =
  match l with
  | [] -> t
  | x :: l ->
     let turtle = exec_command t x (coefx, coefy) in
     exec_commands turtle l (coefx, coefy)
;;
