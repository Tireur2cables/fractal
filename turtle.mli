
(** Turtle graphical commands *)
type command =
| Line of int      (** advance turtle while drawing *)
| Move of int      (** advance without drawing *)
| Turn of int      (** turn turtle by n degrees *)
| Store            (** save the current position of the turtle *)
| Restore          (** restore the last saved position not yet restored *)

(** Position and angle of the turtle *)
type position = {
    x: float;        (** position x *)
    y: float;        (** position y *)
    a: float;        (** angle of the direction *)
  }

type draw_size = {
	ver : float;
	hor : float;
};;

type turtle = {
    current_pos: position;
    saved_pos: position list;
  }

exception Restoration_failure of string

(** Put here any type and function signatures concerning turtle *)

val pi : float

val create_turtle : unit -> turtle

val calc_size : turtle -> command -> draw_size -> (draw_size * turtle)


val calc_commands : turtle -> command list -> draw_size -> (draw_size * turtle)

val exec_command : turtle -> command -> turtle

val exec_commands : turtle -> command list -> turtle
