
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

(** Turtle with current pos and a list of old pos *)
type turtle = {
    current_pos: position;
    saved_pos: position list;
  }

(** This exception is raised if saved_pos list is empty and a Restore command is called *)
exception Restoration_failure of string

(** Create a turtle at specific postion on the graph *)
val create_turtle_at : int -> int -> turtle

(** Create a trutle at origin of the graph *)
val create_turtle : unit -> turtle

(** Update the max and min postion for horizontal and vertical axes *)
val calc_size : turtle -> command -> (float * float * float * float) -> (float * float * float *float * turtle)

(** Calls calc_command for each command of the list *)
val calc_commands : turtle -> command list -> (float * float * float * float) -> (float * float * float * float * turtle)

(** Execute the turtle command as a graphics command add applies the coef in line length i *)
val exec_command : turtle -> command -> float -> turtle

(** Calls exec_command for each command of the list *)
val exec_commands : turtle -> command list -> float -> turtle
