(* type-reps.sml
 *
 * Concrete representation of types.
 *)

structure TypeReps =
  struct

    datatype ty
      = T_Base of string	(* base types; e.g., "string" or "int" *)
      | T_Tuple of ty list	(* tuple types *)
      | T_Data of dataty	(* datatypes *)
      | T_Fun of ty * ty	(* function types *)

    and dataty = Data of {
        name : string,
	mutable : bool,		(* true for reference types *)
        span : int,             (* number of constructors; 0 for exn *)
        cons : dcon list ref    (* list of constructors *)
      }

    and dcon = DCon of Atom.atom * dataty * ty option

  end
