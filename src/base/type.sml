(* type.sml
 *
 * Types
 *)

structure Type : sig

    datatype t
      = T_Base of string		(* base types; e.g., "string" or "int" *)
      | T_Tuple of t list		(* tuple types *)
      | T_Data of TypeReps.dataty	(* datatypes *)
      | T_Fun of t * t			(* function types *)

  end = struct

    datatype t = datatype TypeReps.ty

  end
