(* ast.sml
 *
 * Abstract syntax for a simple ML-like language
 *)

structure AST =
  struct

    datatype exp
      = E_Let of Var.t * exp * exp
      | E_Fun of function * exp
      | E_App of exp * exp list
      | E_If of exp * exp * exp
      | E_Case of exp * (pat * exp) list
      | E_Tuple of exp list
      | E_Select of int * exp
      | E_Var of Var.t
      | E_Con of DataCon.t
      | E_Raise of exp * Type.t
      | E_Exp of string * Type.t		(* to represent other code *)

    and pat
      = P_Wild					(* '_' *)
      | P_Var of Var.t                          (* var *)
      | P_Tuple of pat list			(* '(' pat ',' ... ',' pat ')' *)
      | P_Con of DataCon.t * pat option		(* con pat *)
      | P_Or of pat list			(* pat '|' ... '|' pat *)
      | P_If of pat * exp			(* pat 'if' exp *)

    withtype function = Var.t * Var.t list * exp

  end
