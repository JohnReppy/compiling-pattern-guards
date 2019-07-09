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

    val toString : t -> string

    val same : t * t -> bool

  end = struct

    datatype t = datatype TypeReps.ty

    fun toString (T_Base b) = b
      | toString (T_Tuple[]) = "unit"
      | toString (T_Tuple tys) = let
          fun toS (ty as T_Fun _) = concat["(", toString ty, ")"]
            | toS ty = toString ty
          in
            String.concatWithMap " * " toS tys
          end
      | toString (T_Data dt) = DataTy.toString dt
      | toString (T_Fun(ty1 as T_Fun _, ty2)) = concat[
            "(", toString ty1, ") -> ", toString ty2
          ]
      | toString (T_Fun(ty1, ty2)) = concat[toString ty1, " -> ", toString ty2]

    fun same (T_Base b1, T_Base b2) = (b1 = b2)
      | same (T_Tuple tys1, T_Tuple tys2) = ListPair.allEq same (tys1, tys2)
      | same (T_Data dt1, T_Data dt2) = DataTy.same(dt1, dt2)
      | same (T_Fun(ty11, ty12), T_Fun(ty21, ty22)) =
          same(ty11, ty21) andalso same(ty21, ty22)
      | same _ = false

  end
