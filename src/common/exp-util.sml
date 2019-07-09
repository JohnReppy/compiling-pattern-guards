(* exp-util.sml
 *
 * Utility functions om expressions
 *)

structure ExpUtil : sig

    val typeOf : AST.exp -> Type.t

  (* abbreviated string representation of expressions *)
    val toString : AST.exp -> string

  (* is the expression "true" *)
    val isTrue : AST.exp -> bool

    val kUnit : AST.exp
    val kTrue : AST.exp
    val kFalse : AST.exp

    val call : Var.t * AST.exp list -> AST.exp

  end = struct

    datatype exp = datatype AST.exp

    fun typeOf e = (case e
	   of E_Let(_, _, e) => typeOf e
	    | E_Fun(_, e) => typeOf e
	    | E_App(e, _) => (case typeOf e
		 of Type.T_Fun(_, ty) => ty
		  | _ => raise Fail "expected function type"
		(* end case *))
	    | E_If(_, e, _) => typeOf e
	    | E_Case(_, (_, e)::_) => typeOf e
	    | E_Case _ => raise Fail "bad case"
	    | E_Tuple es => Type.T_Tuple(List.map typeOf es)
	    | E_Select(i, e) => (case typeOf e
		 of Type.T_Tuple tys => List.nth(tys, i-1)
		  | _ => raise Fail "expected tuple type"
		(* end case *))
	    | E_Var x => Var.typeOf x
	    | E_Con dc => Type.T_Data(DataCon.owner dc)
	    | E_Raise(_, ty) => ty
	    | E_Exp(_, ty) => ty
	  (* end case *))

    fun toString exp = "FIXME"

    val kUnit = E_Tuple[]
    val kTrue = E_Con Basis.trueCon
    val kFalse = E_Con Basis.falseCon

    fun isTrue (E_Con dc) = DataCon.same(dc, Basis.trueCon)
      | isTrue _ = false

    fun call (f, args) = let
	  val args = (case args
		 of [e] => e
		  | es => E_Tuple es
		(* end case *))
	  in
	    E_App(E_Var f, args)
	  end

  end
