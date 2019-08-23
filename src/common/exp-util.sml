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

    fun toString e = let
	  fun concatWith sep f ([], l) = l
	    | concatWith sep f ([x], l) = f(x, l)
	    | concatWith sep f (x::xs, l) = concatWith sep f (xs, f(x, sep :: l))
	  fun toS (0, e, l) = "<exp>" :: l
	    | toS (d, e, l) = let
		fun toS' (e, l) = toS(d-1, e, l)
		in
		  case e
		   of E_Let(x, e1, e2) => "let " :: Var.name x :: " = "
			:: toS'(e1, " in " :: toS'(e2, " end" :: l))
		    | E_Fun((f, params, e1), e2) => "<fun>" :: l
		    | E_App(f, args) => toS'(f, "(" :: concatWith "," toS' (args, ")" :: l))
		    | E_If(e1, e2, e3) => "<if>" :: l
		    | E_Case(_, (_, e)::_) => "<case>" :: l
		    | E_Case _ => raise Fail "bad case"
		    | E_Tuple es => "(" :: concatWith "," toS' (es, ")" :: l)
		    | E_Select(i, e) => "#" :: Int.toString i :: " " :: toS'(e, l)
		    | E_Var x => Var.name x :: l
		    | E_Con dc => DataCon.toString dc :: l
		    | E_Raise(e, _) => "raise " :: toS'(e, l)
		    | E_Exp(e, _) => e :: l
		  (* end case *)
		end
	  in
	    String.concat(toS (3, e, []))
	  end

    val kUnit = E_Tuple[]
    val kTrue = E_Con Basis.trueCon
    val kFalse = E_Con Basis.falseCon

    fun isTrue (E_Con dc) = DataCon.same(dc, Basis.trueCon)
      | isTrue _ = false

    fun call (f, args) = E_App(E_Var f, args)

  end
