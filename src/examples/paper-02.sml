(* paper-02.sml
 *
 * An example from the "Compiling ML Pattern Guards" paper.
 *)

structure Paper02 : sig

    val example : Var.t * (AST.pat * AST.exp) list

  end = struct

    structure Ty = Type
    structure B = Basis

  (* case arg of
   * | (_ if e, false) => 111
   * | (_,      true ) => 222
   *)
    val example = let
          val arg = Var.new("arg", Ty.T_Tuple[B.boolTy, B.boolTy])
	  val pFalse = AST.P_Con(B.falseCon, NONE)
	  val pTrue = AST.P_Con(B.trueCon, NONE)
	  fun act s = AST.E_Exp(s, B.intTy)
          in
            (arg, [
		(AST.P_Tuple[AST.P_If(AST.P_Wild, AST.E_Exp("e", B.boolTy)), pFalse], act "111"),
                (AST.P_Tuple[AST.P_Wild, pTrue], act "222")
              ])
          end

  end
