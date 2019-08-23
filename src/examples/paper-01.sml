(* paper-01.sml
 *
 * An example from the "Compiling ML Pattern Guards" paper.  The example is slightly
 * modified to use a datatype instead of integer patterns.
 *)

structure Paper01 : sig

    val example : Var.t * (AST.pat * AST.exp) list

  end = struct

    structure Ty = Type
    structure B = Basis

  (* datatype t = Zero | One | Two *)
    val (tDT, [cZero, cOne, cTwo]) = DataTy.newWithCons ("t", [
	    ("Zero", NONE),
	    ("One", NONE),
	    ("Two", NONE)
	  ])

    val {ty=refTy, refCon} = B.newRefTy("tref", Ty.T_Data tDT)

  (* case x of
   * | (ref One) => 111
   * | (ref _ if (x := One; false)) => 222
   * | (ref One) => 333
   *)
    val example = let
          val x = Var.new("x", refTy)
	  val pOne = AST.P_Con(cOne, NONE)
          fun pRef arg = AST.P_Con(refCon, SOME arg)
	  fun act s = AST.E_Exp(s, B.intTy)
          in
            (x, [
                (pRef pOne, act "111"),
                (AST.P_If(pRef AST.P_Wild, AST.E_Exp("(x := One; false)", B.boolTy)), act "222"),
                (pRef pOne, act "333")
              ])
          end

  end
