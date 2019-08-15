(* xform-pats.sml
 *
 * An example of guards used to implement Successor ML transformation
 * patterns.  The example is from
 *
 *	https://people.mpi-sws.org/~rossberg/hamlet/README-succ.txt
 *)

structure XformPats : sig

    val example : Var.t * (AST.pat * AST.exp) list

  end = struct

    structure Ty = Type
    structure B = Basis

  (* supports the "? exp" form *)
    fun cond (ty, mkExp) = let
	  val x = Var.newTmp ty
	  in
	    AST.P_If(AST.P_Var x, mkExp(AST.E_Var x))
	  end

  (* case (n, s) of
   * | (n, ?isempty) = 1
   * | (n, ?(has n) = 2
   * | (n, _) = 3
   *)
    val example = let
	  val setTy = Ty.T_Base "set"
	  val isempty = AST.E_Var(Var.new("isempty", Ty.T_Fun(setTy, B.boolTy)))
	  val has = AST.E_Var(Var.new("has",
		Ty.T_Fun(setTy, Ty.T_Fun(B.intTy, B.boolTy))))
	  val arg = Var.new("arg", Ty.T_Tuple[B.intTy, setTy])
	  fun n () = AST.P_Var(Var.new("n", B.intTy))
	  val n' = Var.new("n", B.intTy)
	  fun act s = AST.E_Exp(s, B.intTy)
	  in
            (arg, [
		(AST.P_Tuple[n(), cond(setTy, fn s => AST.E_App(isempty, [s]))], act "1"),
		(AST.P_Tuple[
		    AST.P_Var n',
		    cond(setTy, fn s => AST.E_App(AST.E_App(has, [s]), [AST.E_Var n']))
		  ], act "2"),
		(AST.P_Tuple[n(), AST.P_Wild], act "3")
              ])
	  end

  end
