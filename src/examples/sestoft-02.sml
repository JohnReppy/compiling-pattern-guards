(* sestoft-02.sml
 *
 * An example from "ML Pattern Match Compilation and Partial Evaluation" (page 459)
 * by Peter Sestoft.
 *)

structure Sestoft02 : sig

    val example : Var.t * (AST.pat * AST.exp) list

  end = struct

    structure Ty = Type
    structure B = Basis

  (* datatype t = A | B *)
    val (tDT, [cA, cB]) = DataTy.newWithCons("t", [("A", NONE), ("B", NONE)])
    val tTy = Ty.T_Data tDT

  (* case arg of
   * | (A, A, _, _, _, _, _, _, _, _) => 0
   * | (_, _, A, A, _, _, _, _, _, _) => 1
   * | (_, _, _, _, A, A, _, _, _, _) => 2
   * | (_, _, _, _, _, _, A, A, _, _) => 3
   * | (_, _, _, _, _, _, _, _, A, A) => 4
   * | (A, B, A, B, A, B, A, B, A, B) => 5
   *)
    val example = let
          val arg = Var.new("arg", Ty.T_Tuple(List.tabulate(10, fn _ => tTy)))
	  val w = AST.P_Wild
	  val pA = AST.P_Con(cA, NONE)
	  val pB = AST.P_Con(cB, NONE)
	  fun act s = AST.E_Exp(s, B.intTy)
	  in
	    (arg, [
		(AST.P_Tuple[pA, pA,  w,  w,  w,  w,  w,  w,  w,  w], act "0"),
		(AST.P_Tuple[ w,  w, pA, pA,  w,  w,  w,  w,  w,  w], act "1"),
		(AST.P_Tuple[ w,  w,  w,  w, pA, pA,  w,  w,  w,  w], act "2"),
		(AST.P_Tuple[ w,  w,  w,  w,  w,  w, pA, pA,  w,  w], act "3"),
		(AST.P_Tuple[ w,  w,  w,  w,  w,  w,  w,  w, pA, pA], act "4"),
		(AST.P_Tuple[pA, pB, pA, pB, pA, pB, pA, pB, pA, pB], act "5")
	      ])
	  end

  end
