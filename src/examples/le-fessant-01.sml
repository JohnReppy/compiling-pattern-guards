(* le-fessant-01.sml
 *
 * Example from "Optimizing Pattern Matching" (p 29), by Fabrice Le Fessant and
 * Luc Maranget.
 *)

structure LeFessant01 : sig

    val example : Var.t * (AST.pat * AST.exp) list

  end = struct

    structure Ty = Type
    structure B = Basis

  (* integer lists *)
    val {ty, nilCon, consCon} = B.newListTy("intlist", B.intTy)

  (* case arg of
   * | (nil,  _) => 1
   * | (_,    nil) => 2
   * | (_::_, _::_) => 3
   *)
    val example = let
	  val arg = Var.new ("arg", Ty.T_Tuple[ty, ty])
	  val Nil = AST.P_Con(nilCon, NONE)
	  fun Cons (hd, tl) = AST.P_Con(consCon, SOME(AST.P_Tuple[hd, tl]))
	  val w = AST.P_Wild
	  fun act s = AST.E_Exp(s, B.intTy)
	  in
	    (arg, [
		(AST.P_Tuple[Nil,        w], act "1"),
		(AST.P_Tuple[w,          Nil], act "2"),
		(AST.P_Tuple[Cons(w, w), Cons(w, w)], act "3")
	      ])
	  end

  end
