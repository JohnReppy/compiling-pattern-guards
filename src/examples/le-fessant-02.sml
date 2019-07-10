(* le-fessant-02.sml
 *
 * Example from "Optimizing Pattern Matching" (page 30), by Fabrice Le Fessant and
 * Luc Maranget.
 *)

structure LeFessant02 : sig

    val example : Var.t * (AST.pat * AST.exp) list

  end = struct

    structure Ty = Type
    structure B = Basis

  (* datatype t = Nil | One of int | Cons of int * t *)
    val (tDT, [cNil, cOne, cCons]) = DataTy.newWithCons("t", [
	    ("Nil", NONE),
	    ("One", SOME(fn _ => B.intTy)),
	    ("Cons", SOME(fn tTy => Ty.T_Tuple[B.intTy, tTy]))
	  ])
    val tTy = Ty.T_Data tDT

  (* case arg of
   * | (Nil, _) => 1
   * | (_, Nil) => 2
   * | (One x, _) => 3
   * | (_, One _) => 4
   * | (Cons(_, _), Cons(_, _)) => 5
   *)
    val example = let
          val arg = Var.new("arg", Ty.T_Tuple[tTy, tTy])
	  val w = AST.P_Wild
          val Nil = AST.P_Con(cNil, NONE)
          val One = AST.P_Con(cOne, SOME w)
          fun Cons () = AST.P_Con(cCons, SOME(AST.P_Tuple[w, w]))
	  fun act s = AST.E_Exp(s, B.intTy)
          in
            (arg, [
                (AST.P_Tuple[Nil,    w     ], act "1"),
                (AST.P_Tuple[w,      Nil   ], act "2"),
                (AST.P_Tuple[One,    w     ], act "3"),
                (AST.P_Tuple[w,      One   ], act "4"),
                (AST.P_Tuple[Cons(), Cons()], act "5")
              ])
          end

  end
