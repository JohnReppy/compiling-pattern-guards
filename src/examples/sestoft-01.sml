(* sestoft-01.sml
 *
 * An example from "ML Pattern Match Compilation and Partial Evaluation" (page 456)
 * by Peter Sestoft.
 *)

structure Sestoft01 : sig

    val example : Var.t * (AST.pat * AST.exp) list

  end = struct

    structure Ty = Type
    structure B = Basis

  (* datatype lam
   *  = Var of int | Lam of int * lam | App of lam * lam | Let of int * lam * lam
   *)
    val (lamDT, [cVar, cLam, cApp, cLet]) = DataTy.newWithCons ("lam", [
	    ("Var", SOME(fn _ => B.intTy)),
	    ("Lam", SOME(fn lamTy => Ty.T_Tuple[B.intTy, lamTy])),
	    ("App", SOME(fn lamTy => Ty.T_Tuple[lamTy, lamTy])),
	    ("Let", SOME(fn lamTy => Ty.T_Tuple[B.intTy, lamTy, lamTy]))
	  ])

  (* case arg of
   * | Var _ => 111
   * | Lam(_, Var _) => 222
   * | Lam(_, Lam(_, _)) => 333
   * | Lam(_, App(_, _)) => 444
   * | App(Lam(_, _), _) => 555
   * | App(App(_, _), _) => 666
   * | Let(_, Let(_, _, _), _) => 777
   * | Lam(_, Let(_, _, _)) => 888
   * | Let(_, _, App(_, _)) => 999
   * | App(App(Lam(_, Lam(_, _)), _), _) => 1010  (* redundant *)
   *)
    val example = let
          val arg = Var.new("arg", Ty.T_Data lamDT)
          fun Var () = AST.P_Con(cVar, SOME AST.P_Wild)
          fun Lam p = AST.P_Con(cLam, SOME(AST.P_Tuple[AST.P_Wild, p]))
          fun App (e1, e2) = AST.P_Con(cApp, SOME(AST.P_Tuple[e1, e2]))
          fun Let (e1, e2) = AST.P_Con(cLet, SOME(AST.P_Tuple[AST.P_Wild, e1, e2]))
	  fun act s = AST.E_Exp(s, B.intTy)
          in
            (arg, [
                (Var(), act "111"),
                (Lam(Var()), act "222"),
                (Lam(Lam(AST.P_Wild)), act "333"),
                (Lam(App(AST.P_Wild, AST.P_Wild)), act "444"),
                (App(Lam(AST.P_Wild), AST.P_Wild), act "555"),
                (App(App(AST.P_Wild, AST.P_Wild), AST.P_Wild), act "666"),
                (Let(Let(AST.P_Wild, AST.P_Wild), AST.P_Wild), act "777"),
                (Lam(Let(AST.P_Wild, AST.P_Wild)), act "888"),
                (Let(AST.P_Wild, App(AST.P_Wild, AST.P_Wild)), act "999"),
                (App(App(Lam(Lam AST.P_Wild), AST.P_Wild), AST.P_Wild), act "1010")
              ])
          end

  end
