(* t4.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * example extracted from yacc.grm.sml
 *)

structure T4 : sig

    val example : Var.t * (AST.pat * AST.exp) list

  end = struct

    structure Ty = Type
    structure B = Basis

    val (svalueDT, [
	    cC0, cC1, cC2, cC3, cC4, cC5, cC6, cC7, cC8, cC9,
	    cC10, cC11, cC12, cC13, cC14, cC15, cC16, cC17, cC18, cC19,
	    cC20, cC21, cC22, cC23
	  ]) =
	  DataTy.newWithCons ("svalue", [
	     ("C0", NONE),
	     ("C1", SOME(fn _ => B.unitTy)),
	     ("C2", SOME(fn _ => B.unitTy)),
	     ("C3", SOME(fn _ => B.unitTy)),
	     ("C4", SOME(fn _ => B.unitTy)),
	     ("C5", SOME(fn _ => B.unitTy)),
	     ("C6", SOME(fn _ => B.unitTy)),
	     ("C7", SOME(fn _ => B.unitTy)),
	     ("C8", SOME(fn _ => B.unitTy)),
	     ("C9", SOME(fn _ => B.unitTy)),
	     ("C10", SOME(fn _ => B.unitTy)),
	     ("C11", SOME(fn _ => B.unitTy)),
	     ("C12", SOME(fn _ => B.unitTy)),
	     ("C13", SOME(fn _ => B.unitTy)),
	     ("C14", SOME(fn _ => B.unitTy)),
	     ("C15", SOME(fn _ => B.unitTy)),
	     ("C16", SOME(fn _ => B.unitTy)),
	     ("C17", SOME(fn _ => B.unitTy)),
	     ("C18", SOME(fn _ => B.unitTy)),
	     ("C19", SOME(fn _ => B.unitTy)),
	     ("C20", SOME(fn _ => B.unitTy)),
	     ("C21", SOME(fn _ => B.unitTy)),
	     ("C22", SOME(fn _ => B.unitTy)),
	     ("C23", SOME(fn _ => B.unitTy))
	    ])

    val sppTy = Ty.T_Tuple[Ty.T_Data svalueDT, B.intTy, B.intTy]
    val {ty=stackTy, nilCon=stkNil, consCon=stkCons} = B.newListTy ("stack", sppTy)

    infixr :::
    val op ::: = fn (x, xs) => AST.P_Con(stkCons, SOME(AST.P_Tuple[x, xs]))

    (* integer literal patterns *)
    fun iPat n = AST.P_Lit n

    (* integer literal expressions *)
    fun iExp n = AST.E_Exp(Int.toString n, B.intTy)

    fun pair (p1, p2) = AST.P_Tuple[p1, p2]
    fun trpl (p1, p2, p3) = AST.P_Tuple[p1, p2, p3]

    fun c con var = AST.P_Con(con, SOME(AST.P_Var(Var.new(var, B.unitTy))))

    val wild = AST.P_Wild

    (* integer variable pattern *)
    fun iv x = AST.P_Var(Var.new(x, B.intTy))

    (* stack variable pattern *)
    fun sv x = AST.P_Var(Var.new(x, stackTy))

    val example = let
          val arg = Var.new("arg", Ty.T_Tuple[B.intTy, stackTy])
	  in
            (arg, [
		(pair (iPat 0, trpl (c cC14 "x0a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC19 "x0b", wild, wild)
		  ::: trpl (c cC6 "x0c", iv "left", wild)
		  ::: sv "rest"),
		  iExp 0),
	        (pair (iPat 1, trpl (c cC20 "x1a", iv "left1", iv "right")
		 ::: trpl (c cC19 "x1b", iv "left2", wild)
		 ::: sv "rest"), iExp 1),
	        (pair (iPat 2, sv "rest"), iExp 2),
	        (pair (iPat 3, trpl (c cC23 "x3", wild, iv "right")
		 ::: trpl (wild, iv "left", wild)
		 ::: sv "rest"), iExp 3),
	        (pair (iPat 4, trpl (c cC23 "x4", wild, iv "right")
		 ::: trpl (wild, iv "left", wild)
		 ::: sv "rest"), iExp 4),
	        (pair (iPat 5, trpl (c cC22 "x5a", wild, iv "right")
		 ::: trpl (c cC2 "x5b", iv "left", wild)
		 ::: sv "rest"), iExp 5),
	        (pair (iPat 6, trpl (c cC5 "x6", wild, iv "right")
		 ::: trpl (wild, iv "left", wild)
		 ::: sv "rest"), iExp 6),
	        (pair (iPat 7, trpl (c cC22 "x7", wild, iv "right")
		 ::: trpl (wild, iv "left", wild)
		 ::: sv "rest"), iExp 7),
	        (pair (iPat 8, trpl (c cC22 "x8", wild, iv "right")
		 ::: trpl (wild, iv "left", wild)
		 ::: sv "rest"), iExp 8),
	        (pair (iPat 9, trpl (c cC22 "x9", wild, iv "right")
		 ::: trpl (wild, iv "left", wild)
		 ::: sv "rest"), iExp 9),
	        (pair (iPat 10, trpl (c cC10 "x10", wild, iv "right")
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 10),
	        (pair (iPat 11, trpl (c cC12 "x11", wild, iv "right")
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 11),
	        (pair (iPat 12, trpl (c cC22 "x12", wild, iv "right")
		  ::: trpl ( wild ,iv "left", wild)
		  ::: sv "rest"), iExp 12),
	        (pair (iPat 13, trpl (c cC1 "x13", wild, iv "right")
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 13),
	        (pair (iPat 14, trpl (c cC1 "x14", wild, iv "right")
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 14),
	        (pair (iPat 15, trpl (c cC5 "x15", wild, iv "right")
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 15),
	        (pair (iPat 16, trpl (c cC8 "x16a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC1 "x16b", wild, wild)
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 16),
	        (pair (iPat 17, trpl (wild, iv "left", iv "right")
		  ::: sv "rest"), iExp 17),
	        (pair (iPat 18, trpl (wild, iv "left", iv "right")
		  ::: sv "rest"), iExp 18),
	        (pair (iPat 19, trpl (wild, iv "left", iv "right")
		  ::: sv "rest"), iExp 19),
	        (pair (iPat 20, trpl (c cC8 "x20", wild, iv "right")
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 20),
	        (pair (iPat 21, trpl (c cC1 "x21a", wild, iv "right")
		  ::: trpl (c cC5 "x21b", wild, wild)
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 21),
	        (pair (iPat 22, trpl (c cC10 "x22a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC9 "x22b", iv "left", wild)
		  ::: sv "rest"), iExp 22),
	        (pair (iPat 23, trpl (c cC9 "x23", iv "left", iv "right")
		  ::: sv "rest"), iExp 23),
	        (pair (iPat 24, trpl (c cC22 "x24a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC22 "x24b", iv "left", wild)
		  ::: sv "rest"), iExp 24),
	        (pair (iPat 25, trpl (c cC12 "x25a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC11 "x25b", iv "left", wild)
		  ::: sv "rest"), iExp 25),
	        (pair (iPat 26, trpl (c cC11 "x26", iv "left", iv "right")
		  ::: sv "rest"), iExp 26),
	        (pair (iPat 27, trpl (c cC5 "x27a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC5 "x27b", iv "left", wild)
		  ::: sv "rest"), iExp 27),
	        (pair (iPat 28, trpl (c cC8 "x28a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC5 "x28b", wild, wild)
		  ::: wild
		  ::: trpl (c cC23 "x28c", iv "left", wild)
		  ::: sv "rest"), iExp 28),
	        (pair (iPat 29, trpl (c cC5 "x29a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC23 "x29b", iv "left", wild)
		  ::: sv "rest"), iExp 29),
	        (pair (iPat 30, trpl (c cC8 "x30a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC5 "x30b", iv "left", wild)
		  ::: sv "rest"), iExp 30),
	        (pair (iPat 31, trpl (c cC5 "x31", iv "left", iv "right")
		  ::: sv "rest"), iExp 31),
	        (pair (iPat 32, trpl (c cC16 "x32a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC5 "x32b", iv "left", wild)
		  ::: sv "rest"), iExp 32),
	        (pair (iPat 33, trpl (c cC15 "x33a", wild, iv "right")
		  ::: trpl (c cC14 "x33b", iv "left", wild)
		  ::: sv "rest"), iExp 33),
	        (pair (iPat 34, trpl (c cC15 "x34", iv "left", iv "right")
		  ::: sv "rest"), iExp 34),
	        (pair (iPat 35, trpl (c cC22 "x35a", wild, iv "right")
		  ::: trpl (c cC5 "x35b", iv "left", wild)
		  ::: sv "rest"), iExp 35),
	        (pair (iPat 36, sv "rest"), iExp 36),
	        (pair (iPat 37, trpl (c cC1 "x37a", wild, iv "right")
		  ::: trpl (c cC13 "x37b", wild, wild)
		  ::: trpl (c cC22 "x37c", iv "left", wild)
		  ::: sv "rest"), iExp 37),
	        (pair (iPat 38, trpl (c cC1 "x38a", wild, iv "right")
		  ::: trpl (c cC13 "x38b", wild, wild)
		  ::: trpl (c cC22 "x38c", wild, wild)
		  ::: wild
		  ::: trpl (c cC16 "x38d", iv "left", wild)
		  ::: sv "rest"), iExp 38),
	        (pair (iPat 39, trpl (c cC7 "x39", iv "left", iv "right")
		  ::: sv "rest"), iExp 39),
	        (pair (iPat 40, trpl (wild, wild, iv "right")
		  ::: trpl (c cC17 "x40", wild, wild)
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 40),
	        (pair (iPat 41, trpl (wild, wild, iv "right")
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 41),
	        (pair (iPat 42, trpl (c cC1 "x42", iv "left", iv "right")
		  ::: sv "rest"), iExp 42),
	        (pair (iPat 43, trpl (c cC18 "x43a", wild, iv "right")
		  ::: trpl (c cC8 "x43b", iv "left", wild)
		  ::: sv "rest"), iExp 43),
	        (pair (iPat 44, trpl (c cC18 "x44", iv "left", iv "right")
		  ::: sv "rest"), iExp 44),
	        (pair (iPat 45, trpl (c cC8 "x45a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC8 "x45b", iv "left", wild)
		  ::: sv "rest"), iExp 45),
	        (pair (iPat 46, trpl (c cC8 "x46a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC8 "x46b", iv "left", wild)
		  ::: sv "rest"), iExp 46),
	        (pair (iPat 47, trpl (c cC8 "x47a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC21 "x47b", wild, wild)
		  ::: wild
		  ::: trpl (c cC17 "x47c", iv "left", wild)
		  ::: sv "rest"), iExp 47),
	        (pair (iPat 48, trpl (c cC8 "x48a", wild, iv "right")
		  ::: wild
		  ::: trpl (c cC21 "x48b", iv "left", wild)
		  ::: sv "rest"), iExp 48),
	        (pair (iPat 49, trpl (c cC5 "x49", iv "left", iv "right")
		  ::: sv "rest"), iExp 49),
	        (pair (iPat 50, trpl (c cC18 "x50a", wild, iv "right")
		  ::: trpl (c cC4 "x50b", iv "left", wild)
		  ::: sv "rest"), iExp 50),
	        (pair (iPat 51, trpl (c cC5 "x51", iv "left", iv "right")
		  ::: sv "rest"), iExp 51),
	        (pair (iPat 52, trpl (c cC3 "x52", iv "left", iv "right")
		  ::: sv "rest"), iExp 52),
	        (pair (iPat 53, trpl (c cC5 "x53", wild, iv "right")
		  ::: trpl (wild, iv "left", wild)
		  ::: sv "rest"), iExp 53),
	        (pair (iPat 54, sv "rest"), iExp 54),
	        (wild, iExp 55)
	      ])
	  end

  end
