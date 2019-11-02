(* backtrack.sml
 *
 * An implementation of the original backtracking compilation scheme for
 * pattern matching that was used by OCaml.  This algorithm is described
 * in Section 3 of the paper
 *
 *      Optimizing Pattern Matching
 *      by Fabrice Le Fessant and Luc Maranget
 *      ICFP 2001
 *
 * There are a few main differences from the algorithm as described in
 * the paper:
 *
 *  - we use continuation functions to implement backtracking, instead of
 *    the CATCH/EXIT operations.
 *
 *  - we have implemented the row-reordering optimization when grouping blocks
 *    in the "mixture" rule
 *
 *  - we implement the Successor ML pattern guards
 *)

structure Backtrack : sig

    val compile : Var.t * (AST.pat * AST.exp) list -> AST.exp

  (* print the result of the compilation *)
    val test : Var.t * (AST.pat * AST.exp) list -> unit

  end = struct

    structure DCon = DataCon
    structure CSet = DCon.Set
    structure PMat = PatMatrix
    structure CMat = ClauseMatrix
    structure Occ = Occurrence
    structure Ty = Type
    structure OMap = Occ.Map

    datatype exp = datatype AST.exp
    datatype pat = datatype AST.pat

  (* actions in the clause matrix are a pair of an environment specifying how
   * to access the bound variables and an expression.
   *)
    type vmap = Var.t OMap.map
    type action = vmap * exp
    type row = action CMat.row
    type cmat = action CMat.t

    fun itemToString (PMat.Pat p) = PatUtil.toString p
      | itemToString (PMat.Grd g) = concat["[", ExpUtil.toString g, "]"]

    val unitTy = Basis.unitTy

  (* make `let fun k xs = e2 in e1[k/exitK] end` *)
    fun letcont (e1, xs, e2) = let
	  val argTy = Ty.T_Tuple(List.map Var.typeOf xs)
	  val k = Var.newFn(argTy, ExpUtil.typeOf e2)
	  in
	    E_Fun((k, xs, e2), e1 k)
	  end

  (* convert an action pair to an expression *)
    fun actToExp (vMap, exp) = let
	  fun mkLet (occ, x, exp) = AST.E_Let(x, Occ.toExp occ, exp)
	  in
	    OMap.foldli mkLet exp vMap
	  end

  (* return a list of the variables that have occurrences in the given action pair *)
    fun varsOf (vMap, _) = let
	  fun f (occ, _, vset) = Var.Set.add(vset, Occ.varOf occ)
	  in
	    Var.Set.toList(OMap.foldli f Var.Set.empty vMap)
	  end

  (* add an occurrence to variable binding to an action pair *)
    fun bindVar ((vMap, exp), occ, x) = (OMap.insert(vMap, occ, x), exp)

  (* classification of a block of rows according to the "Classical Scheme"
   * (Section 3.3) of Le Fessant and Maranget.  Note that we have added a
   * case for where the first column has a tuple type and is not all variables.
   *)
    datatype row_block
      = Vars of cmat			(* variables in first column *)
      | Tuple of cmat			(* tuple pattern in first column *)
      | Constr of CSet.set * cmat	(* constructors in first column *)
      | OrRow of (pat list * row)	(* or-pattern row *)
      | Guard of cmat			(* pattern guard in first column of first row *)

  (* test two rows for compatibility *)
    fun compatible (ps1, _) (ps2, _) = let
	  fun compatItem (PMat.Pat p1, PMat.Pat p2) = PatUtil.compatible(p1, p2)
	    | compatItem _ = true (* be conservative about guards *)
	  in
	    ListPair.allEq compatItem (ps1, ps2)
	  end

  (* split a matrix into blocks of rows based on the first column of patterns *)
    fun split (rows : row list) = let
	(* return the first pattern of a row *)
	  fun firstPat (PMat.Pat p :: _, _) = p
	    | firstPat ([],  _) = raise Fail "firstPat: empty row"
	    | firstPat _ = raise Fail "firstPat: unexpected guard"
	(* helper function to make a matrix from a reversed list of rows *)
	  fun mkMat rows = CMat.fromRows(rev rows)
	(* figure out what the next block is going to be and call the
	 * appropriate collection function.
	 *)
	  fun collect (row::rowr, blks) = (case firstPat row
	       of P_Wild => collectVars (rowr, [row], blks)
		| P_Var _ => collectVars (rowr, [row], blks)
		| P_Tuple _ => Tuple(CMat.fromRows(row::rowr)) :: blks
		| P_Con(c, _) => collectCons (c, rowr, [row], blks)
		| P_Or ps => let
		    val (_::rowPats, act) = row
		    val blk = OrRow (ps, (rowPats, act))
		    in
		      if null rowr
			then blk :: blks
			else collect (rowr, blk :: blks)
		    end
		| P_If _ => Guard(CMat.fromRows(row::rowr)) :: blks
	      (* end case *))
	    | collect _ = raise Fail "expected rows"
	(* collect a prefix of rows that have a variable pattern
	 * in the first column.
	 *)
	  and collectVars ([], rows, blks) = Vars(mkMat rows)::blks
	    | collectVars (row::rowr, rows, blks) = (case firstPat row
		 of P_Var _ => collectVars (rowr, row::rows, blks)
		  | P_Wild => collectVars (rowr, row::rows, blks)
		  | _ => collect (row::rowr, Vars(mkMat rows)::blks)
		(* end case *))
	(* collect a prefix of rows that have a constructor pattern
	 * in the first column.  We keep track of the set of constructors
	 * so that we can detect if the rows are exhaustive.
	 *)
	  and collectCons (c, rowr, rows, blks) = let
	      (* collect the rows that start with a constructor into a Constr block *)
		fun collect' ([], rows, cset) = Constr(cset, mkMat rows) :: blks
		  | collect' (row::rowr, rows, cset) = (case firstPat row
		       of P_Con(c, _) =>
			    collect' (rowr, row::rows, CSet.add(cset, c))
			| _ => let
(* FIXME: this transformation is not safe if there are columns that involve reference cells *)
			  (* check the rest of the rows to see if there are any rows with a
			   * head constructor that can be lifted up to the Constr block
			   * we are building.
			   *)
			    fun collect'' ([], cRows, cset, rRows) = (cRows, cset, rRows)
			      | collect'' (row::rowr, cRows, cset, rRows) = (
				  case firstPat row
				   of P_Con(c, _) => if List.exists (compatible row) rRows
					then collect'' (rowr, cRows, cset, row::rRows)
					else (* row can be lifted into the "C" matrix *)
					  collect'' (rowr, row::cRows, CSet.add(cset, c), rRows)
				    | _ => collect'' (rowr, cRows, cset, row::rRows)
				  (* end case *))
			    val (cRows, cset, rRows) = collect'' (row::rowr, rows, cset, [])
			    in
			      collect (rev rRows, Constr(cset, mkMat cRows)::blks)
			    end
		      (* end case *))
		in
		  collect' (rowr, rows, CSet.singleton c)
		end
	  in
	    List.rev (collect (rows, []))
	  end

  (* `comp (occs, mat)` generates code for the pattern matrix `mat` matching
   * the variable occurrences `occs`.  This function splits the matrix into blocks
   * (essentially the mixture rule) and then applies the appropriate translation
   * function to each block.
   *)
    fun comp (occs, mat : cmat, exitK) = (case CMat.toRows mat
	   of (([], act)::_) => actToExp act (* no columns left *)
	    | rows as ((PMat.Grd _ :: _, _)::_) => (* first column is a guard column *)
		compGuardCol (occs, rows, exitK)
	    | rows => (case (occs, split rows)
		 of (occ::occr, blk::blkr) => let
		    (* invoke the appropriate translation function for the block *)
		      fun dispatch (Vars mat) k =
			    variableRule (occ, occr, mat, k)
			| dispatch (Tuple mat) k =
			    tupleRule (occ, occr, mat, k)
			| dispatch (Constr(cset, mat)) k =
			    constructorRule (occ, occr, cset, mat, k)
			| dispatch (OrRow(orPats, row)) k =
			    orRule (occ, occr, orPats, row, k)
			| dispatch (Guard mat) k =
			    guardRule (occ, occr, mat, k)
		    (* chain together the blocks using continuation functions for backtracking
		     *)
		      fun f (blk, []) k = dispatch blk k
			| f (blk1, blk2::blks) k =
			    letcont (dispatch blk1, [], f (blk2, blks) k)
		      in
			f (blk, blkr) exitK
		      end
		  | _ => raise Fail "impossible: bogus matrix"
		(* end case *))
	  (* end case *))

  (* compile a guard column *)
    and compGuardCol (_::xr, rows, exitK) = let
	(* split the rows of the pattern matrix into blocks, where each non-trivial
	 * guard is the first row of a block.
	 *)
	  fun split [] = []
	    | split ((PMat.Grd pred :: pats, act)::rowr) = let
		val row = (PMat.Pat(P_Con(Basis.trueCon, NONE)) :: pats, act)
		val (rows, blks) = getRows rowr
		in
		  {pred=pred, rows=row::rows} :: blks
		end
	    | split _ = raise Fail "expected guard in first position"
	  and getRows (rows as (PMat.Grd exp :: pats, act)::rowr) = if ExpUtil.isTrue exp
		then let
		  val (rows, blks) = getRows rowr
		  in
		    ((PMat.Pat(P_Var(Var.newTmp Basis.boolTy)) :: pats, act) :: rows, blks)
		  end
		else ([], split rows)
	    | getRows rows = ([], split rows)
	(* compile a guard block into target code *)
	  fun compBlk ({pred, rows}, k) = if ExpUtil.isTrue pred
		then let
		(* a block of true guards, so remove the guard (first) column and
		 * compile the resulting matrix.
		 *)
		  val pm = CMat.fromRows (List.map (fn (_::ps, act) => (ps, act)) rows)
		  in
		    comp (xr, pm, k)
		  end
		else (case rows
		   of [(_::pats, act as (vmap, exp))] =>
			actToExp (vmap,
			  E_If(pred,
			    comp(xr, CMat.fromRows[(pats, act)], k),
			    ExpUtil.call(k, List.map AST.E_Var (varsOf act))))
		    | (_, act as (vmap, exp))::_ => let
			val bv = Var.newTmp Basis.boolTy
			in
			  actToExp (vmap,
			    E_Let(bv, pred, comp(Occ.var bv ::xr, CMat.fromRows rows, k)))
			end
		    | [] => raise Fail "unexpected empty row list"
		  (* end case *))
	(* compile the sequence of blocks into target code *)
	  fun compBlks [] = raise Fail "impossible"
	    | compBlks [blk] = compBlk (blk, exitK)
	    | compBlks (blk::blks) = letcont (fn k' => compBlk(blk, k'), [], compBlks blks)
	  in
	    compBlks (split rows)
	  end

    and variableRule (x : Occ.t, xr, mat, exitK) = let
        (* shift the first-column variable to a let binding in the action *)
          fun doRow (PMat.Pat(P_Var y) :: rowPats, act, rows) =
                (rowPats, bindVar(act, x, y)) :: rows
	    | doRow (PMat.Pat P_Wild :: rowPats, act, rows) =
		(rowPats, act) :: rows
	    | doRow (p :: _, _, _) = raise Fail(concat[
		  "impossible: non-variable pattern (", itemToString p, ")"
		])
	    | doRow ([], _, _) = raise Fail "impossible: empty row"
          in
            comp (xr, CMat.fromRows (CMat.foldRows doRow [] mat), exitK)
          end

  (* replace the first column with one column per tuple component *)
    and tupleRule (x : Occ.t, xr, mat, exitK) = let
          val tys = (case Occ.typeOf x
		 of Ty.T_Tuple tys => tys
		  | ty => raise Fail("expected tuple type, but got " ^ Type.toString ty)
		(* end case *))
	(* expand the patterns in the first column *)
          fun doRow (PMat.Pat(P_Tuple pats) :: rowPats, act, rows) =
                (List.map PMat.Pat pats @ rowPats, act) :: rows
            | doRow (PMat.Pat(P_Var y) :: rowPats, act, rows) = let
                val pats = List.map (fn _ => PMat.Pat P_Wild) tys
                in
                  (pats @ rowPats, bindVar(act, x, y)) :: rows
                end
	    | doRow (PMat.Pat P_Wild :: rowPats, act, rows) = let
                val pats = List.map (fn _ => PMat.Pat P_Wild) tys
                in
                  (pats @ rowPats, act) :: rows
                end
            | doRow (PMat.Pat(P_Or pats) :: rowPats, act, rows) =
		raise Fail "TODO: or-pattern in tuple column"
	    | doRow (p :: _, _, _) = raise Fail(concat[
		  "impossible: non-tuple pattern (", itemToString p, ")"
		])
	    | doRow ([], _, _) = raise Fail "impossible: empty row"
	(* expand the argument occurrences *)
          val xs' = Occ.tuple(x, List.length tys)
          in
	    comp (xs' @ xr, CMat.fromRows (CMat.foldRows doRow [] mat), exitK)
          end

    and constructorRule (x : Occ.t, xr, cset, mat, exitK) = let
          val dt = (case Occ.typeOf x
		 of Ty.T_Data dt => dt
		  | ty => raise Fail("expected datatype, but got " ^ Type.toString ty)
		(* end case *))
        (* make the switch case for the constructor `dc` *)
          fun mkCase (dc, branches) = let
                val (y, ys) = (case DCon.argTy dc
                       of SOME ty => let val y = Var.newTmp ty
                            in (SOME(P_Var y), Occ.var y :: xr) end
                        | NONE => (NONE, xr)
                      (* end case *))
                fun doRow (PMat.Pat(P_Con(dc', SOME pat)) :: pr, act, rows) =
                      if DCon.same(dc, dc')
                        then (PMat.Pat pat :: pr, act) :: rows
                        else rows
                  | doRow (PMat.Pat(P_Con(dc', NONE)) :: pr, act, rows) =
                      if DCon.same(dc, dc')
                        then (pr, act) :: rows
                        else rows
		  | doRow (p :: _, _, _) = raise Fail(concat[
			"impossible: non-constructor pattern (", itemToString p, ")"
		      ])
		  | doRow ([], _, _) = raise Fail "impossible: empty row"
                val act = comp (ys, CMat.fromRows (CMat.foldRows doRow [] mat), exitK)
                in
                  (P_Con(dc, y), act) :: branches
                end
        (* is the column exhaustive for the datatype? *)
          val default = if (DataTy.span dt = CSet.numItems cset)
                then []
                else [(P_Wild, ExpUtil.call(exitK, []))]
          in
            E_Case(Occ.toExp x, CSet.foldr mkCase default cset)
          end

    and orRule (x : Occ.t, xr, orPats, (rowPats, act), exitK) = let
	(* get the variables that are bound by the or pattern *)
	  val xs = PatUtil.varsOf (List.hd orPats)
	  fun mkOrRow k p = ([PMat.Pat p], (OMap.empty, ExpUtil.call(k, List.map AST.E_Var xs)))
	  fun orMat k = CMat.fromRows (List.map (mkOrRow k) orPats)
	  in
	    letcont (
	      fn k => comp ([x], orMat k, exitK),
	      xs,
	      comp (xr, CMat.fromRows [(rowPats, act)], exitK))
	  end

  (* split the first column into a pattern column and a guard column *)
    and guardRule (x : Occ.t, xr, mat, exitK) = let
	  fun doRow (PMat.Pat p :: ps, act) = let
		val (p, g) = (case p
		       of P_If(p, g) => (p, g)
			| _ => (p, ExpUtil.kTrue)
		      (* end case *))
		in
		  (PMat.Pat p :: PMat.Grd g :: ps, act)
		end
	    | doRow _ = raise Fail "unexpected guard column"
	  val dummy = Occ.var(Var.newTmp(Ty.T_Base "void"))
	  in
	    comp (x::dummy::xr, CMat.mapRows doRow mat, exitK)
	  end

    fun compile (x, cases) = let
	  fun mkRow (pat, exp) = (pat, (OMap.empty, exp))
	  in
	    letcont (
	      fn exitK => comp ([Occ.var x], CMat.fromMatch (List.map mkRow cases), exitK),
	      [],
	      E_Raise(E_Con Basis.matchExn, ExpUtil.typeOf(#2(hd cases))))
	  end

    fun test arg = (
	  PPAST.pr(E_Case(E_Var(#1 arg), #2 arg));
	  print "\n  ==>\n\n";
	  PPAST.pr(compile arg))

  end
