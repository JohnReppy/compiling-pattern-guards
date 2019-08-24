(* match-comp.sml
 *
 * Pattern match compilation based on Pettersson's DFA-based approach
 * as described in his Ph.D. dissertation:
 *
 *	Compiling Natural Semantics
 *	LNCS 1549, Springer-Verlag, 1999
 *
 * There are a few main differences from the algorithm as described in
 * the book:
 *
 *  - we use the target-language AST to represent the DFA, instead of
 *    defining a DFA data structure
 *
 *  - because of the above choice, we do not merge equivalent states (Step 3
 *    of Pettersson's algorithm).
 *
 *  - we implement the Successor ML pattern guards
 *
 * Also note that this version does not support disjunctive patterns.
 *)

structure MatchComp : sig

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

  (* print a clause matrix (for debugging) *)
    val prMat = let
	  fun bind2s (occ, x) = concat[Occ.toString occ, ":=", Var.toString x]
	  fun act2s (vMap, e) = String.concat[
		  "{", String.concatWithMap "," bind2s (OMap.listItemsi vMap),
		  "} ", ExpUtil.toString e
		]
	  in
	    CMat.print act2s
	  end

  (* convert an action pair to an expression *)
    fun actToExp (vMap, exp) = let
	  fun mkLet (occ, x, exp) = E_Let(x, Occ.toExp occ, exp)
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

  (* replace the element of `xs` at position `i` with `ys` *)
    fun replace (xs, i, ys) = let
	  fun lp (0, prefix, x::xs) = List.revAppend(prefix, ys @ xs)
	    | lp (n, prefix, x::xs) = lp(n-1, x::prefix, xs)
	    | lp (_, _, []) = raise Subscript
	  in
	    lp (i, [], xs)
	  end

  (* convert an action pair to an expression *)
    fun actToExp (vMap, exp) = let
          fun mkLet (occ, x, exp) = E_Let(x, Occ.toExp occ, exp)
          in
            OMap.foldli mkLet exp vMap
          end

  (* return true if an item is a pattern guard *)
    fun isGuardPat (PMat.Pat(P_If _)) = true
      | isGuardPat _ = false

  (* the trivial guard *)
    val trivGuard = PMat.Grd(E_Con Basis.trueCon)

  (* is a guard item trivial? *)
    fun isTrivial (E_Con dc) = DCon.same(dc, Basis.trueCon)
      | isTrivial _ = false

  (* is an item the trivial guard? *)
    fun isTrivGuard (PMat.Grd(E_Con dc)) = DCon.same(dc, Basis.trueCon)
      | isTrivGuard _ = false

  (* return true if an item is **not** a variable/wild-card pattern or trivial guard *)
    fun isRefutable (PMat.Pat p) = not(PatUtil.isVar p)
      | isRefutable (PMat.Grd g) = not(isTrivial g)

  (* dummy occurrence ("∙" in the paper) *)
    val dummy = Occ.var(Var.new("*dummy*", Basis.unitTy))

  (* for any column in a clause matrix that contains a guard pattern, split
   * the column.  Repeat until all possible splits have been done.
   *)
    fun guardSplit (occs, mat) = let
	  fun expandItem (PMat.Pat(P_If(p, g))) = [PMat.Pat p, PMat.Grd g]
	    | expandItem (PMat.Pat p) = [PMat.Pat p, trivGuard]
	    | expandItem _ = raise Fail "unexpected guard item"
        (* expand columns from left to right with the invariant that
         * the columns < col do not contain any top-level pattern guards.
         *)
	  fun expand ([], occs', mat, _) = (List.rev occs', mat)
	    | expand (occ::occs, occs', mat, c) =
		if CMat.existsCol isGuardPat (mat, c)
		  then expand (occs, dummy::occ::occs', CMat.expandCol (mat, c, expandItem), c)
		  else expand (occs, occ::occs', mat, c+1)
	  in
	    expand (occs, [], mat, 0)
	  end

  (* remove any trivial guard columns *)
    fun removeTrivialGuards (occs, mat) = let
	  fun contract ([], occs', mat, _) = (occs', mat)
	    | contract (occ::occs, occs', mat, c) =
		if CMat.allCol isTrivGuard (mat, c)
		  then contract (occs, occs', CMat.removeCol(mat, c), c-1)
		  else contract (occs, occ::occs', mat, c-1)
	  in
	    contract (List.rev occs, [], mat, #ncols(CMat.size mat) - 1)
	  end

  (* generate an expression for matching the occurrences with a matrix *)
    fun comp (occs, mat) = let
	(* first we apply the "Guard Split" and "Trivial Guard" rules *)
	  val (occs, mat) = removeTrivialGuards (guardSplit (occs, mat))
	(* get the first row from the matrix *)
	  val (row, act) = CMat.row mat 0
	  in
	  (* analyse the first row *)
	    case List.findi (fn (_, item) => isRefutable item) row
	     of NONE => variableRule (occs, row, act)
	      | SOME(ix, PMat.Pat(P_Tuple _)) => tupleRule (occs, mat, ix)
	      | SOME(ix, PMat.Pat(P_Con(dc, _))) =>
		  if DCon.isRef dc
		    then referenceRule (occs, mat, ix, DCon.owner dc)
		    else mixtureRule (occs, mat, ix, DCon.owner dc)
	      | SOME(ix, g as PMat.Grd _) => guardMatchRule (occs, mat, ix)
	      | _ => raise Fail "impossible"
	    (* end case *)
	  end

  (* all items in the first row are trivial, so bind variables and return the action *)
    and variableRule (occs, row, act) = let
	  fun doPat (occ, PMat.Pat(P_Var x), act) = bindVar(act, occ, x)
	    | doPat (_, _, act) = act
	  in
	    actToExp (ListPair.foldlEq doPat act (occs, row))
	  end

  (* apply the tuple rule to column c *)
    and tupleRule (occs, mat, c) = let
	(* get the corresponding occurrence *)
	  val occ = List.nth(occs, c)
	(* get types of expansion *)
          val tys = (case Occ.typeOf occ
		 of Ty.T_Tuple tys => tys
		  | ty => raise Fail("expected tuple type, but got " ^ Type.toString ty)
		(* end case *))
	(* expand the occurrence at position c *)
	  val occs' = replace (occs, c, Occ.tuple(occ, List.length tys))
	(* expand column c in a row and update the occurrence map as necessary *)
	  fun doRow (ps, act) = let
		fun repl (0, prefix, PMat.Pat p :: rest) = let
		      val (ps, act') = (case p
			     of P_Tuple pats => (List.map PMat.Pat pats, act)
			      | P_Var x => let
				  val act' = (OMap.insert(#1 act, occ, x), #2 act)
				  in
				    (List.map (fn _ => PMat.Pat P_Wild) tys, act')
				  end
			      | P_Wild => (List.map (fn _ => PMat.Pat P_Wild) tys, act)
			      | _ => raise Fail "bogus tuple pattern"
			    (* end case *))
		      in
		        (List.revAppend(prefix, ps @ rest), act')
		      end
		  | repl (n, prefix, pg :: rest) = repl (n-1, pg::prefix, rest)
		  | repl _ = raise Fail "bogus row"
		in
		  repl (c, [], ps)
		end
	  val mat' = CMat.mapRows doRow mat
	  in
	    comp (occs', mat')
	  end

  (* if the matrix has a non-trivial guard or pattern guard, then
   * apply the reference rule to the matrix.  Otherwise, we fallback
   * on the mixture rule.
   *)
    and referenceRule (occs, mat, c, dt) = let
	  fun mayHaveEffect (PMat.Pat p) = PatUtil.hasGuard p
	    | mayHaveEffect (PMat.Grd g) = not(isTrivial g)
	  in
	    case CMat.findRowIndex mayHaveEffect mat
	     of NONE => mixtureRule (occs, mat, c, dt)
	      | SOME rx => if (rx = #nrows(CMat.size mat) - 1)
		  then (* no split, so fallback to mixture rule *)
		    mixtureRule (occs, mat, c, dt)
		  else let
		    val (mat1, mat2) = CMat.splitAtRow (mat, rx+1)
		    val exp2 = comp (occs, mat2)
		  (* construct the default row for the first matrix that
		   * trys the second matrix when there is no match.
		   *)
		    val dfltRow = let
			  fun mkItem (PMat.Pat _) = PMat.Pat P_Wild
			    | mkItem (PMat.Grd _) = trivGuard
			  val dfltItems = List.map mkItem (CMat.matSig mat1)
			  in
			    (dfltItems, (OMap.empty, exp2))
			  end
		    in
		      mixtureRule (occs, CMat.appendRow(mat1, dfltRow), c, dt)
		    end
	    (* end case *)
	  end

  (* apply the mixture rule to column c *)
    and mixtureRule (occs, mat, c, dt) = let
	(* get the occurrence for column `c` *)
	  val occ = List.nth(occs, c)
	(* compute the set of constructors that appear in column c *)
	  val cset = let
		fun collect (PMat.Pat(P_Con(dc, _)), cset) = CSet.add(cset, dc)
		  | collect (_, cset) = cset
		in
		  CMat.foldCol collect CSet.empty (mat, c)
		end
	(* project out the rows that match a given constructor in column c *)
	  fun projRows dc = let
		fun pred (P_Con(dc', _)) = DCon.same(dc, dc')
		  | pred (P_Var _) = true
		  | pred (P_Wild) = true
		  | pred _ = raise Fail "bogus constructor pattern"
		in
		  CMat.filterRowsByCol pred (mat, c)
		end
	(* generate the match rule for the given constructor *)
	  fun doCons dc = (case DCon.argTy dc
		 of SOME ty => let
		      val arg = Var.newTmp ty
		    (* replace the occurrence at position `c` with `arg` *)
		      val occs' = replace (occs, c, [Occ.var arg])
		    (* process a row by rewriting the pattern in column `c`  *)
		      fun doRow (ps, act) = let
			    fun repl (0, prefix, PMat.Pat p :: pr) = let
				  val (p', act') = (case p
					 of P_Con(_, SOME q) => (q, act)
					  | P_Var x => (P_Wild, bindVar(act, occ, x))
					  | P_Wild => (p, act)
					  | _ => raise Fail "impossible"
					(* end case *))
				  in
				    (List.revAppend(prefix, PMat.Pat p' :: pr), act')
				  end
			      | repl (n, prefix, pg :: pr) = repl(n-1, pg::prefix, pr)
			      | repl _ = raise Fail "impossible"
			    in
			      repl (c, [], ps)
			    end
		      val rows' = List.map doRow (projRows dc)
		    (* compile the residual matrix *)
		      val exp = comp (occs', CMat.fromRows rows')
		      in
			(P_Con(dc, SOME(P_Var arg)), exp)
		      end
		  | NONE => let
		    (* remove the corresponding variable occurrence *)
		      val occs' = replace (occs, c, [])
		    (* remove the pattern at position c *)
		      fun doRow (ps, act) = (replace (ps, c, []), act)
		      val rows' = List.map doRow (projRows dc)
		    (* compile the residual matrix *)
		      val exp = comp (occs', CMat.fromRows rows')
		      in
			(P_Con(dc, NONE), exp)
		      end
		(* end case *))
	(* check for the need for a default case *)
	  val default = if (DataTy.span dt = CSet.numItems cset)
		  then []
		else (case CMat.filterRowsByCol PatUtil.isVar (mat, c)
		   of [] => (* default rule is the error case *)
(* FIXME: unitTy is not correct, but it does not matter for now *)
			[(P_Wild, E_Raise(E_Con Basis.matchExn, Basis.unitTy))]
		    | rows => let
		      (* remove the corresponding variable occurrence *)
			val occs' = replace (occs, c, [])
		      (* process a row by rewriting the pattern in column `c`  *)
			fun doRow (ps, act) = let
			      fun repl (0, prefix, PMat.Pat p :: pr) = let
				    val act' = (case p
					   of P_Var x => bindVar(act, occ, x)
					    | _ => act
					  (* end case *))
				    in
				      (List.revAppend(prefix, pr), act')
				    end
				| repl (n, prefix, pg :: pr) = repl(n-1, pg::prefix, pr)
				| repl _ = raise Fail "impossible"
			      in
				repl (c, [], ps)
			      end
			val rows' = List.map doRow rows
		      (* compile the residual matrix *)
			val exp = comp (occs', CMat.fromRows rows')
			in
			  [(P_Wild, exp)]
			end
		  (* end case *))
	  in
	    E_Case(
	      Occ.toExp occ,
	      CSet.foldr (fn (dc, rules) => doCons dc :: rules) default cset)
	  end

  (* apply the guard-match rule to column c *)
    and guardMatchRule (occs, mat, c) = let
	  val flg = Var.new("flg", Basis.boolTy)
	  val occs' = replace (occs, c, [Occ.var flg])
	(* process the first row by extracting the guard expression
	 * in column `c` and replacing it with the constant "true" pattern.
	 * We also replace variable patterns to the left of column `c` with
	 * wildcards and add the variables to the occurrence map.
	 * The variable bindings may be needed to evaluate the guard
	 * expression.
         *)
	  val (grd, row1, rows, vMap) = (case CMat.toRows mat
		 of (ps, act)::rows => let
		      fun rewrite (0, PMat.Grd grd::rest, _) =
			    (grd, PMat.Pat(P_Con(Basis.trueCon, NONE)) :: rest, act)
			| rewrite (n, PMat.Pat(P_Var x)::rest, occ::occr) = let
			    val (grd, rest', act') = rewrite (n-1, rest, occr)
			    in
			      (grd, PMat.Pat P_Wild :: rest', bindVar(act', occ, x))
			    end
			| rewrite (n, pg::rest, _::occr) = let
			    val (grd, rest', act') = rewrite (n-1, rest, occr)
			    in
			      (grd, pg :: rest', act')
			    end
			| rewrite _ = raise Fail "impossible"
		      val (grd, ps', act') = rewrite (c, ps, occs)
		      in
			(grd, (ps', act'), rows, #1 act')
		      end
		  | _ => raise Fail "unexpected empty matrix"
		(* end case *))
	(* make code to evaluate the guard *)
	  fun mkGuardEval mat =
		actToExp (vMap, E_Let(flg, grd, comp(occs', mat)))
	  fun doRows ([], rows) = (* all guards after the first are trivial *)
		mkGuardEval(CMat.fromRows(List.rev rows))
	    | doRows ((ps, act)::rs, rows) = if isTrivGuard(List.nth(ps, c))
		then doRows (rs, (replace (ps, c, [PMat.Pat P_Wild]), act)::rows)
		else let (* need to split the matrix *)
		(* compile the continuation, which has a non-trivial guard
		 * in column `c` of its first row.
		 *)
		  val cont = comp(occs, CMat.fromRows((ps, act)::rs))
		(* construct the default row for the other half of the matrix *)
		  val defaultRow = let
			fun mkItem (PMat.Pat _) = PMat.Pat P_Wild
			  | mkItem (PMat.Grd _) = trivGuard
			val dfltItems = List.map mkItem (#1 row1)
			in
			  (dfltItems, (OMap.empty, cont))
			end
		  in
		    mkGuardEval (CMat.fromRows (List.rev (defaultRow::rows)))
		  end
	  in
	    doRows (rows, [row1])
	  end

    fun compile (x, cases) = let
	  fun mkRow (pat, exp) = (pat, (OMap.empty, exp))
	  in
	    comp ([Occ.var x], CMat.fromMatch (List.map mkRow cases))
	  end

    fun test arg = (
	  PPAST.prMatch arg;
	  print "\n  ==>\n\n";
	  PPAST.pr(compile arg))

  end
