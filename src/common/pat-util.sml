(* pat-util.sml
 *
 * Utility functions for patterns.
 *)

structure PatUtil : sig

    val isVar : AST.pat -> bool
    val isCon : AST.pat -> bool
    val isOr : AST.pat -> bool

  (* does the pattern contain a guard? *)
    val hasGuard : AST.pat -> bool

  (* return the set of variables that are bound in a pattern *)
    val varsOf : AST.pat -> Var.t list

  (* do two patterns overlap? *)
    val compatible : AST.pat * AST.pat -> bool

  (* least-upper-bound of two patterns; returns NONE if they are not compatible *)
    val lub : AST.pat * AST.pat -> AST.pat option

    val toString : AST.pat -> string

  end = struct

    structure DC = DataCon

    datatype pat = datatype AST.pat

    fun isVar P_Wild = true
      | isVar (P_Var _) = true
      | isVar _ = false

    fun isCon (P_Con _) = true
      | isCon _ = false

    fun isOr (P_Or _) = true
      | isOr _ = false

  (* does a pattern contain a guard? *)
    fun hasGuard (P_Tuple pats) = List.exists hasGuard pats
      | hasGuard (P_Con(_, SOME p)) = hasGuard p
      | hasGuard (P_Or ps) = List.exists hasGuard ps
      | hasGuard (P_If _) = true
      | hasGuard _ = false

  (* return the set of variables that are bound in a pattern *)
    fun varsOf p = let
	  fun vars (P_Wild, vs) = vs
	    | vars (P_Var x, vs) = Var.Set.add(vs, x)
	    | vars (P_Tuple ps, vs) = List.foldl vars vs ps
	    | vars (P_Con(_, SOME p), vs) = vars (p, vs)
	    | vars (P_Con(_, NONE), vs) = vs
	    | vars (P_Or(p::_), vs) = vars (p, vs)
	    | vars (P_Or[], vs) = vs
	    | vars (P_If(p, _), vs) = vars (p, vs)
	  in
	    Var.Set.toList (vars (p, Var.Set.empty))
	  end

  (* are two patterns compatible (i.e., does there exist a value that they both match)? *)
    fun compatible (P_Wild, _) = true
      | compatible (_, P_Wild) = true
      | compatible (P_Var _, _) = true
      | compatible (_, P_Var _) = true
      | compatible (P_Tuple pats1, P_Tuple pats2) = ListPair.allEq compatible (pats1, pats2)
      | compatible (P_Con(c1, NONE), P_Con(c2, NONE)) = DC.same(c1, c2)
      | compatible (P_Con(c1, SOME p1), P_Con(c2, SOME p2)) =
          DC.same(c1, c2) andalso compatible (p1, p2)
      | compatible (P_Or ps1, p2) =
          List.exists (fn p => compatible(p, p2)) ps1
      | compatible (p1, P_Or ps2) =
          List.exists (fn p => compatible(p1, p)) ps2
      | compatible (P_If(p1, _), p2) = compatible(p1, p2)
      | compatible (p1, P_If(p2, _)) = compatible(p1, p2)
      | compatible _ = false

  (* compute the least-upper-bound of two patterns (or return NONE if they are not compatible) *)
    fun lub (p1, p2) = let
	  fun erase (P_Var _) = SOME P_Wild
	    | erase p = SOME p
	  in
	    case (p1, p2)
	     of (P_Wild, p2) => erase p2
	      | (p1, P_Wild) => erase p1
	      | (P_Var _, p2) => erase p2
	      | (p1, P_Var _) => erase p1
	      | (P_Tuple pats1, P_Tuple pats2) => let
		  fun lub' ([], [], pats) = SOME(P_Tuple(List.rev pats))
		    | lub' (p1::pr1, p2::pr2, pats) = (case lub(p1, p2)
			 of SOME p => lub' (pr1, pr2, p::pats)
			  | NONE => NONE
			(* end case *))
		    | lub' _ = NONE
		  in
		    lub' (pats1, pats2, [])
		  end
	      | (p as P_Con(dc1, NONE), P_Con(dc2, NONE)) =>
		  if DC.same(dc1, dc2) then SOME p else NONE
	      | (P_Con(dc1, SOME p1), P_Con(dc2, SOME p2)) =>
		  if DC.same(dc1, dc2)
		    then (case lub(p1, p2) of SOME p => SOME(P_Con(dc1, SOME p)) | _ => NONE)
		    else NONE
	      | (P_Or pats1, P_Or pats2) => let
		  fun accum (p1, p2, pats) = (case lub(p1, p2)
			 of SOME p => p::pats
			  | NONE => pats
			(* end case *))
		  val pats = List.foldl
			(fn (p1, pats) => List.foldl (fn (p2, ps) => accum(p1, p2, ps)) pats pats2)
			  [] pats1
		  in
		    case pats
		     of [] => NONE
		      | [p] => SOME p
		      | _ => SOME(P_Or(List.rev pats))
		    (* end case *)
		  end
	      | (p1 as P_Or _, p2) => lub (p1, P_Or[p2])
	      | (p1, p2 as P_Or _) => lub (P_Or[p1], p2)
(* QUESTION: what to do about P_If? *)
	      | _ => NONE
	    (* end case *)
	  end

    val toString = PPAST.patToString

  end
