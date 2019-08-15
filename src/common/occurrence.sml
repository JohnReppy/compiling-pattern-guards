(* occurrence.sml
 *
 * Variable occurrences (a la Pettersson)
 *)

structure Occurrence : sig

  (* A variable occurrence x.i1.i2 represents the i2'th component of the i1'th
   * component of x.  Indices start at one.
   *)
    type t

    val toString : t -> string

  (* make a base occurrence from a variable *)
    val var : Var.t -> t

  (* select the i'th element of a tuple specified by an occurrence; note that indices
   * start at one!
   *)
    val select : t * int -> t

  (* given `occ` and `n`, returns the list `[occ.1, ..., occ.n]` *)
    val tuple : t * int -> t list

  (* return the root variable of an occurrence *)
    val varOf : t -> Var.t

  (* return the type of an occurrence *)
    val typeOf : t -> Type.t

  (* convert an occurrence to an expression *)
    val toExp : t -> AST.exp

    val same : t * t -> bool
    val compare : t * t -> order

    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t

  end = struct

    datatype t = O_Var of Var.t | O_Path of t * int

    fun toString occ = let
	  fun toS (O_Var x, l) = Var.toString x :: l
	    | toS (O_Path(occ, n), l) = toS(occ, "." :: Int.toString n :: l)
	  in
	    String.concat(toS (occ, []))
	  end

    fun var x = O_Var x

    fun select (occ, i) = O_Path(occ, i)

    fun tuple (occ, n) = List.tabulate (n, fn i => O_Path(occ, i+1))

    fun varOf (O_Var x) = x
      | varOf (O_Path(occ, _)) = varOf occ

    fun typeOf (O_Var x) = Var.typeOf x
      | typeOf (O_Path(occ, n)) = (case typeOf occ
	   of Type.T_Tuple tys => List.nth(tys, n-1)
	    | _ => raise Fail "expected tuple type"
	  (* end case *))

    fun toExp (O_Var x) = AST.E_Var x
      | toExp (O_Path(occ, n)) = AST.E_Select(n, toExp occ)

    fun same (O_Var x, O_Var y) = Var.same(x, y)
      | same (O_Path(occ1, n1), O_Path(occ2, n2)) = (n1 = n2) andalso same(occ1, occ2)
      | same _ = false

    fun compare (O_Var x, O_Var y) = Var.compare(x, y)
      | compare (O_Path(occ1, n1), O_Path(occ2, n2)) = ifEqualThen(occ1, occ2, Int.compare(n1, n2))
      | compare (O_Path(occ1, _), occ2 as O_Var _) = ifEqualThen(occ1, occ2, GREATER)
      | compare (occ1 as O_Var _, O_Path(occ2, _)) = ifEqualThen(occ1, occ2, LESS)

    and ifEqualThen (occ1, occ2, res) = (case compare(occ1, occ2)
	   of EQUAL => res
	    | order => order
	  (* end case *))

    structure Set = RedBlackSetFn (
      struct
        type ord_key = t
        val compare = compare
      end)
    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)

  end
