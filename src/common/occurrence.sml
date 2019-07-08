(* occurrence.sml
 *
 * Variable occurrences (a la Pettersson)
 *)

structure Occurrence : sig

  (* A variable occurrence x.i1.i2 represents the i2'th component of the i1'th
   * component of x.  Indices start at one.
   *)
    datatype t = O_Var of Var.t | O_Path of t * int

    val toString : t -> string

  (* given `occ` and `n`, returns the list `[occ.1, ..., occ.n]` *)
    val tuple : t * int -> t list

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

    fun tuple (occ, n) = List.tabulate (n, fn i => O_Path(occ, i+1))

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
