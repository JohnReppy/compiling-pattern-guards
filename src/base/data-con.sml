(* data-con.sml
 *
 * Datatype constructors
 *)

structure DataCon : sig

    type t

    val toString : t -> string

    val argTy : t -> Type.t option
    val isConstrFn : t -> bool
    val owner : t -> TypeReps.dataty

  (* is the owner datatype mutable? *)
    val isRef : t -> bool

    val same : t * t -> bool
    val compare : t * t -> order

  (* return the span (# of constructors) of the type that this constructor is
   * a member of.  Returns 0 for exceptions.
   *)
    val span : t -> int

    structure Set : ORD_SET where type Key.ord_key = t

  end = struct

    datatype t = datatype TypeReps.dcon

    fun toString (DCon(dc, _, _)) = Atom.toString dc

    fun same (DCon(dc1, _, _), DCon(dc2, _, _)) = Atom.same(dc1, dc2)
    fun compare (DCon(dc1, _, _), DCon(dc2, _, _)) = Atom.compare(dc1, dc2)

    fun argTy (DCon(_, _, ty)) = ty

    fun isConstrFn (DCon(_, _, NONE)) = false
      | isConstrFn _ = true

    fun owner (DCon(_, dt, _)) = dt

    fun isRef dc = DataTy.isRef(owner dc)

    fun span (DCon(_, dt, _)) = DataTy.span dt

    structure Set = RedBlackSetFn (
      struct
        type ord_key = t
        val compare = compare
      end)

  end
