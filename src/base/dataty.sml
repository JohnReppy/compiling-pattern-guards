(* dataty.sml
 *
 * Datatypes
 *)

structure DataTy : sig

    type t

    val toString : t -> string

    val same : t * t -> bool

  (* is the datatype mutable? *)
    val isRef : t -> bool

  (* the number of constructors; note that a negative value means that the
   * type has an unbounded number of constructors (e.g., the exn type).
   *)
    val span : t -> int

  (* the constructors of this type; note that this list will be empty when
   * the span is negative.
   *)
    val consOf : t -> TypeReps.dcon list

  (* create a datatype from a type name and list of constructors specifications,
   * where a constructor is specified by its name and a function for generating
   * its argument type given the owning datatype as an argument.
   *)
    val new : string * (string * (TypeReps.ty -> TypeReps.ty) option) list -> t

    val newWithCons : string * (string * (TypeReps.ty -> TypeReps.ty) option) list
	  -> t * TypeReps.dcon list

    val newRef : string * TypeReps.ty -> t * TypeReps.dcon

  end = struct

    datatype t = datatype TypeReps.dataty

    fun toString (Data{name, ...}) = name

    fun same (Data{name=a, ...}, Data{name=b, ...}) = (a = b)

    fun isRef (Data{mutable, ...}) = mutable

    fun span (Data{span, ...}) = span

    fun consOf (Data{span, cons, ...}) = if (span < 0) then [] else !cons

    fun new (name, conSpcs) = let
	  val cons = ref []
	  val dt = Data{name = name, mutable = false, span = List.length conSpcs, cons = cons}
	  val ty = TypeReps.T_Data dt
	  fun mkCons (name, NONE) = TypeReps.DCon(Atom.atom name, dt, NONE)
	    | mkCons (name, SOME argTy) = TypeReps.DCon(Atom.atom name, dt, SOME(argTy ty))
	  in
	    cons := List.map mkCons conSpcs;
	    dt
	  end

    fun newWithCons arg = let
	  val dt = new arg
	  in
	    (dt, consOf dt)
	  end

    fun newRef (name, argTy) = let
	  val cons = ref []
	  val dt = Data{name = name, mutable = true, span = 1, cons = cons}
	  val dc = TypeReps.DCon(Atom.atom(name ^ "_ref"), dt, SOME argTy)
	  in
	    cons := [dc];
	    (dt, dc)
	  end

  end
