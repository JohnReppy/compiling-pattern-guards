(* var.sml
 *
 * Variables.
 *)

structure Var : sig

    type t

  (* new named variable *)
    val new : string * Type.t -> t
  (* new temporary variable *)
    val newTmp : Type.t -> t
  (* new function variable *)
    val newFn : Type.t * Type.t -> t

    val typeOf : t -> Type.t

    val name : t -> string		(* variable name *)
    val toString : t -> string		(* variable name + unique ID *)

    val compare : t * t -> order
    val same : t * t -> bool

  (* finite sets and maps with variable keys *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t

  end = struct

    datatype t = V of {
	name : string,
	ty : Type.t,
	id : word
      }

    local
      val cnt = ref 0w0
    in
    fun new (name, ty) = let
	  val n = !cnt
	  in
	    cnt := n + 0w1;
	    V{name = name, ty = ty, id = n}
	  end
    fun newTmp ty = new ("", ty)
    fun newFn (ty1, ty2) = new ("", TypeReps.T_Fun(ty1, ty2))
    end

    fun typeOf (V{ty, ...}) = ty

    fun name (V{name="", id, ty, ...}) = (case ty
	   of TypeReps.T_Fun _ => "_k_" ^ Word.toString id
	    | _ => "_t_" ^ Word.toString id
	  (* end case *))
      | name (V{name, ...}) = name

    fun toString (x as V{name="", ...}) = name x
      | toString (V{name, id, ...}) = concat[name, "_", Word.toString id]

    fun same (V{id=id1, ...}, V{id=id2, ...}) = (id1 = id2)

    fun compare (V{id=id1, ...}, V{id=id2, ...}) = Word.compare(id1, id2)

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
