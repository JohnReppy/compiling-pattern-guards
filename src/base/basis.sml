(* basis.sml
 *
 * A Basis of types
 *)

structure Basis : sig

  (* some basic types *)
    val unitTy : Type.t
    val intTy : Type.t
    val stringTy : Type.t

  (* booleans *)
    val boolTy : Type.t
    val falseCon : DataCon.t
    val trueCon : DataCon.t

  (* exceptions *)
    val exnTy : Type.t
    val matchExn : DataCon.t
    val failExn : DataCon.t
    val newExn : string * Type.t option -> DataCon.t

  (* generate a new monomorphic ref type *)
    val newRefTy : string * Type.t -> {
            ty : Type.t,
            refCon : DataCon.t
          }

  (* generate a new monomorphic list type *)
    val newListTy : string * Type.t -> {
            ty : Type.t,
            nilCon : DataCon.t,
            consCon : DataCon.t
          }

  end = struct

    structure Ty = Type
    structure DT = DataTy

  (* some basic types *)
    val unitTy = Type.T_Tuple[]
    val intTy = Type.T_Base "int"
    val stringTy = Type.T_Base "string"

  (* booleans *)
    local
      val boolDT = DT.new ("bool", [
              ("false",  NONE),
              ("true", NONE)
            ])
    in
    val boolTy = Ty.T_Data boolDT
    val [falseCon, trueCon] = DT.consOf boolDT
    end (* local *)

  (* exceptions *)
    local
      val exnDT = TypeReps.Data{name = "exn", mutable=false, span = ~1, cons = ref []}
    in
    val exnTy = Ty.T_Data exnDT
    fun newExn (name, argTy) = TypeReps.DCon(Atom.atom name, exnDT, argTy)
    val matchExn = newExn ("Match", NONE)
    val failExn = newExn ("Fail", SOME stringTy)
    end (* local *)

  (* generate a new monomorphic ref type *)
    fun newRefTy (tyName, argTy) = let
	  val (refDT, refDC) =  DT.newRef (tyName, argTy)
          in {
            ty = Type.T_Data refDT,
            refCon = refDC
          } end

  (* generate a new monomorphic list type *)
    fun newListTy (tyName, elemTy) = let
          val (listDT, [nilDC, consDC]) = DT.newWithCons (tyName, [
                  (tyName ^ "_nil", NONE),
                  (tyName ^ "_cons", SOME(fn ty => Ty.T_Tuple[elemTy, ty]))
                ])
          in {
            ty = Type.T_Data listDT,
            nilCon = nilDC,
            consCon = consDC
          } end

  end
