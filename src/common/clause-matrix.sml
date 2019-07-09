(* clause-matrix.sml
 *
 * Representation of clause matrices.
 *)

structure ClauseMatrix : sig

  (* A clause matrix is `P -> A`, where `P` is a m x n pattern matrix and `A`
   * is a column vector of n actions.
   *)
    type 'act t

  (* a row in the matrix *)
    type 'act row = (PatMatrix.row * 'act)

  (* build a clause matrix from a list of cases *)
    val fromMatch : (AST.pat * 'act) list -> 'act t

  (* build a clause matrix from a list of rows *)
    val fromRows : 'act row list -> 'act t

  (* return the list of rows that forms the clause matrix *)
    val toRows : 'act t -> 'act row list

  (* return the size of a clause matrix *)
    val size : 'act t -> {ncols : int, npats : int, nrows : int}

  (* extract the i'th column of a matrix (0-based) *)
    val col : 'act t -> int -> (AST.pat list, AST.exp list) PatMatrix.item

  (* extract the i'th row of a matrix (0-based) *)
    val row : 'act t -> int -> 'act row

  (* extract the action from the i'th row of a matrix (0-based) *)
    val act : 'act t -> int -> 'act

  (* `mapRows f mat` applys the function f to the rows of `mat` and builds a new
   * matrix from the result.
   *)
    val mapRows : (PatMatrix.row * 'a -> PatMatrix.row * 'b) -> 'a t -> 'b t

  (* `foldRows f init mat` folds the function `f` over the rows of the matrix from
   * bottom to top.
   *)
    val foldRows : (PatMatrix.row * 'act * 'a -> 'a) -> 'a -> 'act t -> 'a

  (* `filterRowsByCol pred (mat, c)` returns a list of rows from the clause matrix `mat`
   * such that the pattern in column `c` satisfies the predicate `pred`.
   *)
    val filterRowsByCol : (AST.pat -> bool) -> 'act t * int -> 'act row list

  (* `removeCol (mat, c)` removes column `c` from the matrix *)
    val removeCol : 'act t * int -> 'act t

  (* `splitAtRow (mat, r)` splits the matrix `mat` into two matrices, the first with
   * rows 0..`r-1` and the second with rows `r`..n, where `mat` has n rows.
   * Note that if either matrix is empty (has no rows), then Subscript is raised.
   *)
    val splitAtRow : 'act t * int -> 'act t * 'act t

  (* pretty print a matrix *)
    val pp : TextIOPP.stream * 'act t -> unit

  (* print a matrix to stdOut *)
    val print : 'act t -> unit

  end = struct

    structure PM = PatMatrix
    structure V = Vector
    structure VS = VectorSlice

    datatype 'act t = CMat of {
	pats : PM.t,
        acts : 'act vector
      }

    type 'act row = PM.row * 'act

    fun fromMatch rows = let
          val (pats, acts) = ListPair.unzip rows
          val pats = PM.fromRows (List.map (fn p => [PM.Pat p]) pats)
          val acts = V.fromList acts
          in
            CMat{pats = pats, acts = acts}
          end

    fun fromRows [] = raise Size
      | fromRows rows = let
          val (patss, acts) = ListPair.unzip rows
          val pats = PM.fromRows patss
          val acts = V.fromList acts
          in
            CMat{pats = pats, acts = acts}
          end

    fun toRows (CMat{pats, acts}) =
	  PM.foldRowsi (fn (i, row, rows) => (row, V.sub(acts, i)) :: rows) [] pats

    fun size (CMat{pats, ...}) = PM.size pats

    fun col (CMat{pats, ...}) c = PM.col pats c

    fun row (CMat{pats, acts, ...}) r = (PM.row pats r, V.sub(acts, r))

    fun act (CMat{acts, ...}) r = V.sub(acts, r)

    fun mapRows f (CMat{pats, acts}) = let
          fun doRow (i, row, (rows', acts')) = let
		val (r, a) = f (row, V.sub(acts, i))
		in
		  (r::rows', a::acts')
		end
	  val (rows', acts') = PM.foldRowsi doRow ([], []) pats
          val pats = PM.fromRows rows'
          val acts = V.fromList acts'
          in
            CMat{pats = pats, acts = acts}
          end

    fun foldRows f init (CMat{pats, acts}) = let
          fun doRow (i, row, acc) = f (row, V.sub(acts, i), acc)
          in
            PM.foldRowsi doRow init pats
          end

  (* `filterRowsByCol pred mat col` returns a list of rows from the pattern matrix `mat`
   * such that the pattern in column `col` satisfies the predicate `pred`.
   *)
    fun filterRowsByCol pred (mat as CMat{pats, acts}, c) = let
	  fun doRow (i, row, rows) = (case List.nth(row, c)
		 of PM.Pat p => if pred p
		    then (row, V.sub(acts, i)) :: rows
		    else rows
		  | PM.Grd _ => raise Fail "unexpected guard"
		(* end case *))
	  in
	    PM.foldRowsi doRow [] pats
	  end

    fun removeCol (CMat{pats, acts}, c) =
	  CMat{pats = PM.removeCol (pats, c), acts = acts}

    fun splitAtRow (CMat{pats, acts}, r) = let
	  val (pats1, pats2) = PM.splitAtRow (pats, r)
          val acts1 = VS.vector(VS.slice(acts, 0, SOME r))
          val acts2 = VS.vector(VS.slice(acts, r, NONE))
          val mat1 = CMat{pats = pats1, acts = acts1}
          val mat2 = CMat{pats = pats2, acts = acts2}
          in
            (mat1, mat2)
          end

    structure PP = TextIOPP

  (* pretty print a matrix *)
(* TODO: add actions *)
    fun pp (ppS, CMat{pats, ...}) = PM.pp (ppS, pats)

  (* print a matrix to stdOut *)
    fun print mat = let
          val ppS = PP.openOut {dst = TextIO.stdOut, wid = 120}
	  in
	    pp (ppS, mat);
	    PP.closeStream ppS
          end

  end
