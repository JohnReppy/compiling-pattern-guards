(* pat-matrix.sml
 *
 * Representation of pattern matrices that includes support for guard columns.
 *)

structure PatMatrix : sig

  (* representation of a pattern matrix. *)
    type t

    datatype ('pat, 'grd) item = Pat of 'pat | Grd of 'grd

  (* the "kind" of a column is represented as a `unit item` *)
    type kind = (unit, unit) item

  (* a row in the matrix *)
    type row = (AST.pat, AST.exp) item list

  (* the kinds of a row *)
    val rowSig : row -> kind list

  (* the kinds of the columns of a matrix *)
    val matSig : t -> kind list

  (* build a pattern matrix from a list of rows *)
    val fromRows : row list -> t

  (* return the list of rows that forms the pattern matrix *)
    val toRows : t -> row list

  (* return the column kinds and list of rows that forms the pattern matrix *)
    val toSigAndRows : t -> kind list * row list

  (* return the size of a matrix; npats is the number of pattern-columns, while
   * ncols is the number of all columns
   *)
    val size : t -> {npats : int, ncols : int, nrows : int}

  (* extract the i'th column of a matrix (0-based) *)
    val col : t -> int -> (AST.pat list, AST.exp list) item

  (* extract the i'th row of a matrix (0-based) *)
    val row : t -> int -> row

  (***** OPERATIONS ON ROWS *****)

  (* is the specified column a pattern column (i.e., not a guard column) *)
    val isPatCol : t * int -> bool

  (* `foldRows f init mat` folds the function `f` over the rows of the matrix from
   * bottom to top.
   *)
    val foldRows  : (row * 'a -> 'a) -> 'a -> t -> 'a
    val foldRowsi : (int * row * 'a -> 'a) -> 'a -> t -> 'a

  (* `allRows f mat` returns true if the predicate `f` is true for all the rows
   * in the matrix.
   *)
    val allRows : (row -> bool) -> t -> bool

  (* `existsRows f mat` returns true if the predicate `f` is true for one or more
   * of the rows in the matrix.
   *)
    val existsRows : (row -> bool) -> t -> bool

  (* return the index of the first row that contains an item that satisfies the
   * predicate, or else NONE
   *)
    val findRowIndex : ((AST.pat, AST.exp) item -> bool) -> t -> int option

  (***** OPERATIONS ON COLUMNS *****)

  (* `filterRowsByCol pred (mat, c)` returns a list of rows from the pattern matrix `mat`
   * such that the pattern in column `c` satisfies the predicate `pred`.
   *)
    val filterRowsByCol : (AST.pat -> bool) -> t * int -> row list

  (* `allCol f (mat, c)` returns true if all items in column `c` of `mat` satisfy
   * the predicate `f`.
   *)
    val allCol : ((AST.pat, AST.exp) item -> bool) -> t * int -> bool

  (* `existsCol f (mat, c)` returns true if there is an item in column `c` of `mat`
   * that satisfies the predicate `f`.
   *)
    val existsCol : ((AST.pat, AST.exp) item -> bool) -> t * int -> bool

  (* `foldCol f init (mat, c)` folds the function f over column `c` of `mat`. *)
    val foldCol : ((AST.pat, AST.exp) item * 'a -> 'a) -> 'a -> t * int ->  'a

  (* `removeCol (mat, c)` removes column `c` from the matrix *)
    val removeCol : t * int -> t

  (* `expandCol (mat, c, xf)` replaces each item in column `c` by the result of
   * applying `xf` to the item.
   *)
    val expandCol : t * int * ((AST.pat, AST.exp) item -> (AST.pat, AST.exp) item list) -> t

  (***** MATRIX OPERATIONS *****)

  (* `exists pred mat` returns true if there is an element `pg` in `mat` such that
   * `pred pg` is true.
   *)
    val exists : ((AST.pat, AST.exp) item -> bool) -> t -> bool

  (* `splitAtRow (mat, r)` splits the matrix `mat` into two matrices, the first with
   * rows 0..`r-1` and the second with rows `r`..n, where `mat` has n rows.
   * Note that if either matrix is empty (has no rows), then Subscript is raised.
   *)
    val splitAtRow : t * int -> t * t

  (* append a row on the end of the matrix *)
    val appendRow : t * row -> t

  (* concatenate the rows of two matrices; raises Size if the matrices have different
   * numbers of rows.
   *)
    val beside : t * t -> t

  (* concatenate the columns of two matrices; raises Size if the matrices have different
   * numbers of columns.
   *)
    val above : t * t -> t

  (* pretty print a matrix *)
    val pp : TextIOPP.stream * t -> unit

  (* print a row of a matrix to stdOut *)
    val printRow : row -> unit

  (* print a matrix to stdOut *)
    val print : t -> unit

  end = struct

    structure V = Vector
    structure VS = VectorSlice

    datatype ('pat, 'grd) item = Pat of 'pat | Grd of 'grd

  (* the "kind" of a column is represented as a `unit item` *)
    type kind = (unit, unit) item

    fun sameKind (Pat _, Pat _) = true
      | sameKind (Grd _, Grd _) = true
      | sameKind _ = false

    datatype t = PMat of {
	npats : int,
        ncols : int,
        pats : (AST.pat, AST.exp) item vector vector
      }

    type row = (AST.pat, AST.exp) item list

  (* the kinds of a row *)
    fun rowSig row = List.map (fn (Pat _) => Pat() | (Grd _) => Grd()) row

  (* the kinds of the columns of a matrix *)
    fun matSig (PMat{pats, ...}) = rowSig (V.toList(V.sub(pats, 0)))

  (* compute the number of pattern items in a column *)
    fun numPats patv = V.foldl (fn (Pat _, n) => n+1 | (Grd _, n) => n) 0 patv

    fun fromRows [] = raise Size
      | fromRows (rows as (ps1::_)) = let
          val patvs = V.fromList (List.map V.fromList rows)
	  val (npats, ncols) = let
		fun cnt (Pat _, (np, nc)) = (np+1, nc+1)
		  | cnt (Grd _, (np, nc)) = (np, nc+1)
		in
		  List.foldl cnt (0, 0) ps1
		end
          in
            PMat{npats = npats, ncols = ncols, pats = patvs}
          end

    fun toRows (PMat{pats, ...}) =
	  V.foldr (fn (pgs, rows) => (V.toList pgs) :: rows) [] pats

    fun toSigAndRows mat = let
	  val rows as (r1::_) = toRows mat
	  in
	    (rowSig r1, rows)
	  end

    fun size (PMat{npats, ncols, pats, ...}) =
	  {npats = npats, ncols = ncols, nrows = V.length pats}

    fun colIsPat (pats, c) = (case V.sub(V.sub(pats, 0), c)
	   of Pat _ => true
	    | Grd _ => false
	  (* end case *))

    fun col (PMat{ncols, pats, ...}) c =
          if (ncols <= c) orelse (V.length pats = 0)
            then raise Subscript
	  else if colIsPat(pats, c)
	    then let
	      fun get (v, ps) = (case V.sub(v, c)
		     of Pat p => p :: ps
		      | Grd _ => raise Fail "unexpected guard in pattern column"
		    (* end case *))
	      in
		Pat(V.foldr get [] pats)
	      end
	    else let
	      fun get (v, gs) = (case V.sub(v, c)
		     of Pat _ => raise Fail "unexpected pattern in guard column"
		      | Grd g => g :: gs
		    (* end case *))
	      in
		Grd(V.foldr get [] pats)
	      end

    fun row (PMat{pats, ...}) r = V.toList(V.sub(pats, r))

    fun isPatCol (PMat{pats, ...}, c) = colIsPat(pats, c)

    fun foldRows f init (PMat{pats, ...}) =
	  V.foldr (fn (pv, acc) => f (V.toList pv, acc)) init pats

    fun foldRowsi f init (PMat{pats, ...}) =
	  V.foldri (fn (i, pv, acc) => f (i, V.toList pv, acc)) init pats

  (* `allRows f mat` returns true if the predicate `f` is true for all the rows
   * in the matrix.
   *)
    fun allRows pred (PMat{pats, ...}) = let
          val n = V.length pats
	  fun lp i = (i = n)
                orelse (pred (V.toList (V.sub(pats, i))) andalso lp (i+1))
          in
            lp 0
          end

  (* `existsRows f mat` returns true if the predicate `f` is true for one or more
   * of the rows in the matrix.
   *)
    fun existsRows pred (PMat{pats, ...}) = let
          val n = V.length pats
	  fun lp i = (i < n)
                andalso (pred (V.toList (V.sub(pats, i))) orelse lp (i+1))
          in
            lp 0
          end

    fun findRowIndex pred (mat as PMat{pats, ...}) = (
	  case V.findi (fn (_, pv) => V.exists pred pv) pats
	   of SOME(i, _) => SOME i
	    | NONE => NONE
	  (* end case *))

  (* `filterRowsByCol pred mat col` returns a list of rows from the pattern matrix `mat`
   * such that the pattern in column `col` satisfies the predicate `pred`.
   *)
    fun filterRowsByCol pred (mat as PMat{ncols, pats, ...}, c) =
          if (c < 0) orelse (ncols <= c)
            then raise Subscript
            else let
              fun doRow (pv, rows) = (case V.sub(pv, c)
		     of Pat p => if pred p
			then V.toList pv :: rows
			else rows
		      | Grd _ => raise Fail "unexpected guard"
		    (* end case *))
              in
                V.foldr doRow [] pats
              end

    fun allCol pred (PMat{pats, ...}, c) =
	  V.all (fn row => pred(V.sub(row, c))) pats

    fun existsCol pred (PMat{pats, ...}, c) =
	  V.exists (fn row => pred(V.sub(row, c))) pats

    fun foldCol f init (PMat{pats, ...}, c) =
	  V.foldl (fn (row, acc) => f(V.sub(row, c), acc)) init pats

  (* remove the n'th element from a vector *)
    fun removeNth (v, 0) = VS.vector(VS.slice(v, 1, NONE))
      | removeNth (v, i) = let
	  val n = V.length v - 1
	  in
	    if (i = n)
	      then VS.vector(VS.slice(v, 0, SOME n))
	      else VS.concat[VS.slice(v, 0, SOME n), VS.slice(v, n+1, NONE)]
	  end

    fun removeCol (PMat{ncols=1, ...}, 0) = PMat{ncols = 0, npats = 0, pats = V.fromList[]}
      | removeCol (PMat{ncols, npats, pats}, c) = let
          val ncols' = ncols - 1
	  val pats' = V.map (fn v => removeNth(v, c)) pats
	  val npats' = (case V.sub(V.sub(pats, 0), c)
		 of Pat _ => npats-1
		  | Grd _ => npats
		(* end case *))
	  in
	    PMat{ncols = ncols', npats = npats', pats = pats'}
	  end

    fun expandCol (PMat{ncols, npats, pats}, c, expandItem) = let
	  fun expandRow row = let
		fun exp (_, []) = raise Subscript
		  | exp (0, item::itemr) = expandItem item @ itemr
		  | exp (i, item::itemr) = item :: exp (i-1, itemr)
		in
		  V.fromList (exp (c, V.toList row))
		end
	  val pats' = V.map expandRow pats
	  val ncols' = V.length(V.sub(pats', 0))
	  val npats' = numPats (V.sub(pats', 0))
	(* check that all the rows are still the same size *)
	  val _ = if V.exists (fn v => V.length v <> ncols') pats'
		then raise Size
		else ()
	  in
	    PMat{ncols=ncols', npats=npats', pats=pats'}
	  end

    fun exists pred (PMat{pats, ...}) =  V.exists (V.exists pred) pats

    fun splitAtRow (PMat{ncols, npats, pats}, r) = let
          val pats1 = VS.vector(VS.slice(pats, 0, SOME r))
          val pats2 = VS.vector(VS.slice(pats, r, NONE))
          val mat1 = PMat{ncols = ncols, npats = npats, pats = pats1}
          val mat2 = PMat{ncols = ncols, npats = npats, pats = pats2}
          in
            (mat1, mat2)
          end

    fun appendRow (PMat{ncols, npats, pats}, ps) = PMat{
	    ncols = ncols, npats = npats,
	    pats = V.append(pats, V.fromList ps)
	  }

  (* concatenate the rows of two matrices; raises Size if the matrices have different
   * numbers of rows.
   *)
    fun beside (PMat{ncols=n1, npats=np1, pats=rows1}, PMat{ncols=n2, npats=np2, pats=rows2}) =
	  if (V.length rows1 <> V.length rows2)
	    then raise Size
	    else PMat{
		ncols = n1 + n2,
		npats = np1 + np2,
		pats = V.mapi (fn (i, ps1) => V.concat[ps1, V.sub(rows2, i)]) rows1
	      }

  (* concatenate the columns of two matrices; raises Size if the matrices have different
   * numbers of columns.
   *)
    fun above (PMat{ncols=n1, npats=np1, pats=rows1}, PMat{ncols=n2, npats=np2, pats=rows2}) =
	  if (n1 = n2) andalso (np1 = np2)
	  andalso ListPair.allEq sameKind (V.toList(V.sub(rows1, 0)), V.toList(V.sub(rows2, 0)))
	    then PMat{ncols = n1, npats = np1, pats = V.concat[rows1, rows2]}
	    else raise Size

  (* print a row of a matrix to stdOut *)
    fun printRow pats = let
	  fun toS (Pat p) = PatUtil.toString p
	    | toS (Grd e) = concat["[", ExpUtil.toString e, "]"]
	  in
	    print "|";
	    List.app (fn pg => (print " "; print(toS pg))) pats;
	    print " |\n"
	  end

    structure PP = TextIOPP

  (* pretty print a matrix *)
    fun pp (ppS, PMat{ncols, pats, ...}) = let
          val str = PP.string ppS
	  val wids = Array.array(ncols, 0)
	(* convert the patterns in a row to strings while keeping track of the
	 * maximum width of each column in the matrix.
	 *)
	  fun doRow (i, pv) = let
		fun doPat (c, pg, ps) = let
		      val s = (case pg
			     of Pat p => PatUtil.toString p
			      | Grd e => concat["[", ExpUtil.toString e, "]"]
			    (* end case *))
		      in
			Array.update(wids, c, Int.max(Array.sub(wids, c), String.size s));
			s :: ps
		      end
		in
		  V.foldri doPat [] pv
		end
	(* convert the patterns in the matrix to strings *)
	  val pats = V.mapi doRow pats
	(* pretty print a row *)
	  fun ppRow rowPats = let
		fun ppPat (i, s) = (
		      if (i > 0) then PP.space ppS 2 else ();
		      str (StringCvt.padRight #" " (Array.sub(wids, i)) s))
		in
		  PP.openHBox ppS;
		    str "|"; PP.space ppS 1;
		    List.appi ppPat rowPats;
		    PP.space ppS 1; str "|";
		  PP.closeBox ppS;
		  PP.newline ppS
		end
	  in
	    PP.openVBox ppS (PP.Rel 0);
	      V.app ppRow pats;
	    PP.closeBox ppS
	  end

  (* print a matrix to stdOut *)
    fun print mat = let
          val ppS = PP.openOut {dst = TextIO.stdOut, wid = 120}
	  in
	    pp (ppS, mat);
	    PP.closeStream ppS
          end

  end
