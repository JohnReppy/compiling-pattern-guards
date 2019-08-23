(* pp-ast.sml
 *
 * Pretty printer for AST.
 *)

structure PPAST : sig

    val patToString : AST.pat -> string

    val output : TextIO.outstream * AST.exp -> unit

  (* print to stdOut *)
    val pr : AST.exp -> unit

  (* print a deconstructed match *)
    val prMatch : Var.t * (AST.pat * AST.exp) list -> unit

  end = struct

    structure DC = DataCon
    structure PP = TextIOPP

    datatype exp = datatype AST.exp
    datatype pat = datatype AST.pat

    fun expToString (e : exp) = let
	  fun toS (E_Let(x, e, e'), l) = "<let>" :: l
	    | toS (E_Fun((f, x, e), e'), l) = "<fun>" :: l
	    | toS (E_App(E_Var f, es), l) = Var.toString f :: tuple(es, l)
	    | toS (E_App(E_Con c, [e]), l) = apply (DC.toString c, e, l)
	    | toS (E_App _, l) = "<app>" :: l
	    | toS (E_If _, l) = "<if>" :: l
	    | toS (E_Case _, l) = "<case>" :: l
            | toS (E_Tuple es, l) = tuple(es, l)
	    | toS (E_Select(i, e), l) = apply("#"^Int.toString i, e, l)
	    | toS (E_Var x, l) = Var.name x :: l
	    | toS (E_Con dc, l) = DC.toString dc :: l
	    | toS (E_Raise(e, _), l) = "raise " :: toS(e, l)
	    | toS (E_Exp(s, _), l) = s :: l
	  and tuple ([], l) = "()" :: l
	    | tuple (e::es, l) =
                "(" :: toS (e, List.foldr (fn (e, l) => ", " :: toS(e, l)) (")" :: l) es)
	  and apply (f, arg as E_Tuple _, l) = f :: toS(arg, l)
	    | apply (f, e as E_App _, l) = f :: paren (e, l)
	    | apply (f, e as E_If _, l) = f :: paren (e, l)
	    | apply (f, e as E_Case _, l) = f :: paren (e, l)
	    | apply (f, e as E_Select _, l) = f :: paren (e, l)
	    | apply (f, e as E_Raise _, l) = f :: paren (e, l)
	    | apply (f, e, l) = f :: " " :: toS (e, l)
          and paren (exp, l) = "(" :: toS(exp, ")" :: l)
	  in
            String.concat(toS(e, []))
	  end

    fun patToString (p : pat) = let
          fun prec P_Wild = 4
            | prec (P_Var _) = 4
            | prec (P_Tuple _) = 4
            | prec (P_Con(_, NONE)) = 4
            | prec (P_Con(_, _)) = 3
            | prec (P_Or _) = 2
            | prec (P_If _) = 1
          fun toS (P_Wild, l) = "_" :: l
	    | toS (P_Var x, l) = Var.name x :: l
            | toS (P_Tuple[], l) = "()" :: l
            | toS (P_Tuple(p::ps), l) =
                "(" :: toS (p, List.foldr (fn (p, l) => ", " :: toS(p, l)) (")" :: l) ps)
            | toS (P_Con(dc, NONE), l) = DC.toString dc :: l
            | toS (P_Con(dc, SOME(p as P_Tuple _)), l) = DC.toString dc :: toS(p, l)
            | toS (P_Con(dc, SOME p), l) = if (prec p < 4)
                then DC.toString dc :: "(" :: toS(p, ")" :: l)
                else DC.toString dc :: " " :: toS(p, l)
            | toS (P_Or [], l) = raise Fail "bogus or-pattern"
            | toS (P_Or(p::ps), l) =
                paren (3, p, List.foldr (fn (p, l) => " | " :: toS(p, l)) l ps)
            | toS (P_If(p, e), l) = paren (2, p, " if " :: expToString e :: l)
          and paren (n, pat, l) = if (prec pat < n)
                then "(" :: toS(pat, ")" :: l)
                else toS(pat, l)
          in
            String.concat(toS(p, []))
          end

    fun atomic (E_Let _) = true
      | atomic (E_Fun _) = true
      | atomic (E_App _) = false
      | atomic (E_If _) = false
      | atomic (E_Case _) = false
      | atomic (E_Tuple _) = true
      | atomic (E_Select _) = false
      | atomic (E_Var _) = true
      | atomic (E_Con _) = true
      | atomic (E_Raise _) = false
      | atomic (E_Exp _) = true

    fun output (outS, code) = let
          val ppStrm = PP.openOut {dst = outS, wid = 120}
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
	  fun ppBlk blk = let
		fun pp (E_Fun((f, xs, e1), e2)) = (
		      PP.openHBox ppStrm;
			string "fun"; sp(); string(Var.name f);
			sp();
			case xs
			 of [] => string "()"
			  | x::xr => (
			      string "("; string(Var.toString x);
			      List.app (fn x => (string ","; sp(); string(Var.toString x))) xr;
			      string ")")
			(* end case *);
			sp(); string "="; sp();
			PP.openVBox ppStrm (PP.Abs 4);
		          ppCode e1;
			PP.closeBox ppStrm;
		      PP.closeBox ppStrm;
		      nl(); pp e2)
		  | pp (E_Let(x, e1, e2)) = (
		      PP.openHBox ppStrm;
			string "let"; sp(); string(Var.toString x); sp(); string "=";
			sp(); ppCode e1;
		      PP.closeBox ppStrm;
		      nl(); pp e2)
		  | pp e = (
		      PP.openVBox ppStrm (PP.Abs 2);
			string "in"; nl();
			ppCode e;
		      PP.closeBox ppStrm;
		      nl(); string "end")
		in
		  PP.openVBox ppStrm (PP.Abs 0);
		    pp blk;
		  PP.closeBox ppStrm
		end
          and ppCode e = (case e
               of E_Let _ => ppBlk e
                | E_Fun _ => ppBlk e
		| E_App(f, [arg]) => (
		    PP.openHBox ppStrm;
		      ppAtomic f;
		      sp();
		      ppAtomic arg;
		    PP.closeBox ppStrm)
		| E_App(e1, args) => (
		    PP.openHBox ppStrm;
		      ppAtomic e1;
		      case args
		       of [] => string "()"
			| e::es => (
			    string "("; ppCode e;
			    List.app (fn e => (string ","; sp(); ppCode e)) es;
			    string ")")
		      (* end case *);
		    PP.closeBox ppStrm)
                | E_If(cond, e1, e2) =>  (
                    PP.openBox ppStrm (PP.Abs 2);
                      PP.openHBox ppStrm;
                        string "if"; sp(); ppCode cond;
                      PP.closeBox ppStrm;
                      sp();
                      PP.openHBox ppStrm;
                        string "then"; sp(); ppCode e1;
                      PP.closeBox ppStrm;
                      sp();
                      PP.openHBox ppStrm;
                        string "else"; sp(); ppCode e2;
                      PP.closeBox ppStrm;
                    PP.closeBox ppStrm)
                | E_Case(e, cases) => (
                    PP.openVBox ppStrm (PP.Abs 0);
                      PP.openHBox ppStrm;
                        string "case"; sp(); ppCode e; sp(); string "of";
                      PP.closeBox ppStrm;
                      List.app ppCase cases;
                    PP.closeBox ppStrm)
                | E_Tuple[] => string "()"
                | E_Tuple(e::es) => (
                    PP.openHBox ppStrm;
                      string "("; ppCode e;
                      List.app (fn e => (string ","; sp(); ppCode e)) es;
                      string ")";
                    PP.closeBox ppStrm)
                | E_Var x => string(Var.toString x)
                | E_Con dc => string(DataCon.toString dc)
                | E_Select(i, e) =>  (
		    PP.openHBox ppStrm;
		      string("#" ^ Int.toString i);
		      sp();
		      ppAtomic e;
		    PP.closeBox ppStrm)
		| E_Raise(e, _) => (
		    PP.openHBox ppStrm;
		      string "raise"; sp(); ppAtomic e;
		    PP.closeBox ppStrm)
                | E_Exp(exp, _) => string exp
              (* end case *))
	  and ppAtomic (e : AST.exp) = if atomic e
		then ppCode e
		else (
		  PP.openHBox ppStrm;
		    string "("; ppCode e; string ")";
		  PP.closeBox ppStrm)
          and ppCase (pat, e) = (
                nl();
		PP.openVBox ppStrm (PP.Rel 4);
		  PP.openHBox ppStrm;
		    string "|"; sp(); string(patToString pat);
		    sp(); string "=>"; sp();
		    case e
		     of E_Case _ => ppAtomic e
		      | _ => ppCode e
		  (* end case *);
		  PP.closeBox ppStrm;
		PP.closeBox ppStrm)
          in
            PP.openVBox ppStrm (PP.Abs 0);
              ppCode code; nl();
            PP.closeBox ppStrm;
            PP.closeStream ppStrm
          end

    fun pr e = output(TextIO.stdOut, e)

    fun prMatch (x, cases) = pr(AST.E_Case(E_Var x, cases))

  end
