(* pp-ast.sml
 *
 * Pretty printer for AST.
 *)

structure PPAST : sig

    val patToString : AST.pat -> string

    val output : TextIO.outstream * AST.exp -> unit

  (* print to stdOut *)
    val print : AST.exp -> unit

  end = struct

    structure DC = DataCon
    structure PP = TextIOPP

    datatype exp = datatype AST.exp
    datatype pat = datatype AST.pat

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
                "(" :: toS (p, List.foldr (fn (p, l) => "," :: toS(p, l)) (")" :: l) ps)
            | toS (P_Con(dc, NONE), l) = DC.toString dc :: l
            | toS (P_Con(dc, SOME p), l) = if (prec p < 4)
                then DC.toString dc :: "(" :: toS(p, ")" :: l)
                else DC.toString dc :: " " :: toS(p, l)
            | toS (P_Or [], l) = raise Fail "bogus or-pattern"
            | toS (P_Or(p::ps), l) =
                paren (3, p, List.foldr (fn (p, l) => " | " :: toS(p, l)) l ps)
            | toS (P_If(p, e), l) = paren (2, p, " if <exp>" :: l)
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
		fun pp (E_Fun((f, x, e1), e2)) = (
		      PP.openHBox ppStrm;
			string "fun"; sp(); string(Var.name f);
			sp(); string(Var.toString x);
			sp(); string "=";
			sp(); ppCode e1;
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
		| E_App(e1 as E_App _, e2) => (
		    PP.openHBox ppStrm;
		      ppCode e1; sp(); ppAtomic e2;
		    PP.closeBox ppStrm)
		| E_App(e1, e2) => (
		    PP.openHBox ppStrm;
		      ppAtomic e1; sp(); ppAtomic e2;
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
                | E_Var x => string(Var.name x)
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
	  and ppAtomic e = if atomic e
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

    fun print e = output(TextIO.stdOut, e)

  end
