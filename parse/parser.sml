
structure Parser :>
  sig
    val parse_repl : string -> Syntax.directive
    val parse_prog : string -> Syntax.directive
  end =
  struct
    open Syntax

    exception Error

    fun identity x = x
    fun lift x () = x

    fun null () = []
    fun single x = [x]
    val cons = op ::

    structure Arg =
      struct
        type string = string
        datatype terminal = datatype Token.token

        open Token

        type int = int
        type char = char
        type bool = bool
        type ty = ty

        type typeseq = typee list
        type typee = Syntax.typee

        val typeseq_sing = single
        val typeseq_cons = cons

        fun type_lit tylit =
          case tylit of
            Token.INT => Tint
          | Token.STR => Tstring
          | Token.CHAR => Tchar
          | Token.BOOL => Tbool
          | Token.VOID => Tunit

        fun type_prod tseq = Tprod tseq
        val type_id = identity
        val type_constr = Ttycon
        val type_arrow = Tarrow
        val type_tyvar = Ttyvar

        type argseq = (typee * string) list

        val argseq_nil = null
        val argseq_sing = single
        fun argseq_cons (ty, id, argseq) =
          (ty, id) :: argseq

        type dec = dec
        type decseq = dec list

        fun dec_basic (ty, id, exp) =
          Dval (ty, id, exp)
        fun dec_fun (ty, id, args, decs, ret_exp) =
          Dfun (ty, id, args, Elet (decs, ret_exp))

        val decseq_nil = null
        val decseq_cons = cons

        type casee = pat * exp
        type caseseq = casee list

        val casee_mk = identity
        val caseseq_sing = single
        val caseseq_cons = cons

        type pat = pat
        type patseq = pat list

        val patseq_sing = single
        val patseq_cons = cons

        val pat_id = identity
        val pat_wild = lift Pwild
        fun pat_ident id = Pident id
        val pat_numlit = Pnum
        val pat_strlit = Pstring
        val pat_charlit = Pchar
        val pat_boollit = Pbool
        val pat_tuple = Pprod
        val pat_app = Papp
        val pat_nil = lift Pnil
        val pat_cons = Pcons

        type exp = exp
        type expseq = exp list

        fun expseq_mk (e1, e2) = [e1, e2]
        val expseq_cons = cons

        val exp_id = identity
        val exp_tuple = Etuple
        val exp_numlit = Enum
        val exp_strlit = Estring
        val exp_charlit = Echar
        val exp_boollit = Ebool
        val exp_ident = Eident
        val exp_nil = lift Enil

        val exp_app = Eapp

        fun mk_binop opp (e1 : exp, e2 : exp) = Ebinop (e1, opp, e2)
        val exp_times = mk_binop Mul
        val exp_plus = mk_binop Add
        val exp_minus = mk_binop Sub
        val exp_divide = mk_binop Div
        val exp_neg = Eneg
        val exp_cons = mk_binop Cons
        val exp_lt = mk_binop Lt
        val exp_gt = mk_binop Gt
        val exp_lte = mk_binop Lte
        val exp_gte = mk_binop Gte
        val exp_nequal = mk_binop Nequal
        val exp_equalequal = mk_binop Equalequal

        fun exp_let (decs, exp) = Elet (decs, exp)

        fun exp_case (exp, cases) = Ecase (exp, cases)

        type toprepl = toprepl
        type topprog = dec list
        val toprepl_valdec = Topdec
        val toprepl_exp = Topexp
        val topprog_decs = identity

        type directive = directive
        val top_prog = Dprog
        val top_repl = Drepl

        fun error s =
          (case Stream.front s of
              Stream.Nil =>
                 ( print "Syntax error at end of file.\n"
                 ; Error
                 )
            | Stream.Cons (x, xs) =>
                 ( print ("Syntax error, somewhere.")
                 ; Error
                 )
         )
      end

    structure ParseMain =
      ParseMainFun
      (structure Streamable = StreamStreamable
      structure Arg = Arg)

    exception Error

    fun parse_repl s =
      #1 (ParseMain.parse (Stream.eager (Stream.Cons (Token.PARSE_REPL, Lexer.lex s))))

    fun parse_prog s =
      #1 (ParseMain.parse (Stream.eager (Stream.Cons (Token.PARSE_PROG, Lexer.lex_file s))))
  end

