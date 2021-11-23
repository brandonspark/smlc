
structure Lexer :>
  sig
    val lex : string -> Token.token Stream.stream
    val lex_to_list : string -> Token.token list
    val lex_file : string -> Token.token Stream.stream
    val lex_file_to_list : string -> Token.token list
  end =
  struct
    open Token

    exception Error

    structure Table =
      HashTable (structure Key = StringHashable)

    val keywords : token Table.table = Table.table 20

    val () =
      List.app
        (fn (str, token) => Table.insert keywords str token)
        [ ("int", TYLIT INT)
        , ("void", TYLIT VOID)
        , ("char", TYLIT CHAR)
        , ("string", TYLIT STR)
        , ("bool", TYLIT BOOL)
        , ("switch", SWITCH)
        , ("case", CASE)
        , ("return", RETURN)
        , ("true", BOOLLIT true)
        , ("false", BOOLLIT false)
        ]

    open Stream

    type t = token front
    type v = char list -> char list * char stream
    type u = char stream

    type self = { lex : char stream -> t
                , string : char stream -> v
                , comment : char stream -> u
                , comment_nl : char stream -> u
                }

    type info = { match : char list,
                  len : int,
                  start : char stream,
                  follow : char stream,
                  self : self }

    fun action f ({ match, len, follow, self, ... }:info) =
       Cons (f (match, len), lazy (fn () => #lex self follow))

    fun simple token ({ len, follow, self, ... }:info) =
       Cons (token, lazy (fn () => #lex self follow))

    fun revappend l1 l2 =
       (case l1 of
           [] => l2
         | x :: rest =>
              revappend rest (x :: l2))

    structure Arg =
      struct
        type symbol = char
        val ord = Char.ord

        type t = t
        type v = v
        type u = u

        type self = self
        type info = info

        fun eof _ = Nil

        val ident =
           action
           (fn (chars, _) =>
                  let
                     val str = implode chars
                  in
                     (case Table.find keywords str of
                         NONE =>
                            IDENT str
                       | SOME token =>
                            token)
                  end)
        val select =
           action
           (fn (chars, _) =>
                  IDENT (implode chars)
            )
        val tyvar =
           action
           (fn (chars, _) =>
                  TYVAR (implode chars)
            )
        fun skip ({ len, follow, self, ... }:info) = #lex self follow

        fun skip_newline ({ len, follow, self, ... }:info) = #lex self follow

        val equal = simple EQUAL
        val lbrace = simple LBRACE
        val rbrace = simple RBRACE
        val lparen = simple LPAREN
        val rparen = simple RPAREN
        val lbracket = simple LBRACKET
        val rbracket = simple RBRACKET
        val colon = simple COLON
        val semicolon = simple SEMICOLON
        val asterisk = simple ASTERISK
        val plus = simple PLUS
        val minus = simple MINUS
        val slash = simple SLASH
        val tilde = simple TILDE
        val comma = simple COMMA
        val equal = simple EQUAL
        val underscore = simple UNDERSCORE
        val arrow = simple ARROW
        val cons = simple CONS
        val lt = simple LT
        val gt = simple GT
        val lte = simple LTE
        val gte = simple GTE
        val not_equal = simple NEQUAL
        val equal_equal = simple EQUALEQUAL

       fun mk_number f =
         action
         (fn (match, len) =>
           (case f (implode match) of
             SOME n => NUMLIT n
           | NONE =>
               raise Fail ("illegal number")))

       val number = mk_number Int.fromString

       fun enter_comment ({len, follow, self, ...}: info) =
         let
           val follow' = #comment self follow
         in
           #lex self follow'
         end

       fun enter_comment_nl ({len, follow, self, ...}: info) =
         let
           val follow' = #comment_nl self follow
         in
           #lex self follow'
         end

       fun enter_string ({len, follow, self, ...}: info) =
         let
           val (chars, follow') = #string self follow []
         in
           Cons (STRLIT (String.implode (rev chars)),
                 lazy (fn () => (#lex self follow')))
         end

        fun enter_char ({len, follow, self, ...}: info) =
          let
            val (chars, follow') = #string self follow []
          in
            case chars of
              [ch] =>
                Cons (CHARLIT ch,
                      lazy (fn () => #lex self follow'))
            | _ =>
                raise Fail ("illegal character constant")
          end

        (* comment *)

        fun reenter_comment ({ len, follow, self, ...}: info) =
          let
            val follow' = #comment self follow
          in
            #comment self follow'
          end

        fun exit_comment ({ len, follow, ...}: info) =
          follow

        fun comment_skip ({ len, follow, self, ...} : info) =
          #comment self follow

        fun unclosed_comment _ = raise Fail ("unclosed comment")
        fun comment_error _ = raise Fail ("illegal character")

        (* string *)

        fun string_action f ({ match, len, follow, self, ...}: info) acc =
          #string self follow (f (match, acc))

        val string_elem =
          string_action
          (fn (match, acc) => revappend match acc)

        val string_newline =
          string_action
          (fn (_, acc) => #"\n" :: acc)

        val string_backslash =
          string_action
          (fn (_, acc) => #"\\" :: acc)

        val string_quote =
          string_action
          (fn (_, acc) => #"\"" :: acc)

        fun unclosed_string _ _ = raise (Fail ("unclosed string"))
        fun string_error _ _ = raise (Fail ("illegal character"))

        fun string_skip ({ len, follow, self, ... }:info) acc =
          #string self follow acc

        fun exit_string ({ len, follow, ... }:info) =
          fn acc => (acc, follow)


        fun error _ =
            (
            print "Lexical error.\n";
            raise Error
            )

      end

    structure LexMain =
      LexMainFun
      (structure Streamable = StreamStreamable
       structure Arg = Arg)

    fun lex s = lazy (fn () => LexMain.lex (Stream.fromString s))
    fun lex_to_list s = Stream.toList (lex s)
    fun lex_file filename =
      let
        val ins = TextIO.openIn filename
        val e =
            Stream.fromList (String.explode (TextIO.inputAll ins))
        val () = TextIO.closeIn ins
      in
        lazy (fn () => LexMain.lex e)
      end
    val lex_file_to_list = Stream.toList o lex_file
  end
