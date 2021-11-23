
structure Token =
  struct
    datatype ty =
      INT
    | STR
    | CHAR
    | BOOL
    | VOID

    datatype token =
      NUMLIT of int
    | STRLIT of string
    | CHARLIT of char
    | BOOLLIT of bool
    | IDENT of string
    | TYLIT of ty
    | TYVAR of string

    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | LBRACKET
    | RBRACKET
    | EQUAL
    | SEMICOLON
    | COLON
    | CONS

    | UNDERSCORE
    | COMMA

    | SWITCH
    | CASE
    | RETURN

    | ASTERISK
    | PLUS
    | MINUS
    | SLASH
    | TILDE
    | LT
    | GT
    | LTE
    | GTE
    | NEQUAL
    | EQUALEQUAL
    | ARROW

    | PARSE_PROG
    | PARSE_REPL

  end
