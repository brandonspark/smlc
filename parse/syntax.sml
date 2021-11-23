
structure Syntax =
  struct
    datatype typee =
      Tint
    | Tbool
    | Tstring
    | Tchar
    | Tunit
    | Ttyvar of string
    | Tprod of typee list
    | Ttycon of typee * string
    | Tarrow of typee * typee

    and pat =
      Pident of string
    | Pwild
    | Pprod of pat list
    | Pnum of int
    | Pstring of string
    | Pchar of char
    | Pbool of bool
    | Papp of string * pat
    | Pcons of pat * pat
    | Pnil

    and exp =
      Enum of int
    | Eneg of exp
    | Estring of string
    | Echar of char
    | Ebool of bool
    | Eident of string
    | Ecase of exp * (pat * exp) list
    | Elet of dec list * exp
    | Etuple of exp list
    | Ebinop of exp * binop * exp
    | Eapp of exp * exp
    | Enil

    and binop =
      Add
    | Sub
    | Div
    | Mul
    | Cons
    | Equalequal
    | Nequal
    | Lt
    | Gt
    | Lte
    | Gte

    and toprepl =
      Topdec of dec
    | Topexp of exp

    and directive =
      Dprog of dec list
    | Drepl of toprepl

    and dec =
      Dval of typee * string * exp
    | Dfun of typee * string * (typee * string) list * exp

    fun out_typee typee =
      case typee of
        Tint => "int"
      | Tbool => "bool"
      | Tstring => "string"
      | Tchar => "char"
      | Tunit => "unit"
      | Ttyvar s => s
      | Tprod ts =>
          "(" ^ String.concatWithMap " * " out_typee ts ^ ")"
      | Ttycon (t, con) =>
          out_typee t ^ " " ^ con
      | Tarrow (t1, t2) =>
          "(" ^ out_typee t1 ^ " -> " ^ out_typee t2 ^ ")"

    and out_pat pat =
      case pat of
        Pident s => s
      | Pnum n => Int.toString n
      | Pstring s => s
      | Pchar c => Char.toString c
      | Pbool b => if b then "true" else "false"
      | Pwild => "_"
      | Pprod ps =>
          "(" ^ String.concatWithMap ", " out_pat ps ^ ")"
      | Papp (id, pat) =>
          id ^ " (" ^ out_pat pat ^ ")"
      | Pcons (p1, p2) =>
          out_pat p1 ^ " :: (" ^ out_pat p2 ^ ")"
      | Pnil => "[]"

    and out_exp exp =
      case exp of
        Enum i => Int.toString i
      | Eneg e => "(~" ^ out_exp e ^ ")"
      | Estring s => "\"" ^ s ^ "\""
      | Echar c => "#\"" ^ Char.toString c ^ "\""
      | Ebool b => if b then "true" else "false"
      | Eident s => s
      | Ecase (e, cases) =>
          "(" ^ "case " ^ out_exp e ^ " of "
          ^ (String.concatWithMap " | " (fn (pat, exp) =>
              out_pat pat ^ " => " ^ out_exp exp) cases)
          ^ ")"
      | Elet (decs, exp) =>
          "(let " ^
            String.concatWithMap
              " "
              out_dec
              decs
          ^ " in "
          ^ out_exp exp
          ^ " end)"
      | Etuple es =>
          "(" ^ String.concatWithMap ", " out_exp es ^ ")"
      | Ebinop (e1, binop, e2) =>
          let
            val binop_s =
              case binop of
                Add => "+"
              | Mul => "*"
              | Sub => "-"
              | Div => "div"
              | Cons => "::"
              | Lt => "<"
              | Gt => ">"
              | Lte => "<="
              | Gte => ">="
              | Nequal => "!="
              | Equalequal => "=="
          in
            "(" ^ out_exp e1 ^ " " ^ binop_s ^ " " ^ out_exp e2 ^ ")"
          end
      | Eapp (e1, e2) =>
          out_exp e1 ^ "(" ^ out_exp e2 ^ ")"
      | Enil => "[]"

    and out_dec dec =
      case dec of
        Dval (ty, id, exp) =>
          "val " ^ id ^ " : " ^ out_typee ty ^ " = " ^ out_exp exp ^ ";"
      | Dfun (ty, id, args, exp) =>
          "fun " ^ id ^ " (" ^
            String.concatWithMap ", " (fn (ty, id) => id ^ " : " ^ out_typee ty)
              args
          ^ ") : " ^ out_typee ty ^ " = " ^ out_exp exp ^ ";"


    fun out_top top =
      case top of
        Topdec dec => out_dec dec
      | Topexp exp =>
          out_exp exp ^ ";"
  end
