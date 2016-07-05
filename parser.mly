%{
  open Lisp

  let magic_atom_handler a =
    match a with
	Symbol "nil" -> List Nil
      | Symbol s -> Atom(Symbol s)
      | Int i -> Atom(Int i)
      | Float f -> Atom(Float f)
      | String s -> Atom(String s)
%}
%token <int> INT
%token <float> FLOAT
%token <string> SYMBOL
%token <string> STRING
%token <Lisp.lispval> LISPVAL
%token LPAREN RPAREN DOT QUOTE FUNQUOTE
%start main
%type <Lisp.lispval> main
%%

main:
    expr				{ $1 }
;

exprlist:
    expr exprlist			{ Cons($1, List $2) }
  | expr				{ Cons($1, List Nil) }
;

expr:
    atom				{ magic_atom_handler $1 }
  | LISPVAL
      {
	(* magic pass-through value for read-time evaluation *)
	$1
      }
  | list				{ List $1 }
/*(*  | LPAREN expr DOT expr RPAREN		{ List(Cons($2, $4)) }*)*/
  | QUOTE expr				{ List(Cons(Atom(Symbol "quote"), List(Cons($2, List Nil)))) }
  | FUNQUOTE expr			{ List(Cons(Atom(Symbol "function"), List(Cons($2, List Nil)))) }
;

list:
    LPAREN exprlist RPAREN		{ $2 }
  | LPAREN exprlist DOT expr RPAREN
      {
	let rec dot_handler l t =
	  match l with
	      Cons(a, List Nil) -> Cons(a, t)
	    | Cons(a, List tail) -> Cons(a, List(dot_handler tail t))
	    | Cons(_, Atom _) -> raise (Impossible "improper list as first part of dot notation")
	    | Nil -> raise (Impossible "should never get here")
	in
	  dot_handler $2 $4
      }
  | LPAREN RPAREN			{ Nil }
;

atom:
    INT					{ Int $1 }
  | FLOAT				{ Float $1 }
  | SYMBOL				{ Symbol $1 }
  | STRING				{ String $1 }
;

