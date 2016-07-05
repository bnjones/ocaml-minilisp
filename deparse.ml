open Lisp

let square_parens = ref false

let unlex a =
  match a with
      Symbol s -> s
    | String s -> "\""^(String.escaped s)^"\""
    | Float f ->
	let s = string_of_float f in
	  if (String.contains s '.') && ((fst (modf f)) = 0.0) then
	    s^"0"
	  else
	    s	      
    | Int i -> string_of_int i

let deparse v =
  let rec unlex_maybe v =
    match v with
	Atom a -> unlex a
      | List l -> if l = Nil then "nil" else begin
	  if !square_parens then
	    "["^(deparse_real l)^"]"
	  else
	    "("^(deparse_real l)^")"
	end
  and deparse_real l =
    match l with
	Cons(v, List l) -> begin
	  if l = Nil then
	    unlex_maybe v
	  else
	    (unlex_maybe v)^" "^(deparse_real l)
	end
      | Cons(a, b) -> (unlex_maybe a)^" . "^(unlex_maybe b)
      | Nil -> "nil"
  in
    unlex_maybe v

let rec caml_repr v =
  let caml_atom_repr a =
    match a with
	Symbol s -> "Symbol \""^s^"\""
      | String s -> "String \""^s^"\""
      | Float f -> "Float "^(string_of_float f)
      | Int i -> "Int "^(string_of_int i)
  in
  let rec caml_cons_repr c =
    match c with
	Cons(v, c) -> "Cons("^(caml_repr v)^", "^(caml_repr c)^"))"
      | Nil -> "Nil"
  in
    match v with
	Atom a -> "Atom("^(caml_atom_repr a)^")"
      | List l -> "List("^(caml_cons_repr l)^")"
