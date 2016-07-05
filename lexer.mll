{
  open Parser
  exception Eof
  exception Reader_error of string

  let string_of_char c =
    String.make 1 c

  let string_reverse s =
    let maxind = (String.length s) - 1 in
      for i = 0 to maxind/2 do
	let c = s.[i] in
	  s.[i] <- s.[maxind-i];
	  s.[maxind-i] <- c
      done;
      s
}

rule token = parse
    (* The ordering of the rules here is significant: e.g. -1 is an
       integer but 1- should be a symbol *)
    ['-']? ['0'-'9']+ as lexeme		{ INT(int_of_string lexeme) }
  | '#' 				{ readermacro lexbuf }
  | ['+' '-']? ['0'-'9']* '.'? ['0'-'9']+ ( 'e' ['+' '-']? ['0'-'9']+ )?
      as lexeme { FLOAT(float_of_string lexeme) }
  |  ['A'-'Z' 'a'-'z' '-' '/' '*' '+' '0'-'9' ':' '&' '>'
         '<' '=' '!' '?' '$' '@']+ as lexeme { SYMBOL(lexeme) }
  | '"'					{ STRING(string lexbuf) }
  | [ '[' '(' ]				{ LPAREN }
  | [ ']' ')' ]				{ RPAREN }
  | '\''				{ QUOTE }
  | '.'					{ DOT }
  | ';' [^'\n']* '\n'			{ token lexbuf } (* skip comments *)
  | [' ' '\t' '\n']			{ token lexbuf } (* skip whitespace *)
  | eof					{ raise Eof }

and string = parse
    '\\'				{ let c = escaped lexbuf in c^(string lexbuf) }
  | [^ '"' '\\']+ as lexeme		{ lexeme^(string lexbuf) }
  | '"'					{ "" }
  | eof					{ raise Eof }

and escaped = parse
    (* TODO: escape sequences *)
    '^' (['A'-'Z'] as ctrlchar)
      { string_of_char (char_of_int ((int_of_char ctrlchar)-64)) }
  | '0' (['0'-'9']['0'-'9'] as digits)
      { string_of_char (char_of_int (int_of_string digits)) }
  | _ as lexeme				{ string_of_char lexeme }
  | eof					{ raise Eof }

(* TODO: actual Lisp-defined reader macros *)
and readermacro = parse
    '<'					{ raise (Reader_error "invalid reader macro") }
  | ['x''X'] (['-' '+'] as sign)? (['0'-'9' 'a'-'f' 'A'-'F']+ as digits)
      {
	match sign with
	    Some '-' -> INT(-(int_of_string ("0x"^digits)))
	  | Some '+' | None -> INT(int_of_string ("0x"^digits))
	  | Some _ -> raise (Lisp.Impossible "minus sign changed into something else in the middle of #X")
      }
  | ['o''O'] (['-' '+'] as sign)? (['0'-'7']+ as digits)
      {
	match sign with
	    Some '-' -> INT(-(int_of_string ("0o"^digits)))
	  | Some '+' | None -> INT(int_of_string ("0o"^digits))
	  | Some _ -> raise (Lisp.Impossible "minus sign changed into something else in the middle of #O")
      }
  | ['b''B'] (['-' '+'] as sign)? (['0' '1']+ as digits)
      {
	match sign with
	    Some '-' -> INT(-(int_of_string ("0b"^digits)))
	  | Some '+' | None -> INT(int_of_string ("0b"^digits))
	  | Some _ -> raise (Lisp.Impossible "minus sign changed into something else in the middle of #B")
      }
  | (['0'-'9']+ as radix) ['r''R'] (['-''+'] as sign)? (['0'-'9' 'A'-'Z' 'a'-'z']+ as digits)
      {
	let radix = int_of_string radix in
	let parse string =
	  let dpow a exp =
	    match exp with
		0 -> 1
	      | 1 -> a
	      | e when e >= 2 ->
		  begin
		    let result = ref a in
		      for i = 2 to exp do
			result := !result * a
		      done;
		      !result
		  end
	      | e -> raise (Lisp.Impossible ("exponent `"^(string_of_int e)^"' slipped through guarded clause in #R"))
	  in
	  let index = ref 0 in
	  let value = ref 0 in
	    String.iter (fun c ->
			   let digit =
			     match c with
				 '0'..'9' -> (int_of_char c) - (int_of_char '0')
			       | 'A'..'Z' -> (int_of_char c) - (int_of_char 'A') + 10
			       | _ -> raise (Lisp.Impossible "invalid digit in #R")
			   in
			   let placeval = dpow radix !index in
			     if digit >= radix then
			       raise (Reader_error "digit greater than allowed radix in #R")
			     else begin
			       value := !value + digit*placeval;
			       incr index
			     end
			) (String.uppercase (string_reverse string));
	    !value
	in
	let value = parse digits in
	  match sign with
	      Some '-' -> INT(-value)
	    | Some '+' | None -> INT(value)
	    | Some _ -> raise (Lisp.Impossible "minus sign changed into something else in the middle of #R reader macro")	  
      }
  | '\\' (_ as char)			{ INT(int_of_char char) }
  | '\''				{ FUNQUOTE }
  | '.'
      {
	(* read-time evaluation *)
	if (not (Hashtbl.mem Eval.variables "*read-eval*")) || (Lisp.truep (Hashtbl.find Eval.variables "*read-eval*")) then
	  LISPVAL(Eval.eval (Parser.main token lexbuf))
	else
	  raise (Reader_error "Readtime evaluation disabled (*read-eval* nil)")
      }
  | '+'
      {
	(* TODO: *features* *)
	let condition = Parser.main token lexbuf in
	let expression = Parser.main token lexbuf in
	let result = Eval.eval condition in
	  if Lisp.truep result then
	    LISPVAL(expression)
	  else
	    token lexbuf
      }
  | '-'
      {
	let condition = Parser.main token lexbuf in
	let expression = Parser.main token lexbuf in
	let result = Eval.eval condition in
	  if not (Lisp.truep result) then
	    LISPVAL(expression)
	  else
	    token lexbuf
      }
  | '|'					{ long_comment lexbuf; token lexbuf }
  | _					{ raise (Reader_error "invalid reader macro") }
and long_comment = shortest
    '|' '#'				{ () }
| '#' '|'				{ long_comment lexbuf; long_comment lexbuf }
| [^'|' '#']+				{ long_comment lexbuf }
| '|' _				{ long_comment lexbuf }
| '#' _				{ long_comment lexbuf }

      {
	(* trailer *)
}
