let channel_lexbuf = ref None

let read_channel chan =
  try
    let lexbuf =
      match !channel_lexbuf with
	  Some buf -> buf
	| None ->
	    begin
	      let lexbuf = Lexing.from_channel chan in
		channel_lexbuf := Some lexbuf;
		lexbuf
	    end
    in
      Parser.main Lexer.token lexbuf
  with
    Failure "lexing: empty token" ->
      raise (Lexer.Reader_error "invalid token syntax")

let close_channel () =
  channel_lexbuf := None
    
let read_string s =
  try
    let lexbuf = Lexing.from_string s in
      Parser.main Lexer.token lexbuf
  with
    Failure "lexing: empty token" ->
      raise (Lexer.Reader_error "invalid token syntax")
