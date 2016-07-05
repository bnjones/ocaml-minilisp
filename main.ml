open Lisp
open Eval

let print_trace trace =
  let rec join l s =
    match l with
      [x] -> x
      | h::t -> h^s^(join t s)
      | [] -> ""
  in
    print_string ("\nHistory: "^(join trace " <- "))

let update_repl_histvars char newvalue =
  let starstarstar = String.make 3 char in
  let starstar = String.make 2 char in
  let star = String.make 1 char in
  begin
    try
      bind starstarstar (Hashtbl.find variables starstar)
    with Not_found -> ()
  end;
  begin
    try
      bind starstar (Hashtbl.find variables star)
    with Not_found -> ()
  end;
  bind star newvalue

let _ =
  let deparse = ref Deparse.deparse in
  let tracefun = ref ignore in
  let timings = ref false in
  let infiles = ref [] in
  let curfile = ref None in
  let prompt = ref "#> " in
  Arg.parse (Arg.align [
    ("-camlrepr",
     Arg.Unit (fun () -> deparse := Deparse.caml_repr),
     " deparse as OCaml");
    ("-bt", Arg.Unit
      (fun () -> Eval.dotrace := true; tracefun := print_trace),
     " print backtrace on errors");
    ("-maxdepth", Arg.Set_int Eval.max_depth, " set max eval depth");
    ("-timings", Arg.Set timings, " print timings");
    ("-noprompt", Arg.Unit (fun () -> prompt := ""), " disable prompt")])
    (fun arg -> infiles := arg::!infiles) "minilisp";
  infiles := List.rev !infiles;

  let interactive = !infiles = [] in
  let repl () =
    if interactive then
      begin
        print_string !prompt;
        flush stdout;
      end;
    begin
      let input =
        Reader.read_channel (
          if interactive then
            stdin
          else begin
            match !curfile with
              Some file -> file
            | None ->
              begin
                let fn = List.hd !infiles in
                let file = open_in fn in
                curfile := Some file;
                file
              end
          end) in
      let start_time = Unix.gettimeofday() in
      begin
        Eval.depth := 0;
        Eval.entry_time := Sys.time();
        Eval.trace := [];
        bind "-" input;
        let result = eval input in
        update_repl_histvars '+' input;
        update_repl_histvars '*' result;
        print_string (!deparse result)
      end;
      print_newline();
      (if !timings then
          Printf.printf "(%.5fs)\n" (
            Unix.gettimeofday() -. start_time));
    end
  in

  bind "t" (Atom(Symbol "t"));
  Functions.setup();
  defun "show-scope" (Camlfun lisp_show_scope);
  defun "load-library" (Camlfun lisp_load_library);

  while true do
    begin
      try
        repl ()
      with Lexer.Eof ->
        begin
          Reader.close_channel();
          curfile := None;
          if !infiles = [] then exit 0;
          infiles := List.tl !infiles;
          if !infiles = [] then exit 0
        end
      | Lisp.Void_variable v ->
        print_string ("Void variable '"^v^"'"); !tracefun !Eval.trace;
        print_newline(); Reader.close_channel()
      | Lisp.Void_function f ->
        print_string ("Void function '"^f^"'"); !tracefun !Eval.trace;
        print_newline(); Reader.close_channel()
      | Lisp.Eval_error msg ->
        print_string ("Evaluation stopped: "^msg); !tracefun !Eval.trace;
        print_newline(); Reader.close_channel()
      | Lisp.Type_mismatch ->
        print_string "Type mismatch somewhere"; !tracefun !Eval.trace;
        print_newline(); Reader.close_channel()
      | Lisp.Argument_mismatch ->
        print_string "Argument mismatch"; !tracefun !Eval.trace;
        print_newline(); Reader.close_channel()
      | Dynlink.Error e ->
        print_string (Dynlink.error_message e); !tracefun !Eval.trace;
        print_newline(); Reader.close_channel()
      | any_other_exception ->
        begin
          print_string ("Uncaught OCaml exception: "^
                           (Printexc.to_string any_other_exception)^"\n");
          Reader.close_channel()
        end
    end
  done
