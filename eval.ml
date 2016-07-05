open Lisp

type lispfun =
  (* Lisp function: lambda-list * body *)
  Lispfun of cons * cons
  (* OCaml function: called with the cdr of the form, and the return
     value is the result of evaluating that expression *)
| Camlfun of (cons -> lispval)
  (* Same as Camlfun, but the arguments are passed unevaluated *)
| Specialform of (cons -> lispval)

let evalfail msg =
  raise (Eval_error msg)

let pairfail () =
  raise Sequence_expected

let impossible msg =
  raise (Impossible msg)

let variables = (Hashtbl.create 64 : (string, lispval) Hashtbl.t)
let functions = (Hashtbl.create 64 : (string, lispfun) Hashtbl.t)
let properties = (Hashtbl.create 32 : (string, string list) Hashtbl.t)

let magic_closure_symbol = Symbol "#<closure>"
let defun name func =
  Hashtbl.replace functions name func

let newbind name value =
  if name.[0] = ':' then
    raise (Lisp.Setting_constant name)
  else
    Hashtbl.add variables name value
  
let bind name value =
  if name.[0] = ':' then
    raise (Lisp.Setting_constant name)
  else
    Hashtbl.replace variables name value
      
let unbind name = Hashtbl.remove variables name

let rec bind_args_worker symbols args seen_optional =
  let symcar c =
    match c with
	Cons(Atom(Symbol s), _) -> s
      | _ -> evalfail "lambda arglist car was expected to be a symbol"
  in
  let conscdr c =
    match c with
	Cons(_, List l) -> l
      | Nil -> if seen_optional then Nil else impossible "fell off the end of function arglist without having seen &optional"
      | _ -> evalfail "non-list encountered in lambda argument list"
  in
    if symbols = Nil && args != Nil then
      raise Argument_mismatch;
    if symbols != Nil && args = Nil
      && not seen_optional
      && not (symcar symbols = "&optional")
    then
      raise Argument_mismatch;
    if symbols = Nil && args = Nil then
      ()
    else begin
      match args with
	  Cons(hd, List tl) -> begin
	    let lambda_list_element = symcar symbols in
	      match lambda_list_element with
		  "&optional" ->
		    if seen_optional then
		      evalfail "lambda list keyword &optional not allowed twice"
		    else
		      bind_args_worker (conscdr symbols) args true
		| symbol -> begin
		    newbind lambda_list_element hd;
		    bind_args_worker (conscdr symbols) tl seen_optional
		  end
	  end
	| Cons(hd, Atom a) -> evalfail "can't bind arguments from a pair"
	| Nil -> begin
	    let lambda_list_element = symcar symbols in
	      match lambda_list_element with
		  "&optional" ->
		    if seen_optional then
		      evalfail "lambda list keyword &optional not allowed twice"
		    else
		      bind_args_worker (conscdr symbols) Nil true
		| symbol -> begin
		    newbind lambda_list_element (List Nil);
		    bind_args_worker (conscdr symbols) Nil seen_optional
		  end
	  end
    end
and bind_args symbols args = bind_args_worker symbols args false


let rec collect_argnames args =
  match args with
      Cons(Atom(Symbol hd), List tl) -> hd::(collect_argnames tl)
    | Nil -> []
    | _ -> evalfail "collect_argnames: elements of argument list must be symbols"

(* Flatten an argument list like the arguments to apply
    e.g. (apply '+ a b c '(d e f)) => (+ a b c d e f)
   If the last argument is a list, it's appended to the end of the
   argument list *)
let rec flatten args =
  match args with
      Cons(List(Cons(hd, List cdr)), List Nil) ->
	Cons(hd, List cdr)
    | Cons(hd, List(Cons(List final, List Nil))) ->
	Cons(hd, List final)
    | Cons(hd, List tl) -> Cons(hd, List (flatten tl))
    | Nil -> Nil
    | _ -> evalfail "unflattenable object"

let depth = ref 0
let max_depth = ref 500
let entry_time = ref (Sys.time())
let max_time = ref None
let dotrace = ref false
let trace = ref []
exception Max_time_exceeded
exception Max_eval_depth_exceeded
  
let trace_enter name =
  if !dotrace then trace := name::!trace

let trace_exit () =
  if !dotrace then trace := List.tl !trace
  
let rec dispatch name args =
  begin
    match !max_time with
	Some t ->
	  if ((Sys.time()) -. !entry_time) > t then
	    raise Max_time_exceeded
      | None -> ()
  end;
  begin
    match args with
	List l ->
	  begin
	    let args = l in
	      try
		incr depth;
		(if (!depth) > (!max_depth) then
		   (depth := 0; raise Max_eval_depth_exceeded));
		trace_enter name;
		let func = Hashtbl.find functions name in
		let func_eval_result = match func with
		    Lispfun (argnames, body) ->
		      dispatch_lambda (List(Cons(Atom(Symbol "lambda"), List(Cons(List argnames, List body))))) args
		  | Camlfun f ->
		      let args = map_cons eval args in
			f args
		  | Specialform f -> f args
		in
		  decr depth;
		  trace_exit();
		  func_eval_result
	      with Not_found -> (decr depth; raise (Lisp.Void_function name))
	  end
      | _ -> raise Sequence_expected
  end
and eval v =
  let eval_atom a =
    match a with
	Symbol "nil" -> impossible "parser didn't handle nil properly"
      | (Symbol "t" as t) -> Atom t
      | Symbol s -> begin
	  if s.[0] = ':' then
	    Atom(Symbol s)
	  else
	    begin
	      try
		Hashtbl.find variables s
	      with Not_found -> raise (Lisp.Void_variable s)
	    end
	end
      | Float f -> Atom (Float f)
      | Int i -> Atom (Int i)
      | String s -> Atom (String s)
  in
    match v with
	Atom a -> eval_atom (a)
      | List (Cons(car, cdr)) ->
	  begin
	    match car with
		Atom(Symbol "quote") ->
		  begin
		    match cdr with
			List l when l != Nil -> Lisp.car l
		      | _ -> evalfail "quote with no arguments or used as the car of a pair"
		  end
	      | Atom(Symbol s) -> dispatch s cdr
	      | List l ->
		  begin
		    match cdr with
			List arglist -> begin
			  trace_enter "#<lambda>";
			  let v = dispatch_lambda (List l) arglist in
			    trace_exit();
			    v
			end
		      | _ -> evalfail "lambda expression used as car of a pair"
		  end
	      | a -> raise (Lisp.Void_function (Deparse.deparse a))
	  end
      | List Nil -> List Nil
and progn args =
  match args with
      Cons(a, List Nil) -> eval a
    | Cons(a, List tl) -> (ignore (eval a); progn tl)
    | Cons(a, _) -> pairfail()
    | Nil -> List Nil
and dispatch_lambda lambda args =
  match lambda with
      List(Cons(Atom(Symbol "lambda"), List(Cons(List argnames, List body)))) ->
	begin
	  let unbind_args argnames =
	    iter_cons (fun v ->
			 match v with
			     Atom(Symbol "&optional") -> ()
			   | Atom(Symbol s) -> unbind s
			   | _ -> impossible "unbinding from non-symbol in arglist after presumably successful binding earlier"
		      ) argnames
	  in
	  let args = map_cons eval args in
	    bind_args argnames args;
	    let result =
	      try
		progn body
	      with some_exception ->
		unbind_args argnames;
		raise some_exception
	    in
	      unbind_args argnames;
	      result
	end
    | List(Cons(Atom(magic_closure_symbol), List(Cons(List arglist, List(Cons(List body, List Nil)))))) ->
	letbindings (fun () ->
		       dispatch_lambda (List body) args) arglist
    | _ -> evalfail "Malformed lambda expression"
and letbindings f varlist =
  let bindings = ref [] in
  let iter_varlist varlist =
    iter_cons
      begin
	fun v ->
	  match v with
	      List(Cons(Atom(Symbol name), List(Cons(value, List Nil)))) ->
		bindings := (name, eval value)::!bindings
	    | Atom(Symbol name) ->
		bindings := (name, List Nil)::!bindings
	    | _ -> evalfail "letbindings: invalid form in varlist"
      end
      varlist
  in
    iter_varlist varlist;
    List.iter (fun b -> let name, value = b in newbind name value) !bindings;
    let result =
      try
	f ()
      with some_exception ->
	List.iter (fun b -> let name, value = b in unbind name) !bindings;
	raise some_exception
    in
      List.iter (fun b -> let name, value = b in unbind name) !bindings;
      result

let one_argument args =
  match args with
      Cons(v, List Nil) -> v
    | _ -> evalfail "one_argument: too many or too few arguments to function"
	
let two_arguments args =
  match args with
      Cons(a, List(Cons(b, List Nil))) ->
	(a, b)
    | _ -> evalfail "two_arguments: a function requires two arguments but received too many or too few"

	
let lisp_cons args =
  match args with
      Cons(a, List(Cons(b, List Nil))) -> List (Cons(a, b))
    | _ -> evalfail "lisp_cons: wrong number of arguments or pair"
	  
let rec lisp_set args =
  match args with
      Cons(a, List(Cons(b, List tail))) ->
	begin
	  match a with
	      Atom(Symbol name) -> bind name b; lisp_set tail
	    | _ -> evalfail "lisp_set: trying to bind a value to a non-symbol"
	end
    | Nil -> List Nil
    | _ -> evalfail "lisp_set: not enough arguments or other mysterious problem"

let rec lisp_setq args =
  match args with
      Cons(a, List(Cons(b, List tail))) ->
	begin
	  match a with
	      Atom(Symbol name) -> bind name (eval b); lisp_setq tail
	    | _ -> evalfail "lisp_setq: trying to bind a value to a non-symbol"
	end
    | Nil -> List Nil
    | _ -> evalfail "lisp_setq: not enough arguments or other mysterious problem"

let lisp_prog1 args =
  match args with
      Cons(hd, List tl) ->
	let result = eval hd in
	  ignore (progn tl);
	  result
    | Cons(hd, _) -> pairfail()
    | Nil -> evalfail "lisp_prog1: not enough arguments"

let lisp_prog2 args =
  match args with
      Cons(a, List(Cons(b, List tl))) ->
	ignore (eval a);
	let result = eval b in
	  ignore (progn tl);
	  result
    | Cons(_, Atom _) -> pairfail()
    | _ -> evalfail "lisp_prog2: not enough arguments"
      
let lisp_left_mathop intop floatop initval args =
  Lisp.fold_left_cons (fun a b ->
			 match compatible_numbers_from_lispvals a b with
			     `Ints(a, b) -> Atom(Int (intop a b))
			   | `Floats(a, b) -> Atom(Float (floatop a b))
		      ) (Atom(initval)) args

let lisp_add = lisp_left_mathop (+) (+.) (Int 0)
let lisp_mul = lisp_left_mathop ( * ) ( *. ) (Int 1)
let lisp_div args =
  match args with
      Cons(Atom a, List tl) when tl != Nil ->
	lisp_left_mathop (/) (/.) a tl
    | _ -> evalfail "lisp_div: not enough arguments"

let lisp_mod args =
  let a, b = two_arguments args in
    match a, b with
	(Atom(Int a_value), Atom(Int b_value)) -> Atom(Int (a_value mod b_value))
      | _ -> raise Type_mismatch
	
let lisp_sub args =
  (* (progn (setq a 42) (- a)) => -42
     (- 2 1) => 1
     (- 1 2) => -1
     (- 1 2 3 4 5) => -13
     (-) => 0
   *)
  let flip_sign a =
    match a with
	Atom(Int a) -> Atom(Int (-a))
      | Atom(Float a) -> Atom(Float (-.a))
      | _ -> raise Type_mismatch
  in
    match args with
	Cons(hd, List Nil) -> flip_sign hd
      | Cons(hd, List tl) -> lisp_add (Cons(hd, List(map_cons flip_sign tl)))
      | Cons(hd, _) -> pairfail()
      | Nil -> Atom(Int 0)
	  
let lisp_print_string args =
  let catargs = fold_left_cons (fun a b ->
				  match a with
				      Atom(String a_value) ->
					begin
					  match b with
					      Atom(String b_value) ->
						Atom(String (a_value^b_value))
					    | _ -> raise Type_mismatch
					end
				    | _ -> raise Type_mismatch)
    (Atom(String "")) args
  in
    print_string
      begin
	match catargs with
	    Atom(String s) -> s
	  | _ -> impossible "Arguments to lisp_print_string slipped through a watertight type check during concatenation"
      end;
    print_newline();
    List Nil

let lisp_eval args =
  match args with
      Cons(a, List Nil) -> eval a
    | Cons(a, List l) -> evalfail "lisp_eval: too many arguments"
    | Cons(_, _) -> pairfail()
    | Nil -> evalfail "lisp_eval: not enough arguments"

let handle_defun_form defun_func args =
  match args with
      Cons(name, List(Cons(List arglist, List body))) ->
	begin
	  let name =
	    match name with
		Atom(Symbol s) -> s
	      | _ -> raise Type_mismatch
	  in
	    (try
	      match Hashtbl.find functions name with
		  Lispfun(_, _) -> ()
		| _ -> evalfail "can't redefine primitive function"
	    with Not_found -> ());
	    defun_func name (Lispfun(arglist, body));
	    Atom(Symbol name)
	end
    | _ -> evalfail "handle_defun_form: not enough arguments or body or arglist not a list"
	
let lisp_defun args =
  handle_defun_form defun args

let lisp_labels args =
  let pushdefun name func =
    Hashtbl.add functions name func
  in
  match args with
      Cons(List deflist, List body) ->
	begin
	  let bindings = ref [] in
	  let iter_deflist deflist =
	    iter_cons
	      begin
		fun v ->
		  match v with
		      List def ->
			begin
			  match def with
			      Cons(Atom(Symbol name), List body) -> (bindings := name::!bindings; handle_defun_form pushdefun def)
			    | _ -> evalfail "lisp_labels: invalid definition (case 1) in deflist"
			end
		    | _ -> evalfail "lisp_labels: invalid definition (case 2) in deflist"
	      end
	      deflist
	  in
	    iter_deflist deflist;
	    let result =
	      try
		progn body
	      with some_exception ->
		List.iter (fun b -> Hashtbl.remove functions b) !bindings;
		raise some_exception
	    in
	      List.iter (fun b -> Hashtbl.remove functions b) !bindings;
	      result
	end
    | _ -> evalfail "lisp_labels: too few or too many or wrong type arguments"
	
  

let lisp_show_scope args =
  Hashtbl.iter (fun key value ->
		  print_string ("'"^key^"': value is:\n    "^(Deparse.deparse value)^"\n")) variables;
  List Nil

let lisp_car args =
  match args with
      Cons(List a, List Nil) -> car a
    | Cons(_, List Nil) -> raise Type_mismatch
    | _ -> evalfail "lisp_car: Too many or too few arguments"
	
let lisp_cdr args =
  match args with
      Cons(List a, List Nil) -> cdr a
    | Cons(_, List Nil) -> raise Type_mismatch
    | _ -> evalfail "lisp_cdr: Too many or too few arguments"
	
let lisp_list args = List args

let rec lisp_liststar args =
  match args with
      Cons(a, List Nil) -> a
    | Cons(a, List(Cons(b, List Nil))) -> List(Cons(a, b))
    | Cons(a, List(Cons(b, List tail))) -> List(Cons(a, List(Cons(b, lisp_liststar tail))))
    | Cons(_, Atom _)
    | Cons(_, List(Cons(_, Atom _))) -> pairfail ()
    | Nil -> raise Argument_mismatch
  
let lisp_atomp args =
  generalised_bool (atomp (one_argument args))

let lisp_symbolp args =
  generalised_bool (symbolp (one_argument args))

let lisp_numberp args =
  generalised_bool (numberp (one_argument args))
	
let lisp_consp args =
  generalised_bool (consp (one_argument args))

let lisp_listp args =
  generalised_bool (listp (one_argument args))

let lisp_stringp args =
  generalised_bool (stringp (one_argument args))

let lisp_when args =
  if truep (eval (car args)) then
    progn (cdrlist args)
  else
    List Nil
    
let lisp_if args =
  let rest = cdrlist args in
    if truep (eval (car args)) then
      eval (car rest)
    else
      progn (cdrlist rest)

let rec lisp_cond args =
  let cond_clause clause =
    match clause with
	Cons(cond, List Nil) ->
	  let result = eval cond in
	    if truep result then (true, result) else (false, List Nil)
      |	Cons(cond, List body) ->
	  if truep (eval cond) then
	    (true, progn body)
	  else
	    (false, List Nil)
      | Cons(_, _) -> evalfail "cond_clause: clause is a pair"
      | Nil -> evalfail "cond_clause: clause is nil"
  in
    match args with
	Cons(List clause, List rest) ->
	  let finished, result = cond_clause clause in
	    if finished then
	      result
	    else
	      lisp_cond rest
      | Cons(_, List _) -> evalfail "lisp_cond: non-list clause"
      | Cons(_, _) -> evalfail "lisp_cond: cond clause is a pair"
      | Nil -> List Nil

    
let lisp_let args =
  match args with
      Cons(List varlist, List body) ->
	letbindings (fun () -> progn body) varlist
    | _ -> evalfail "lisp_let: too few or too many or wrong type arguments"

	    
let lisp_letstar args =
  let bindings = ref [] in
  let iter_varlist varlist =
    iter_cons
      begin
	fun v ->
	  match v with
	      List(Cons(Atom(Symbol name), List(Cons(value, List Nil)))) ->
		bindings := name::!bindings;
		newbind name (eval value)
	    | Atom(Symbol name) ->
		bindings := name::!bindings;
		newbind name (List Nil)
	    | _ -> evalfail "lisp_letstar: invalid form in varlist"
      end
      varlist
  in
    match args with
	Cons(List varlist, List body) ->
	  begin
	    iter_varlist varlist;
	    let result =
	      try
		progn body
	      with some_exception ->
		List.iter unbind !bindings;
		raise some_exception
	    in
	      List.iter unbind !bindings;
	      result
	  end
      | _ -> evalfail "lisp_letstar: too few or too many or wrong type arguments"
	

let lisp_equal args =
  let a, b = two_arguments args in
    generalised_bool (a = b)

(* < *)
let lisp_less args =
  let a, b = two_arguments args in
      match compatible_numbers_from_lispvals a b with
          `Floats(a, b) -> generalised_bool (a < b)
        | `Ints(a, b) -> generalised_bool (a < b)

(* > *)
let lisp_greater args =
  let a, b = two_arguments args in
      match compatible_numbers_from_lispvals a b with
          `Floats(a, b) -> generalised_bool (a > b)
        | `Ints(a, b) -> generalised_bool (a > b)

(* <= *)
let lisp_lesseq args =
  let a, b = two_arguments args in
      match compatible_numbers_from_lispvals a b with
          `Floats(a, b) -> generalised_bool (a <= b)
        | `Ints(a, b) -> generalised_bool (a <= b)

(* >= *)
let lisp_greatereq args =
  let a, b = two_arguments args in
      match compatible_numbers_from_lispvals a b with
          `Floats(a, b) -> generalised_bool (a >= b)
        | `Ints(a, b) -> generalised_bool (a >= b)

(* = (distinct from 'equal') *)
let lisp_numequal args =
  let a, b = two_arguments args in
      match compatible_numbers_from_lispvals a b with
          `Floats(a, b) -> generalised_bool (a = b)
        | `Ints(a, b) -> generalised_bool (a = b)

let lisp_number_to_string args =
  let a = one_argument args in
    match a with
	Atom(Int v) -> Atom(String (string_of_int v))
      | Atom(Float v) -> Atom(String (string_of_float v))
      | _ -> raise Type_mismatch

let lisp_concat args =
  fold_left_cons (fun a b ->
		    match a with
			Atom(String a_value) ->
			  begin
			    match b with
				Atom(String b_value) -> Atom(String (a_value^b_value))
			      | _ -> raise Type_mismatch
			  end
		      | _ -> raise Type_mismatch)
    (Atom(String "")) args

let lisp_symbol_to_string args =
  let a = one_argument args in
    match a with
	Atom(Symbol s) -> Atom(String s)
      | _ -> raise Type_mismatch

let lisp_deparse args =
  let args = one_argument args in
    Atom(String (Deparse.deparse args))

let lisp_mathfun f args =
  let arg = one_argument args in
    match arg with
	Atom(Int v) -> Atom(Float (f (float_of_int v)))
      | Atom(Float v) -> Atom(Float (f v))
      | _ -> raise Type_mismatch

let lisp_abs args =
  let a = one_argument args in
    match a with
	Atom(Float f) -> Atom(Float (if f < 0.0 then -.f else f))
      | Atom(Int i) -> Atom(Int (abs i))
      | _ -> raise Type_mismatch
  
let lisp_pow args =
  let a, b = two_arguments args in
      match compatible_numbers_from_lispvals a b with
	  `Floats(a, b) -> Atom(Float (a ** b))
	| `Ints(a, b) -> Atom(Float ((float_of_int a) ** (float_of_int b)))

let lisp_apply args =
  match args with
      Cons(_, List Nil) -> evalfail "lisp_apply: not enough arguments"
    | Cons(new_car, List cdr) ->
	let new_cdr = map_cons quote (flatten cdr) in
	let x = (List(Cons(new_car, List new_cdr))) in
	  eval x
    | _ -> evalfail "lisp_apply: wrong type argument"

let lisp_mapcar args =
  let func, sequence = two_arguments args in
    match sequence with
	List l ->
	    List (map_cons (fun v -> eval (List(Cons(func, List(Cons(quote v, List Nil)))))) l)
      | _ -> raise Type_mismatch
	  
let lisp_funcall args =
  match args with
      Cons(func, List cdr) ->
        let x = (List(Cons(func, List (map_cons quote cdr)))) in eval x
    | _ -> evalfail "lisp_funcall: wrong type argument"

let lisp_lambda args =
  (* convert current scope's bindings into a let-style form *)
  let currentbindings = ref [] in
    Hashtbl.iter (fun key value ->
		    try
		      ignore (List.assoc key !currentbindings)
		    with Not_found -> currentbindings := (key, value)::!currentbindings) variables;
    let let_list =
      List.fold_right (fun binding cons ->
			 let key, value = binding in
			   Cons(List(Cons(Atom(Symbol key), List(Cons(quote value, List Nil)))), List cons)
		      ) !currentbindings Nil
    in
      match args with
	  Cons(List argnames, List body) ->
	      List(Cons(Atom(magic_closure_symbol), List(Cons(List let_list, List(Cons(List(Cons(Atom(Symbol "lambda"), List(Cons(List argnames , List body)))), List Nil))))))
	| _ -> evalfail "generating closure from malformed lambda expression"

let lisp_function args =
  let a = one_argument args in
    match a with
	Atom(Symbol a) ->
	  begin
	    try
	      let func = Hashtbl.find functions a in
		match func with
		    Lispfun(arglist, body) -> List(Cons(Atom(Symbol "lambda"), List(Cons(List arglist, List body))))
		  | Camlfun _ -> Atom(Symbol a)
		  | Specialform _ -> Atom(Symbol a)
	    with Not_found -> raise (Void_function a)
	  end
      | _ -> raise Type_mismatch
	    

let lisp_unbind_variable args =
  let a = one_argument args in
    match a with
	Atom(Symbol name) -> (unbind name; List Nil)
      | _ -> raise Type_mismatch

let lisp_unbind_function args =
  let a = one_argument args in
    match a with
	Atom(Symbol name) ->
	  begin
	    try
	      match Hashtbl.find functions name with
		  Lispfun(_, _) -> (Hashtbl.remove functions name; List Nil)
		| _ -> evalfail "can't unbind caml function or special form"
	    with Not_found -> evalfail "unbinding function that's not bound"
	  end
      | _ -> raise Type_mismatch

let lisp_boundp args =
  let a = one_argument args in
    match a with
	Atom(Symbol name) -> generalised_bool (Hashtbl.mem variables name)
      | _ -> raise Type_mismatch
	  
let lisp_fboundp args =
  let a = one_argument args in
    match a with
	Atom(Symbol name) -> generalised_bool (Hashtbl.mem functions name)
      | _ -> raise Type_mismatch

let lisp_fbody args =
  let a = one_argument args in
    match a with
	Atom(Symbol name) ->
	  begin
	    try
	      let func = Hashtbl.find functions name in
		match func with
		    Lispfun(arglist, body) -> List body
		  | Specialform _ -> List(Cons(Atom(Symbol "special-form"), List(Cons(Atom(Symbol name), List Nil))))
		  | Camlfun _ -> List(Cons(Atom(Symbol "builtin-function"), List(Cons(Atom(Symbol name), List Nil))))
	    with Not_found -> raise (Void_function name)
	  end
      | _ -> raise Type_mismatch
	  
		  
let rec lisp_or args =
  match args with
      Cons(v, List rest) ->
	let result = eval v in
	  if truep result then
	    result
	  else
	    lisp_or rest
    | Nil -> List Nil
    | Cons(_, Atom _) -> pairfail()

let rec lisp_and args =
  match args with
      Cons(v, List Nil) -> eval v
    | Cons(v, List rest) ->
	let result = eval v in
	  if not (truep result) then
	    result
	  else
	    lisp_and rest
    | Nil -> generalised_bool true
    | Cons(_, Atom _) -> pairfail()

let lisp_random args =
  match args with
      Cons(v, List Nil) ->
	begin
	  match v with
	      Atom(Int i) -> if i > 0 then Atom(Int (Random.int i)) else evalfail "lisp_random: argument must be >0"
	    | _ -> raise Type_mismatch
	end
    | Nil -> Atom(Int (Random.bits()))
    | _ -> raise Argument_mismatch

let lisp_nullp args =
  let x = one_argument args in
    generalised_bool (nullp x)

let lisp_not args =
  let x = one_argument args in
    generalised_bool (not (truep x))

let lisp_length args =
  let x = one_argument args in
    match x with
	List cons ->
	  begin
	    let len = ref 0 in
	      iter_cons (fun v -> incr len) cons;
	      Atom(Int !len)
	  end
      | _ -> raise Type_mismatch

let rec lisp_nth args =
  match args with
      Cons(Atom(Int index), List(Cons(List list, List Nil))) ->
	begin
	  if index < 0 then
	    evalfail "lisp_nth: index less than zero"
	  else ();
	  if index = 0 then
	    car list
	  else
	    lisp_nth (Cons(Atom(Int (index - 1)), List(Cons(cdr list, List Nil))))
	end
    | Cons(_, List(Cons(_, List Nil))) -> raise Type_mismatch
    | _ -> raise Argument_mismatch
	
let lisp_load_library args =
  let pathl = one_argument args in
    match pathl with
	Atom(String path)
      | Atom(Symbol path) ->
	  begin
	    if Sys.file_exists path then
	      (load_library path; pathl)
	    else begin
	      let path = "lib/"^path^"/"^path^".cma" in
		(load_library path; Atom(String path))
	    end
	  end
      | _ -> raise Type_mismatch
