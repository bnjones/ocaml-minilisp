type atom = Symbol of string | Float of float | Int of int
	    | String of string

type lispval = Atom of atom | List of cons
and cons = Cons of lispval * lispval | Nil

exception Impossible of string
exception Type_mismatch
exception Sequence_expected
exception Void_function of string
exception Void_variable of string
exception Eval_error of string
exception Argument_mismatch
exception Setting_constant of string

let __dynlink_init_called = ref false

let load_library path =
  if !__dynlink_init_called then
    Dynlink.loadfile path
  else
    begin
      Dynlink.init();
      Dynlink.allow_unsafe_modules true;
      __dynlink_init_called := true;
      Dynlink.loadfile path
    end
  
let quote v =
  List(Cons(Atom(Symbol "quote"), List(Cons(v, List Nil))))
      
let rec iter_cons f cons =
  match cons with
      Cons(v, List c) -> (f v; iter_cons f c)
    | Cons(v, Atom a) -> failwith "iter_cons: Can't iterate over a pair"
    | Nil -> ()

let rec map_cons f cons =
  match cons with
      (* 'let' used to make f execute in the right order over the
	elements of cons *)
      Cons(v, List c) -> let result = f v in Cons(result, List(map_cons f c))
    | Cons(v, Atom a) -> failwith "map_cons: Can't iterate over a pair"
    | Nil -> Nil

let rec fold_left_cons f (init:lispval) cons =
  match cons with
      Cons(v, List c) ->
	if c = Nil then
	  f init v
	else
	  fold_left_cons f (f init v) c
    | Cons(v, Atom a) -> failwith "fold_left_cons: Can't iterate over a pair"
    | Nil -> init

(* car and cdr of nil are nil *)
let car c =
  match c with
      Cons(car, cdr) -> car
    | Nil -> List Nil

let cdr c =
  match c with
      Cons(car, cdr) -> cdr
    | Nil -> List Nil

let cdrlist c =
  match c with
      Cons(car, List cdr) -> cdr
    | Nil -> Nil
    | _ -> raise Sequence_expected

(* helper for returning a Lisp value from an OCaml boolean
   expression *)
let generalised_bool b =
  if b then
    Atom(Symbol "t")
  else
    List Nil

let atomp v =
  match v with
      Atom _ -> true
    | _ -> false

let symbolp v =
  match v with
      Atom(Symbol _) -> true
    | _ -> false

let numberp v =
  match v with
      Atom(Int _) -> true
    | Atom(Float _) -> true
    | _ -> false

let consp v =
  match v with
      List(Cons(_, _)) -> true
    | _ -> false

let listp v =
  match v with
      List(Cons(_, List _)) -> true
    | _ -> false

let stringp v =
  match v with
      Atom(String _) -> true
    | _ -> false
	
let nullp v =
  match v with
      List Nil -> true
    | _ -> false
	
let truep v = not (nullp v)

let compatible_numbers_from_lispvals a b =
  match a with
      Atom(Int a_value) ->
	begin
	  match b with
	      Atom(Int b_value) -> `Ints(a_value, b_value)
	    | Atom(Float b_value) -> `Floats(float_of_int a_value, b_value)
	    | _ -> raise Type_mismatch
	end
    | Atom(Float a_value) ->
	begin
	  match b with
	      Atom(Int b_value) -> `Floats(a_value, float_of_int b_value)
	    | Atom(Float b_value) -> `Floats(a_value, b_value)
	    | _ -> raise Type_mismatch
	end
    | _ -> raise Type_mismatch

let lisp_list_of_caml_list l =
  List.fold_right (fun a cons ->
		     Cons(a, List cons)
		  ) l Nil

let caml_list_of_lisp_list cons =
  let list = ref [] in
    iter_cons (fun v -> list := v::!list) cons;
    List.rev !list
