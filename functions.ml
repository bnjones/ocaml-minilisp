open Eval
  
let setup () =
  defun "cons" (Camlfun lisp_cons);
  defun "car" (Camlfun lisp_car);
  defun "cdr" (Camlfun lisp_cdr);
  defun "list" (Camlfun lisp_list);
  defun "list*" (Camlfun lisp_liststar);
  defun "set" (Camlfun lisp_set);
  defun "setq" (Specialform lisp_setq);
  defun "eval" (Camlfun lisp_eval);
  defun "progn" (Specialform progn);
  defun "prog1" (Specialform lisp_prog1);
  defun "prog2" (Specialform lisp_prog2);
  defun "defun" (Specialform lisp_defun);
  defun "lambda" (Specialform lisp_lambda);
  defun "atomp" (Camlfun lisp_atomp);
  defun "symbolp" (Camlfun lisp_symbolp);
  defun "numberp" (Camlfun lisp_numberp);
  defun "consp" (Camlfun lisp_consp);
  defun "listp" (Camlfun lisp_listp);
  defun "stringp" (Camlfun lisp_stringp);
  defun "nullp" (Camlfun lisp_nullp);
  defun "not" (Camlfun lisp_not);
  defun "length" (Camlfun lisp_length);
  defun "nth" (Camlfun lisp_nth);
  defun "+" (Camlfun lisp_add);
  defun "-" (Camlfun lisp_sub);
  defun "*" (Camlfun lisp_mul);
  defun "/" (Camlfun lisp_div);
  defun "write" (Camlfun lisp_print_string);

  defun "cond" (Specialform lisp_cond);
  defun "when" (Specialform lisp_when);
  defun "if" (Specialform lisp_if);

  defun "equal" (Camlfun lisp_equal);
  defun "<" (Camlfun lisp_less);
  defun ">" (Camlfun lisp_greater);
  defun "<=" (Camlfun lisp_lesseq);
  defun ">=" (Camlfun lisp_greatereq);
  defun "=" (Camlfun lisp_numequal);

  defun "sin" (Camlfun (lisp_mathfun sin));
  defun "cos" (Camlfun (lisp_mathfun cos));
  defun "tan" (Camlfun (lisp_mathfun tan));
  defun "exp" (Camlfun (lisp_mathfun exp));
  defun "sqrt" (Camlfun (lisp_mathfun sqrt));
  defun "pow" (Camlfun lisp_pow);
  defun "mod" (Camlfun lisp_mod);
  defun "log" (Camlfun (lisp_mathfun log));
  defun "log10" (Camlfun (lisp_mathfun log10));
  defun "abs" (Camlfun lisp_abs);
  
  defun "number->string" (Camlfun lisp_number_to_string);
  defun "symbol->string" (Camlfun lisp_symbol_to_string);
  defun "object->string" (Camlfun lisp_deparse);
  defun "concat" (Camlfun lisp_concat);

  defun "function" (Specialform lisp_function);
  defun "apply" (Camlfun lisp_apply);
  defun "mapcar" (Camlfun lisp_mapcar);
  defun "funcall" (Camlfun lisp_funcall);

  defun "let" (Specialform lisp_let);
  defun "let*" (Specialform lisp_letstar);
  defun "boundp" (Camlfun lisp_boundp);
  defun "makunbound" (Camlfun lisp_unbind_variable);
  defun "fboundp" (Camlfun lisp_fboundp);
  defun "fmakunbound" (Camlfun lisp_unbind_function);
  defun "fbody" (Camlfun lisp_fbody);
  defun "fdefinition" (Camlfun lisp_function);
  defun "labels" (Specialform lisp_labels);

  defun "and" (Specialform lisp_and);
  defun "or" (Specialform lisp_or);
  
  defun "random" (Camlfun lisp_random)
    
