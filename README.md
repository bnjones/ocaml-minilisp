# Tiny Lisp implementation in OCaml

This is a very minimal Lisp-like language that I wrote about 8 years
ago as an extension language for another project.

I wrote this when I was learning OCaml, so there are some things that
probably look a bit strange. Looking at it again I'd do things
differently if I was doing this today, but hopefully it's interesting
or useful to somebody.

## Building

Install ocamlbuild, then run `ocamlbuild -libs unix,dynlink
main.byte`, or just type `make` (which runs ocamlbuild).

## Running

Run `./main.byte` to get a read-eval-print loop:

```
#> (+ 1 2)
3
#> (defun fib (n)
      (cond
         ((<= n 1) 1)
         (t (+ (fib (- n 1)) (fib (- n 2))))))
fib
#> (mapcar #'fib '(1 2 3 4 5))
(1 2 3 5 8)
```
