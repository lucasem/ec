all:
	ocamlopt -p unix.cmxa -thread threads.cmxa -o test utils.ml type.ml expression.ml library.ml enumerate.ml task.ml compress.ml em.ml polynomial.ml
