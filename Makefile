test: lisp
	./lisp < input.txt | diff - output.txt

lisp: lisp.rs
	rustc lisp.rs
