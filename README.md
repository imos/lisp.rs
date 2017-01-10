# lisp.rs
Lisp implementation in Rust.

## How to use

```
$ rustc lisp.rs && ./lisp
> (car '(a b c))
a
> (cdr '(a b c))
(b c)
> (cons 1 (cons 2 (cons 3 ())))
(1 2 3)
> (defun fact (n) (if (eq n 0) 1 (* n (fact (- n 1)))))
fact
> (fact 10)
3628800
> (defun fib (n) (if (eq n 1) 1 (if (eq n 0) 1 (+ (fib(- n 1)) (fib(- n 2))))))
fib
> (fib 12)
233
> (defun gen (n) (lambda (m) (setq n (+ n m))))
gen
> (setq x (gen 100))
<expr>
> (x 10)
110
> (x 90)
200
> (x 300)
500
```
