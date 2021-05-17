fibbenchif:
	cabal v2-run --enable-profiling lisp "(begin (define (fib n) (if (== n 1) 1 (if (== n 0) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 35))" --  +RTS -p
fibbenchcase:
	cabal v2-run --enable-profiling lisp "(begin (define (fib n) (case n ((0 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))) (fib 35))" --  +RTS -p
	
